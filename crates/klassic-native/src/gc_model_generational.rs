//! Bounded logical-graph model. Physical pages contain one object each; this
//! models reference lifetime and handoff, not allocator fragmentation/latency.
use crate::gc_abi::{Access, GcRef, Generation, Phase, PhaseSnapshot, encode_ref};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

type Id = u64;
type Slot = (GcRef, usize);
const ROOTS: usize = 4; // two cells per mutator
const FIELDS: usize = 2;
const OBJECTS: usize = 4;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Page {
    id: Id,
    incarnation: u64,
    generation: Generation,
    fields: [GcRef; FIELDS],
    // Nonmoving metadata address shared by source and unpublished destination.
    // This is independent of the debug identity and virtual page incarnation.
    mark_record: u64,
    forwarding: Option<GcRef>,
    claimed: Option<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Entry {
    owner: GcRef,
    offset: usize,
    incarnation: u64,
    previous: GcRef,
    active: [bool; 2],
    epoch: u64,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Scan {
    generation: Generation,
    object: GcRef,
    cursor: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Move {
    source: GcRef,
    destination: Option<GcRef>,
    promote: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Graph {
    pages: BTreeMap<u64, Page>,
    incarnations: BTreeMap<u64, u64>,
    next_offset: u64,
    next_mark_record: u64,
    marks: BTreeMap<u64, [bool; 2]>,
    roots: [GcRef; ROOTS],
    oracle_roots: [Option<Id>; ROOTS],
    oracle_fields: BTreeMap<Id, [Option<Id>; FIELDS]>,
    live: BTreeSet<Id>,
    snapshot: [BTreeSet<Id>; 2],
    phase: PhaseSnapshot,
    active: [bool; 2],
    queues: [VecDeque<GcRef>; 2],
    scans: [Option<Scan>; 2],
    pending_marks: [Option<(Generation, GcRef)>; 2],
    moves: [Option<Move>; 2],
    buffers: [Vec<Entry>; 2],
    root_satb: [Vec<(Generation, GcRef)>; 2],
    remembered: [BTreeSet<Slot>; 2], // current, previous
    remset_epoch: u64,
    old_keepalive: Vec<GcRef>,
}

impl Graph {
    fn new() -> Self {
        Self {
            pages: BTreeMap::new(),
            incarnations: BTreeMap::new(),
            next_offset: 16,
            next_mark_record: 0x100000,
            marks: BTreeMap::new(),
            roots: [GcRef::NULL; ROOTS],
            oracle_roots: [None; ROOTS],
            oracle_fields: BTreeMap::new(),
            live: BTreeSet::new(),
            snapshot: std::array::from_fn(|_| BTreeSet::new()),
            phase: PhaseSnapshot::initial(),
            active: [false; 2],
            queues: std::array::from_fn(|_| VecDeque::new()),
            scans: [None, None],
            pending_marks: [None, None],
            moves: [None, None],
            buffers: [vec![], vec![]],
            root_satb: [vec![], vec![]],
            remembered: std::array::from_fn(|_| BTreeSet::new()),
            remset_epoch: 0,
            old_keepalive: vec![],
        }
    }

    fn reference(&self, offset: u64) -> GcRef {
        encode_ref(offset, self.phase.color()).unwrap()
    }

    // Resolution uses offsets and forwarding only. Oracle IDs/incarnations do
    // not repair stale references; they diagnose erroneous reuse independently.
    fn resolve(&self, reference: GcRef) -> Result<GcRef, &'static str> {
        if reference == GcRef::NULL {
            return Ok(reference);
        }
        let mut offset = reference.bits() >> 16;
        for _ in 0..=self.pages.len() {
            let page = self.pages.get(&offset).ok_or("reference to retired page")?;
            match page.forwarding {
                Some(to) => offset = to.bits() >> 16,
                None => return Ok(self.reference(offset)),
            }
        }
        Err("forwarding cycle")
    }

    fn id(&self, reference: GcRef) -> Result<Option<Id>, &'static str> {
        let canonical = self.resolve(reference)?;
        Ok(if canonical == GcRef::NULL {
            None
        } else {
            Some(self.pages[&(canonical.bits() >> 16)].id)
        })
    }

    fn alloc_into(&mut self, id: Id, generation: Generation, root: usize, reuse: Option<u64>) {
        assert!(self.live.len() < OBJECTS && !self.oracle_fields.contains_key(&id));
        let offset = reuse.unwrap_or_else(|| {
            let offset = self.next_offset;
            self.next_offset += 16;
            offset
        });
        assert!(
            !self.pages.contains_key(&offset),
            "cannot reuse retained source"
        );
        let incarnation = self.incarnations.entry(offset).or_default();
        *incarnation += 1;
        let mark_record = self.next_mark_record;
        self.next_mark_record += 16;
        self.marks.insert(mark_record, self.active);
        self.pages.insert(
            offset,
            Page {
                id,
                incarnation: *incarnation,
                generation,
                fields: [GcRef::NULL; FIELDS],
                mark_record,
                forwarding: None,
                claimed: None,
            },
        );
        self.live.insert(id);
        self.oracle_fields.insert(id, [None; FIELDS]);
        self.root_store(root, self.reference(offset));
        if self.active[1] && generation == Generation::Young {
            self.old_keepalive.push(self.reference(offset));
        }
    }

    fn root_store(&mut self, root: usize, value: GcRef) {
        let mutator = root / 2;
        for generation in [Generation::Young, Generation::Old] {
            if self.active[generation as usize] {
                self.root_satb[mutator].push((generation, self.roots[root]));
                self.enqueue(generation, value);
            }
        }
        self.oracle_roots[root] = self.id(value).unwrap();
        self.roots[root] = value;
    }

    fn store(&mut self, mutator: usize, owner_root: usize, field: usize, value: GcRef) -> bool {
        let owner = self.resolve(self.roots[owner_root]).unwrap();
        let offset = owner.bits() >> 16;
        let page = &self.pages[&offset];
        if page.claimed.is_some() {
            return false;
        }
        let previous = page.fields[field];
        let id = page.id;
        // The fast path tests the old slot value, never the new target. Null
        // and forgotten double-remember stamps cannot certify bookkeeping.
        if !self.phase.is_good(previous, page.generation, Access::Store) {
            if self.buffers[mutator].len() == 8 {
                return false;
            }
            self.buffers[mutator].push(Entry {
                owner,
                offset: field,
                incarnation: page.incarnation,
                previous,
                active: self.active,
                epoch: self.remset_epoch,
            });
        }
        let value = self.resolve(value).unwrap();
        let value_id = self.id(value).unwrap();
        self.oracle_fields.get_mut(&id).unwrap()[field] = value_id;
        self.pages.get_mut(&offset).unwrap().fields[field] = value;
        true
    }

    fn flush(&mut self, mutator: usize) {
        for entry in std::mem::take(&mut self.buffers[mutator]) {
            let captured = &self.pages[&(entry.owner.bits() >> 16)];
            assert_eq!(
                captured.incarnation, entry.incarnation,
                "metadata lease lost"
            );
            assert_eq!(
                entry.epoch, self.remset_epoch,
                "entry missed remset handoff"
            );
            for generation in [Generation::Young, Generation::Old] {
                if entry.active[generation as usize] {
                    self.enqueue(generation, entry.previous);
                }
            }
            let owner = self.resolve(entry.owner).unwrap();
            let page = &self.pages[&(owner.bits() >> 16)];
            if page.generation == Generation::Old {
                self.remembered[0].insert((owner, entry.offset));
            }
        }
        for (generation, previous) in std::mem::take(&mut self.root_satb[mutator]) {
            self.enqueue(generation, previous);
        }
    }

    fn reachable(&self) -> BTreeSet<Id> {
        let mut found = BTreeSet::new();
        let mut todo: Vec<_> = self.oracle_roots.iter().flatten().copied().collect();
        while let Some(id) = todo.pop() {
            if found.insert(id) {
                todo.extend(self.oracle_fields[&id].into_iter().flatten());
            }
        }
        found
    }

    fn start(&mut self, generation: Generation) -> bool {
        let g = generation as usize;
        if self.active[g]
            || self.buffers.iter().any(|b| !b.is_empty())
            || self.root_satb.iter().any(|b| !b.is_empty())
        {
            return false;
        }
        // This model starts each next mark after the previous cycle has retired.
        if self.phase.phase(generation) != Phase::Idle {
            return false;
        }
        self.snapshot[g] = self
            .reachable()
            .into_iter()
            .filter(|id| {
                generation == Generation::Old
                    || self.pages.values().any(|p| {
                        p.id == *id && p.forwarding.is_none() && p.generation == Generation::Young
                    })
            })
            .collect();
        self.phase = self.phase.transition(generation, Phase::MarkStart).unwrap();
        self.active[g] = true;
        for mark in self.marks.values_mut() {
            mark[g] = false;
        }
        if generation == Generation::Young {
            self.remset_epoch += 1;
            assert!(self.remembered[1].is_empty());
            self.remembered.swap(0, 1);
        } else {
            self.old_keepalive.clear();
        }
        for root in self.roots {
            self.enqueue(generation, root);
        }
        if generation == Generation::Young {
            for keepalive in self.old_keepalive.clone() {
                self.enqueue(generation, keepalive);
            }
        }
        self.phase = self
            .phase
            .transition(generation, Phase::ConcurrentMark)
            .unwrap();
        true
    }

    fn enqueue(&mut self, generation: Generation, value: GcRef) {
        if value == GcRef::NULL {
            return;
        }
        let value = self.resolve(value).unwrap();
        let page = self.pages.get_mut(&(value.bits() >> 16)).unwrap();
        if generation == Generation::Young && page.generation == Generation::Old {
            return;
        }
        let marks = self.marks.get_mut(&page.mark_record).unwrap();
        if !marks[generation as usize] {
            marks[generation as usize] = true;
            self.queues[generation as usize].push_back(value);
            if generation == Generation::Old && page.generation == Generation::Young {
                self.old_keepalive.push(value);
            }
        }
    }

    fn claim_mark(&mut self, worker: usize, generation: Generation, value: GcRef) -> bool {
        if self.pending_marks[worker].is_some() || value == GcRef::NULL {
            return false;
        }
        let value = self.resolve(value).unwrap();
        let page = self.pages.get_mut(&(value.bits() >> 16)).unwrap();
        let marks = self.marks.get_mut(&page.mark_record).unwrap();
        if marks[generation as usize]
            || (generation == Generation::Young && page.generation == Generation::Old)
        {
            return false;
        }
        marks[generation as usize] = true;
        if generation == Generation::Old && page.generation == Generation::Young {
            self.old_keepalive.push(value);
        }
        // Mark-bit ownership and queue publication are intentionally separate.
        self.pending_marks[worker] = Some((generation, value));
        true
    }

    fn publish_mark(&mut self, worker: usize) {
        let (generation, value) = self.pending_marks[worker].take().unwrap();
        self.queues[generation as usize].push_back(value);
    }

    fn gc_heal(&mut self, value: GcRef) -> GcRef {
        if value == GcRef::NULL {
            return value;
        }
        for generation in [Generation::Young, Generation::Old] {
            if self.active[generation as usize] {
                self.enqueue(generation, value);
            }
        }
        let canonical = self.resolve(value).unwrap();
        encode_ref(canonical.bits() >> 16, self.phase.color_unremembered()).unwrap()
    }

    fn scan_remembered(&mut self) -> bool {
        let Some((owner, field)) = self.remembered[1].pop_first() else {
            return false;
        };
        let owner = self.resolve(owner).unwrap();
        let value = self.pages[&(owner.bits() >> 16)].fields[field];
        let mut healed = self.gc_heal(value);
        if value != GcRef::NULL {
            let value = self.resolve(value).unwrap();
            if self.pages[&(value.bits() >> 16)].generation == Generation::Young {
                self.remembered[0].insert((owner, field));
                // Only recording the current bitmap and completing marking
                // authorizes a single remembered bit in a collector heal.
                healed = value;
            }
        }
        self.pages.get_mut(&(owner.bits() >> 16)).unwrap().fields[field] = healed;
        true
    }

    fn begin_scan(&mut self, worker: usize, generation: Generation) -> bool {
        if self.scans[worker].is_some() {
            return false;
        }
        let Some(object) = self.queues[generation as usize].pop_front() else {
            return false;
        };
        self.scans[worker] = Some(Scan {
            generation,
            object,
            cursor: 0,
        });
        true
    }

    fn scan_one(&mut self, worker: usize) -> bool {
        let Some(mut scan) = self.scans[worker].take() else {
            return false;
        };
        let owner = self.resolve(scan.object).unwrap();
        let offset = owner.bits() >> 16;
        if self.pages[&offset].claimed.is_some() {
            self.scans[worker] = Some(scan);
            return false;
        }
        let value = self.pages[&offset].fields[scan.cursor];
        self.enqueue(scan.generation, value);
        let healed = self.gc_heal(value);
        self.pages.get_mut(&offset).unwrap().fields[scan.cursor] = healed;
        scan.object = owner;
        scan.cursor += 1;
        if scan.cursor < FIELDS {
            self.scans[worker] = Some(scan);
        }
        true
    }

    fn end_mark(&mut self, generation: Generation) -> bool {
        let g = generation as usize;
        if !self.active[g]
            || self
                .pending_marks
                .iter()
                .flatten()
                .any(|(generation, _)| *generation as usize == g)
            || !self.queues[g].is_empty()
            || self
                .scans
                .iter()
                .flatten()
                .any(|s| s.generation == generation)
            || self.buffers.iter().any(|b| !b.is_empty())
            || self.root_satb.iter().any(|b| !b.is_empty())
            || (generation == Generation::Young && !self.remembered[1].is_empty())
        {
            return false;
        }
        self.phase = self
            .phase
            .transition(generation, Phase::MarkEndCheck)
            .unwrap();
        self.phase = self.phase.transition(generation, Phase::Select).unwrap();
        self.active[g] = false;
        true
    }

    fn drain(&mut self, generation: Generation) {
        for mutator in 0..2 {
            self.flush(mutator);
        }
        while generation == Generation::Young && self.scan_remembered() {}
        loop {
            let mut progress = false;
            for worker in 0..2 {
                if self.scans[worker].is_none() {
                    progress |= self.begin_scan(worker, generation);
                }
                if self.scans[worker]
                    .as_ref()
                    .is_some_and(|s| s.generation == generation)
                {
                    progress |= self.scan_one(worker);
                }
            }
            if !progress {
                break;
            }
        }
        assert!(
            self.end_mark(generation),
            "termination has outstanding producers/scanners"
        );
    }

    fn move_claim(&mut self, worker: usize, root: usize, promote: bool) -> bool {
        if self.moves[worker].is_some() {
            return false;
        }
        let source = self.resolve(self.roots[root]).unwrap();
        let page = self.pages.get_mut(&(source.bits() >> 16)).unwrap();
        if page.claimed.is_some() {
            return false;
        }
        page.claimed = Some(worker);
        self.moves[worker] = Some(Move {
            source,
            destination: None,
            promote,
        });
        true
    }

    fn move_copy(&mut self, worker: usize) {
        let job = self.moves[worker].as_ref().unwrap();
        assert!(job.destination.is_none());
        let mut copy = self.pages[&(job.source.bits() >> 16)].clone();
        copy.claimed = None;
        if job.promote {
            copy.generation = Generation::Old;
        }
        let offset = self.next_offset;
        self.next_offset += 16;
        self.incarnations.insert(offset, 1);
        copy.incarnation = 1;
        self.pages.insert(offset, copy);
        self.moves[worker].as_mut().unwrap().destination = Some(self.reference(offset));
    }

    fn move_publish(&mut self, worker: usize) {
        let job = self.moves[worker].take().unwrap();
        let destination = job.destination.expect("no publication before copy");
        let source = self.pages.get_mut(&(job.source.bits() >> 16)).unwrap();
        assert_eq!(source.claimed, Some(worker));
        source.forwarding = Some(destination);
        source.claimed = None;
        let is_old = self.pages[&(destination.bits() >> 16)].generation == Generation::Old;
        // Moving an Old owner preserves both bitmaps; promotion records all
        // fields, including null slots whose future store cannot fast-path.
        for index in 0..2 {
            let slots = std::mem::take(&mut self.remembered[index]);
            self.remembered[index] = slots
                .into_iter()
                .map(|(owner, slot)| (self.resolve(owner).unwrap(), slot))
                .collect();
        }
        if job.promote && is_old {
            for field in 0..FIELDS {
                self.remembered[0].insert((destination, field));
            }
        }
    }

    // Reference locations are remapped one by one in runtime; this convenience
    // driver performs those steps to quiescence for the small model. It must
    // never be transplanted into a global stopped interval.
    fn remap_storage(&mut self) {
        for root in 0..ROOTS {
            self.roots[root] = self.resolve(self.roots[root]).unwrap();
        }
        let offsets: Vec<_> = self
            .pages
            .iter()
            .filter(|(_, p)| p.forwarding.is_none())
            .map(|(o, _)| *o)
            .collect();
        for offset in offsets {
            for field in 0..FIELDS {
                let value = self.pages[&offset].fields[field];
                let healed = self.gc_heal(value);
                self.pages.get_mut(&offset).unwrap().fields[field] = healed;
            }
        }
        for generation in 0..2 {
            self.queues[generation] = self.queues[generation]
                .iter()
                .map(|r| self.resolve(*r).unwrap())
                .collect();
        }
        for worker in 0..2 {
            if let Some(scan) = &self.scans[worker] {
                let healed = self.resolve(scan.object).unwrap();
                self.scans[worker].as_mut().unwrap().object = healed;
            }
        }
        for worker in 0..2 {
            if let Some((generation, value)) = self.pending_marks[worker] {
                self.pending_marks[worker] = Some((generation, self.resolve(value).unwrap()));
            }
        }
        self.old_keepalive = self
            .old_keepalive
            .iter()
            .map(|r| self.resolve(*r).unwrap())
            .collect();
        for bitmap in 0..2 {
            self.remembered[bitmap] = self.remembered[bitmap]
                .iter()
                .map(|(r, f)| (self.resolve(*r).unwrap(), *f))
                .collect();
        }
        // Buffers deliberately retain original owners/incarnations and leases
        // until flush; remapping them without moving the lease is incorrect.
    }

    fn references(&self) -> Vec<GcRef> {
        let mut refs = self.roots.to_vec();
        for page in self.pages.values() {
            if let Some(to) = page.forwarding {
                refs.push(to);
            } else {
                refs.extend(page.fields);
            }
        }
        for queue in &self.queues {
            refs.extend(queue);
        }
        for scan in self.scans.iter().flatten() {
            refs.push(scan.object);
        }
        for job in self.moves.iter().flatten() {
            refs.push(job.source);
            refs.extend(job.destination);
        }
        for buffer in &self.buffers {
            for entry in buffer {
                refs.extend([entry.owner, entry.previous]);
            }
        }
        for satb in &self.root_satb {
            refs.extend(satb.iter().map(|(_, r)| r));
        }
        for bitmap in &self.remembered {
            refs.extend(bitmap.iter().map(|(r, _)| r));
        }
        refs.extend(self.pending_marks.iter().flatten().map(|(_, value)| value));
        refs.extend(&self.old_keepalive);
        refs
    }

    fn retire_source(&mut self, offset: u64) -> bool {
        if self.pages[&offset].forwarding.is_none()
            || self.references().iter().any(|r| r.bits() >> 16 == offset)
        {
            return false;
        }
        self.pages.remove(&offset);
        true
    }

    fn sweep(&mut self, generation: Generation) {
        assert!(!self.active[generation as usize]);
        let dead: BTreeSet<_> = self
            .pages
            .values()
            .filter(|p| {
                p.forwarding.is_none()
                    && (generation == Generation::Old || p.generation == Generation::Young)
                    && !self.marks[&p.mark_record][generation as usize]
            })
            .map(|p| p.id)
            .collect();
        for id in &dead {
            self.live.remove(id);
        }
        self.pages.retain(|_, p| !dead.contains(&p.id));
        let used_marks: BTreeSet<_> = self.pages.values().map(|p| p.mark_record).collect();
        self.marks.retain(|record, _| used_marks.contains(record));
        for bitmap in &mut self.remembered {
            bitmap.retain(|(r, _)| self.pages.contains_key(&(r.bits() >> 16)));
        }
        if generation == Generation::Old {
            self.old_keepalive.clear();
        }
    }

    fn finish_cycle(&mut self, generation: Generation) {
        // Conservative color reuse gate: prior forwarding sources must have
        // retired. A cycle counter alone cannot authorize another parity flip.
        assert!(self.pages.values().all(|p| p.forwarding.is_none()));
        assert!(self.moves.iter().all(Option::is_none));
        for phase in [
            Phase::RelocateStart,
            Phase::ConcurrentRelocate,
            Phase::Retire,
            Phase::Idle,
        ] {
            self.phase = self.phase.transition(generation, phase).unwrap();
        }
        self.snapshot[generation as usize].clear();
    }

    fn verify(&self) -> Result<(), String> {
        for root in 0..ROOTS {
            if self.id(self.roots[root]).map_err(str::to_owned)? != self.oracle_roots[root] {
                return Err("root identity changed".into());
            }
        }
        for id in self.reachable() {
            if !self.live.contains(&id) {
                return Err(format!("reachable object {id} reclaimed"));
            }
        }
        for generation in 0..2 {
            for id in &self.snapshot[generation] {
                if !self.live.contains(id) {
                    return Err(format!("snapshot object {id} reclaimed"));
                }
            }
        }
        for id in &self.live {
            let canonical: Vec<_> = self
                .pages
                .iter()
                .filter(|(offset, p)| {
                    p.id == *id
                        && p.forwarding.is_none()
                        && !self
                            .moves
                            .iter()
                            .flatten()
                            .any(|m| m.destination.is_some_and(|r| r.bits() >> 16 == **offset))
                })
                .collect();
            if canonical.len() != 1 {
                return Err(format!(
                    "object {id} has {} canonical copies",
                    canonical.len()
                ));
            }
            let page = canonical[0].1;
            for field in 0..FIELDS {
                if self.id(page.fields[field]).map_err(str::to_owned)?
                    != self.oracle_fields[id][field]
                {
                    return Err("completed store lost".into());
                }
            }
        }
        Ok(())
    }
}

fn seed() -> Graph {
    let mut graph = Graph::new();
    graph.alloc_into(1, Generation::Old, 0, None);
    graph.alloc_into(2, Generation::Young, 2, None);
    graph.alloc_into(3, Generation::Young, 3, None);
    graph.alloc_into(4, Generation::Old, 1, None);
    assert!(graph.store(0, 0, 0, graph.roots[2]));
    assert!(graph.store(1, 2, 0, graph.roots[3]));
    graph.flush(0);
    graph.flush(1);
    graph.verify().unwrap();
    graph
}

#[test]
fn gc_model_old_queue_survives_young_move_and_promotion() {
    let mut g = seed();
    assert!(g.start(Generation::Old));
    // One worker owns an Old-mark scan of a Young object. The other worker
    // moves/promotes it. The first scanner retains its source GcRef meanwhile.
    while g.queues[1]
        .front()
        .is_none_or(|r| g.id(*r).unwrap() != Some(2))
    {
        assert!(g.begin_scan(0, Generation::Old));
        assert!(g.scan_one(0));
        assert!(g.scan_one(0));
    }
    assert!(g.begin_scan(0, Generation::Old));
    let source = g.roots[2].bits() >> 16;
    assert!(g.start(Generation::Young));
    assert!(g.move_claim(1, 2, true));
    assert!(
        !g.store(0, 2, 1, g.roots[3]),
        "source cannot remain writable during copy"
    );
    g.move_copy(1);
    g.verify().unwrap();
    g.move_publish(1);
    g.verify().unwrap();
    assert!(
        !g.retire_source(source),
        "Old worker still holds original source"
    );
    // A Young cycle may finish while the Old job remains in flight.
    g.drain(Generation::Young);
    g.sweep(Generation::Young);
    g.verify().unwrap();
    g.remap_storage();
    assert!(g.retire_source(source));
    g.finish_cycle(Generation::Young);
    assert!(g.active[1], "Young collection must not discard Old marking");
    assert!(g.scan_one(0));
    assert!(g.scan_one(0));
    g.drain(Generation::Old);
    g.sweep(Generation::Old);
    g.verify().unwrap();
    g.finish_cycle(Generation::Old);
}

#[test]
fn gc_model_buffer_owns_enqueued_value_across_root_reuse_and_owner_move() {
    let mut g = seed();
    assert!(g.store(0, 0, 1, g.roots[3]));
    let original = g.roots[0];
    let original_offset = original.bits() >> 16;
    // Keep original owner in another cell, then overwrite the caller's cell.
    g.root_store(1, original);
    g.root_store(0, g.roots[2]);
    assert!(g.move_claim(1, 1, false));
    g.move_copy(1);
    g.move_publish(1);
    g.remap_storage();
    assert!(
        !g.retire_source(original_offset),
        "buffer metadata lease is still live"
    );
    assert!(
        !g.start(Generation::Young),
        "flip must wait for old-epoch buffer"
    );
    g.flush(0);
    let canonical = g.resolve(original).unwrap();
    assert!(g.remembered[0].contains(&(canonical, 1)));
    assert!(g.retire_source(original_offset));
    g.root_store(3, GcRef::NULL);
    assert!(g.start(Generation::Young));
    g.drain(Generation::Young);
    g.sweep(Generation::Young);
    g.verify().unwrap();
    assert!(g.live.contains(&3));
}

#[test]
fn gc_model_null_clear_flip_and_repeated_minor_keep_the_only_edge() {
    let mut g = seed();
    assert!(g.store(0, 0, 0, GcRef::NULL));
    g.flush(0);
    assert!(g.start(Generation::Young));
    g.drain(Generation::Young);
    g.sweep(Generation::Young);
    g.verify().unwrap();
    g.finish_cycle(Generation::Young);
    // Null does not prove remembered membership, even after a flip.
    assert!(g.store(0, 0, 0, g.roots[2]));
    g.flush(0);
    g.root_store(2, GcRef::NULL);
    g.root_store(3, GcRef::NULL);
    let mut colors = BTreeSet::new();
    for _ in 0..6 {
        colors.insert(g.phase.color());
        assert!(g.start(Generation::Young));
        g.drain(Generation::Young);
        g.sweep(Generation::Young);
        g.verify().unwrap();
        assert!(g.live.contains(&2) && g.live.contains(&3));
        g.finish_cycle(Generation::Young);
    }
    assert_eq!(colors.len(), 2, "parities wrap repeatedly, not just once");
}

#[test]
fn gc_model_termination_waits_for_private_satb_and_in_flight_scan() {
    let mut g = seed();
    assert!(g.start(Generation::Old));
    assert!(g.begin_scan(0, Generation::Old));
    assert!(!g.end_mark(Generation::Old));
    assert!(g.store(0, 0, 0, GcRef::NULL));
    g.root_store(2, GcRef::NULL);
    g.root_store(3, GcRef::NULL);
    assert!(!g.end_mark(Generation::Old));
    g.flush(0);
    g.flush(1);
    g.drain(Generation::Old);
    g.sweep(Generation::Old);
    // Targets removed after the snapshot remain alive through SATB.
    g.verify().unwrap();
    assert!(g.live.contains(&2) && g.live.contains(&3));
}

#[test]
fn gc_model_incoming_forwarding_requires_source_retirement_order() {
    let mut g = seed();
    let first = g.roots[2].bits() >> 16;
    assert!(g.move_claim(0, 2, false));
    g.move_copy(0);
    g.move_publish(0);
    let middle = g.resolve(g.roots[2]).unwrap().bits() >> 16;
    assert!(g.move_claim(1, 2, false));
    g.move_copy(1);
    g.move_publish(1);
    g.remap_storage();
    assert!(
        !g.retire_source(middle),
        "older forwarding still targets this range"
    );
    assert!(g.retire_source(first));
    assert!(g.retire_source(middle));
    assert_eq!(g.id(g.roots[2]).unwrap(), Some(2));
    g.verify().unwrap();
}

#[test]
fn gc_model_delayed_reference_prevents_address_reuse() {
    let mut g = seed();
    let old = g.roots[2];
    let offset = old.bits() >> 16;
    assert!(g.move_claim(0, 2, false));
    g.move_copy(0);
    g.move_publish(0);
    assert!(!g.retire_source(offset));
    g.remap_storage();
    assert!(g.retire_source(offset));
    // Reclaim a now-unrooted Old object to make room for a new identity.
    g.root_store(1, GcRef::NULL);
    assert!(g.start(Generation::Old));
    g.drain(Generation::Old);
    g.sweep(Generation::Old);
    g.verify().unwrap();
    g.finish_cycle(Generation::Old);
    g.alloc_into(5, Generation::Young, 1, Some(offset));
    assert_eq!(g.incarnations[&offset], 2);
    assert_eq!(g.id(g.roots[2]).unwrap(), Some(2));
    assert_eq!(g.id(g.roots[1]).unwrap(), Some(5));
    g.verify().unwrap();
    // Deliberately retaining this unregistered raw value demonstrates why the
    // gate must cover every holder: incarnation in metadata cannot rescue it.
    assert_eq!(g.id(old).unwrap(), Some(5));
}

#[test]
fn gc_model_missing_remset_and_early_reuse_have_counterexamples() {
    let mut g = seed();
    g.root_store(2, GcRef::NULL);
    g.root_store(3, GcRef::NULL);
    g.remembered[0].clear(); // injected lost handoff
    assert!(g.start(Generation::Young));
    g.drain(Generation::Young);
    g.sweep(Generation::Young);
    assert!(
        g.verify().is_err(),
        "oracle must notice reachable Young object reclaimed"
    );

    let mut g = seed();
    let offset = g.roots[2].bits() >> 16;
    assert!(g.move_claim(0, 2, false));
    g.move_copy(0);
    g.move_publish(0);
    // Bypass retirement guard and replace address contents. Correct metadata
    // incarnation alone cannot distinguish stale roots from this new object.
    let mut replacement = g.pages[&offset].clone();
    replacement.id = 99;
    replacement.incarnation += 1;
    replacement.forwarding = None;
    g.pages.insert(offset, replacement);
    assert!(
        g.verify().is_err(),
        "oracle must catch stale root changing identity"
    );
}

#[test]
fn gc_model_explores_store_flip_mark_scan_and_promotion_schedules() {
    use std::collections::HashSet;
    let mut initial = seed();
    assert!(initial.start(Generation::Old));
    let mut stack = vec![(initial, [0u8; 4])];
    let mut seen = HashSet::new();
    let mut terminals = 0;
    let mut overlapping = 0;
    while let Some((g, pc)) = stack.pop() {
        if !seen.insert((g.clone(), pc)) {
            continue;
        }
        // Exceeding the exploration budget is a failure, not a passing truncation.
        assert!(
            seen.len() <= 250_000,
            "increase or reduce the model before claiming complete exploration"
        );
        g.verify().unwrap();
        if g.active == [true, true] {
            overlapping += 1;
        }
        if pc == [2, 2, 13, 5] {
            let mut completed = g;
            completed.sweep(Generation::Young);
            completed.verify().unwrap();
            completed.remap_storage();
            assert!(completed.retire_source(32));
            completed.finish_cycle(Generation::Young);
            completed.sweep(Generation::Old);
            completed.verify().unwrap();
            completed.finish_cycle(Generation::Old);
            terminals += 1;
            continue;
        }
        let mut enabled = 0;
        for actor in 0..4 {
            let mut next = g.clone();
            let progress = match (actor, pc[actor]) {
                (0, 0) => next.store(0, 0, 1, next.roots[3]),
                (0, 1) => {
                    next.flush(0);
                    true
                }
                (1, 0) => next.store(1, 2, 1, next.roots[0]),
                (1, 1) => {
                    next.flush(1);
                    true
                }
                (2, index @ 0..12) => {
                    if index % 3 == 0 {
                        next.begin_scan(0, Generation::Old)
                    } else {
                        next.scan_one(0)
                    }
                }
                (2, 12) => next.end_mark(Generation::Old),
                (3, 0) => next.start(Generation::Young),
                (3, 1) => next.move_claim(1, 2, true),
                (3, 2) => {
                    next.move_copy(1);
                    true
                }
                (3, 3) => {
                    next.move_publish(1);
                    true
                }
                (3, 4) if pc[0] == 2 && pc[1] == 2 => {
                    // All producer buffers were explicitly flushed. Finish the
                    // Young scanner while allowing an Old job to remain active.
                    next.drain(Generation::Young);
                    true
                }
                _ => false,
            };
            if progress {
                let mut next_pc = pc;
                next_pc[actor] += 1;
                stack.push((next, next_pc));
                enabled += 1;
            }
        }
        assert_ne!(enabled, 0, "deadlock at {pc:?}");
    }
    assert!(terminals > 0 && overlapping > 0);
    eprintln!(
        "generational schedules: {} states, {terminals} terminals, {overlapping} overlap states",
        seen.len()
    );
}

#[test]
fn gc_model_queue_empty_does_not_hide_mark_bit_owner() {
    let mut g = seed();
    let child = g.roots[2];
    g.root_store(2, GcRef::NULL);
    g.root_store(3, GcRef::NULL);
    assert!(g.start(Generation::Old));
    assert!(g.claim_mark(1, Generation::Old, child));
    // Trace roots while the other worker has acquired the child's mark bit
    // but has not published its work item. A second visitor observes marked.
    while g.begin_scan(0, Generation::Old) {
        assert!(g.scan_one(0));
        assert!(g.scan_one(0));
    }
    assert!(g.queues[1].is_empty() && g.scans.iter().all(Option::is_none));
    assert!(!g.end_mark(Generation::Old));
    let mut broken = g.clone();
    broken.pending_marks[1] = None; // injected missing in-flight accounting
    assert!(broken.end_mark(Generation::Old));
    broken.sweep(Generation::Old);
    assert!(
        broken.verify().is_err(),
        "the unscanned child's last descendant is lost"
    );
    g.publish_mark(1);
    g.drain(Generation::Old);
    g.sweep(Generation::Old);
    g.verify().unwrap();
}

#[test]
fn gc_model_dead_owner_does_not_retain_remembered_metadata() {
    let mut g = seed();
    g.root_store(0, GcRef::NULL);
    g.root_store(1, GcRef::NULL);
    assert!(g.start(Generation::Old));
    g.drain(Generation::Old);
    g.sweep(Generation::Old);
    g.verify().unwrap();
    assert_eq!(g.pages.len(), 2);
    assert!(g.remembered.iter().all(BTreeSet::is_empty));
    g.finish_cycle(Generation::Old);
    for id in 5..10 {
        g.alloc_into(id, Generation::Old, 0, Some(16));
        assert!(g.store(0, 0, 0, g.roots[2]));
        g.flush(0);
        g.root_store(0, GcRef::NULL);
        assert!(g.start(Generation::Old));
        g.drain(Generation::Old);
        g.sweep(Generation::Old);
        g.verify().unwrap();
        g.finish_cycle(Generation::Old);
        assert_eq!(g.pages.len(), 2);
        assert_eq!(g.marks.len(), 2);
        assert!(g.remembered.iter().all(BTreeSet::is_empty));
    }
}

#[test]
fn gc_model_old_mark_between_young_copy_and_publication_is_not_lost() {
    let mut g = seed();
    assert!(g.start(Generation::Young));
    g.drain(Generation::Young);
    assert!(g.move_claim(0, 2, false));
    g.move_copy(0);
    // Old begins while a Young copy is private. Updating only the source's
    // mark bit here would leave the destination white after publication.
    assert!(g.start(Generation::Old));
    assert!(g.begin_scan(1, Generation::Old));
    assert!(g.scan_one(1)); // only incoming edge observes the marked source
    assert!(g.scan_one(1));
    g.move_publish(0);
    g.drain(Generation::Old);
    g.sweep(Generation::Old);
    g.verify().unwrap();
}

#[test]
fn gc_model_forgetting_old_to_old_slot_never_recovers_a_store_good_parity() {
    let mut g = seed();
    assert!(g.store(0, 0, 0, g.roots[1])); // Old -> Old, forgettable after flip
    g.flush(0);
    for _ in 0..2 {
        assert!(g.start(Generation::Young));
        g.drain(Generation::Young);
        g.sweep(Generation::Young);
        g.verify().unwrap();
        g.finish_cycle(Generation::Young);
    }
    assert!(g.remembered[0].is_empty());
    let old = g.pages[&16].fields[0];
    assert!(
        !g.phase.is_good(old, Generation::Old, Access::Store),
        "forgotten parity wrapped into a false fast path"
    );
    assert!(g.store(0, 0, 0, g.roots[2])); // sole new Old -> Young edge
    g.flush(0);
    g.root_store(2, GcRef::NULL);
    g.root_store(3, GcRef::NULL);
    assert!(g.start(Generation::Young));
    g.drain(Generation::Young);
    g.sweep(Generation::Young);
    g.verify().unwrap();
}
