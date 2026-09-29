//! Deterministic model of the canonical-access/relocation critical section.
//!
//! This is the first slice of the full generational state model, not a model of
//! completed marking or reclamation. Two mutators contend on an owner's slot;
//! two workers contend on forwarding. Loads/heals, stores, claim/copy/publication
//! are separate scheduler steps. No timing assumptions or runtime threads.

#[cfg(test)]
mod tests {
    use crate::gc_abi::{GcRef, Generation, ObjectId, PhaseSnapshot, encode_ref};
    use std::collections::HashSet;

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    enum Fault {
        None,
        UnconditionalHeal,
        CopyWithActiveAccess,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    struct Model {
        generation: Generation,
        pc: [u8; 4],
        // A physical source and destination represent ONE logical object.
        source_id: ObjectId,
        destination_id: Option<ObjectId>,
        source: GcRef,
        destination: Option<GcRef>,
        claimed: Option<usize>,
        published: bool,
        guards: [bool; 2],
        at_destination: [bool; 2],
        observed: Option<GcRef>,
        // Independent language oracle: latest completed store's logical target.
        expected_target: u64,
    }

    fn target(offset: u64, fresh: bool) -> GcRef {
        // The fixture flips Young remap parity to require healing without moving leaves.
        let snapshot = PhaseSnapshot::new(
            crate::gc_abi::Phase::Idle,
            crate::gc_abi::Phase::Idle,
            if fresh { 4 } else { 0 },
            0,
        )
        .unwrap();
        encode_ref(offset, snapshot.color()).unwrap()
    }

    impl Model {
        fn new(generation: Generation) -> Self {
            Self {
                generation,
                pc: [0; 4],
                source_id: ObjectId(1),
                destination_id: None,
                source: target(32, false),
                destination: None,
                claimed: None,
                published: false,
                guards: [false; 2],
                at_destination: [false; 2],
                observed: None,
                expected_target: 32,
            }
        }

        fn slot(&mut self, actor: usize) -> &mut GcRef {
            if self.at_destination[actor] {
                self.destination.as_mut().expect("published destination")
            } else {
                &mut self.source
            }
        }

        fn acquire(&mut self, actor: usize) -> bool {
            if self.claimed.is_some() && !self.published {
                return false;
            }
            self.guards[actor] = true;
            self.at_destination[actor] = self.published;
            true
        }

        // Return false only when this actor must wait for another enabled step.
        fn step(&mut self, actor: usize, fault: Fault) -> bool {
            match (actor, self.pc[actor]) {
                (0, 0) => {
                    if !self.acquire(actor) {
                        return false;
                    }
                    self.observed = Some(*self.slot(actor));
                }
                (0, 1) => {
                    let observed = self.observed.take().unwrap();
                    let slot = self.slot(actor);
                    // CAS failure must preserve another mutator's completed store.
                    if fault == Fault::UnconditionalHeal || *slot == observed {
                        *slot = target(observed.bits() >> 16, true);
                    }
                    self.guards[actor] = false;
                }
                (1, 0) => {
                    if !self.acquire(actor) {
                        return false;
                    }
                }
                (1, 1) => {
                    *self.slot(actor) = target(48, true);
                    self.expected_target = 48;
                }
                (1, 2) => {
                    self.guards[actor] = false;
                }
                (2 | 3, 0) => {
                    if self.claimed.is_none() {
                        if fault != Fault::CopyWithActiveAccess && self.guards.iter().any(|g| *g) {
                            return false;
                        }
                        self.claimed = Some(actor);
                    }
                }
                (2 | 3, 1) => {
                    if self.claimed == Some(actor) {
                        self.destination = Some(self.source);
                        self.destination_id = Some(self.source_id);
                    } else if !self.published {
                        return false;
                    }
                }
                (2 | 3, 2) => {
                    if self.claimed == Some(actor) {
                        assert!(
                            self.destination.is_some(),
                            "copy before release publication"
                        );
                        self.published = true;
                    }
                }
                _ => return false,
            }
            self.pc[actor] += 1;
            true
        }

        fn invariant(&self) -> bool {
            let canonical = if self.published {
                self.destination.unwrap()
            } else {
                self.source
            };
            // Debug identity is only an assertion, never used to resolve access.
            canonical.bits() >> 16 == self.expected_target
                && self.destination_id.is_none_or(|id| id == self.source_id)
        }

        fn complete(&self) -> bool {
            self.pc == [2, 3, 3, 3]
        }
    }

    struct Exploration {
        states: usize,
        terminals: usize,
        counterexample: Option<Vec<usize>>,
    }

    fn explore(generation: Generation, fault: Fault) -> Exploration {
        let mut seen = HashSet::new();
        let mut stack = vec![(Model::new(generation), Vec::new())];
        let mut terminals = 0;
        while let Some((state, schedule)) = stack.pop() {
            if !seen.insert(state.clone()) {
                continue;
            }
            if !state.invariant() {
                return Exploration {
                    states: seen.len(),
                    terminals,
                    counterexample: Some(schedule),
                };
            }
            if state.complete() {
                terminals += 1;
                continue;
            }
            let mut enabled = 0;
            for actor in 0..4 {
                let mut successor = state.clone();
                if successor.step(actor, fault) {
                    enabled += 1;
                    let mut trace = schedule.clone();
                    trace.push(actor);
                    stack.push((successor, trace));
                }
            }
            assert_ne!(enabled, 0, "deadlock at {schedule:?}: {state:?}");
        }
        Exploration {
            states: seen.len(),
            terminals,
            counterexample: None,
        }
    }

    #[test]
    fn gc_model_two_mutators_two_relocators_preserve_stores_and_identity() {
        for generation in [Generation::Young, Generation::Old] {
            let result = explore(generation, Fault::None);
            assert!(result.counterexample.is_none());
            assert!(result.terminals > 0);
            assert!(result.states > 100);
            eprintln!(
                "{generation:?}: {} distinct states, {} terminal states",
                result.states, result.terminals
            );
        }
    }

    #[test]
    fn gc_model_finds_unconditional_heal_counterexample() {
        let result = explore(Generation::Old, Fault::UnconditionalHeal);
        assert!(
            result.counterexample.is_some(),
            "oracle must detect a lost store"
        );
        eprintln!(
            "unconditional heal counterexample: {:?}",
            result.counterexample.unwrap()
        );
    }

    #[test]
    fn gc_model_finds_copy_without_access_guard_counterexample() {
        let result = explore(Generation::Young, Fault::CopyWithActiveAccess);
        assert!(
            result.counterexample.is_some(),
            "copying a writable source loses stores"
        );
        eprintln!(
            "unguarded copy counterexample: {:?}",
            result.counterexample.unwrap()
        );
    }
}

#[cfg(test)]
#[path = "gc_model_generational.rs"]
mod generational;
