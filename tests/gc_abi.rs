//! The generational ABI is tested independently of the legacy collector.
use klassic_native::gc_abi::*;

#[test]
fn null_and_invalid_references_are_rejected_before_address_formation() {
    assert_eq!(decode_ref(GcRef::NULL, usize::MAX), Ok(0));
    let snapshot = PhaseSnapshot::initial();
    let color = snapshot.color();
    assert_eq!(encode_ref(0, color), Err(GcError::InvalidReference));
    assert_eq!(encode_ref(17, color), Err(GcError::InvalidReference));
    assert_eq!(encode_ref(1 << 48, color), Err(GcError::InvalidReference));
    for bit in [0, 1, 2, 3, 6, 7] {
        assert_eq!(
            encode_ref(16, color | (1 << bit)),
            Err(GcError::InvalidReference)
        );
    }
    assert_eq!(
        encode_ref(16, color & !REMAP_MASK),
        Err(GcError::InvalidReference)
    );
    assert_eq!(
        encode_ref(16, color | REMAP_MASK),
        Err(GcError::InvalidReference)
    );
    let reference = encode_ref(16, color).unwrap();
    assert_eq!(
        decode_ref(reference, usize::MAX - 15),
        Err(GcError::InvalidReference)
    );
    assert_eq!(decode_ref(reference, 0x1000), Ok(0x1010));
    assert_eq!(decode_ref(reference, 1), Err(GcError::InvalidReference));
}

#[test]
fn all_epoch_combinations_have_unique_colors_and_exact_masks() {
    let mut colors = std::collections::BTreeSet::new();
    for epochs in 0..32 {
        let s =
            PhaseSnapshot::new(Phase::ConcurrentMark, Phase::ConcurrentMark, epochs, 1).unwrap();
        let color = s.color();
        assert!(colors.insert(color));
        assert_eq!((color & REMAP_MASK).count_ones(), 1);
        let r = encode_ref(32, color).unwrap();
        assert!(s.is_good(r, Generation::Young, Access::Load));
        assert!(s.is_good(r, Generation::Old, Access::Load));
        assert!(s.is_good(r, Generation::Young, Access::Store));
        assert!(!s.is_good(GcRef::NULL, Generation::Old, Access::Store));
        assert!(s.is_good(GcRef::NULL, Generation::Young, Access::Load));
        for other_epochs in 0..32 {
            let other = PhaseSnapshot::new(
                Phase::ConcurrentMark,
                Phase::ConcurrentMark,
                other_epochs,
                1,
            )
            .unwrap();
            let other_ref = encode_ref(32, other.color()).unwrap();
            assert_eq!(
                s.is_good(other_ref, Generation::Young, Access::Store),
                epochs == other_epochs
            );
        }
    }
    assert_eq!(colors.len(), 32);
}

#[test]
fn metadata_validation_distinguishes_object_start_from_interior_and_uncommitted() {
    let r = encode_ref(32, PhaseSnapshot::initial().color()).unwrap();
    assert_eq!(
        decode_ref_checked(r, 0x1000, |_| false),
        Err(GcError::InvalidReference)
    );
    assert_eq!(
        decode_ref_checked(r, 0x1000, |offset| offset == 32),
        Ok(0x1020)
    );
    let padding = encode_ref(48, PhaseSnapshot::initial().color()).unwrap();
    assert_eq!(
        decode_ref_checked(padding, 0x1000, |offset| offset == 32),
        Err(GcError::InvalidReference)
    );
    assert_eq!(
        decode_ref_checked(GcRef::NULL, usize::MAX, |_| panic!(
            "null must not query heap"
        )),
        Ok(0)
    );
}

#[test]
fn phase_publication_and_transition_validation() {
    let s = PhaseSnapshot::initial();
    assert_eq!(s.phase(Generation::Young), Phase::Idle);
    assert!(
        s.transition(Generation::Young, Phase::ConcurrentRelocate)
            .is_err()
    );
    let start = s.transition(Generation::Old, Phase::MarkStart).unwrap();
    assert_ne!(start.color() & OLD_MARK_MASK, s.color() & OLD_MARK_MASK);
    assert_eq!(start.color() & YOUNG_MARK_MASK, s.color() & YOUNG_MARK_MASK);
    let mark = start
        .transition(Generation::Old, Phase::ConcurrentMark)
        .unwrap();
    let young_start = mark
        .transition(Generation::Young, Phase::MarkStart)
        .unwrap();
    assert_eq!(young_start.phase(Generation::Old), Phase::ConcurrentMark);
    assert_ne!(
        young_start.color() & REMEMBERED_MASK,
        mark.color() & REMEMBERED_MASK
    );
    let check = mark
        .transition(Generation::Old, Phase::MarkEndCheck)
        .unwrap();
    assert!(
        check
            .transition(Generation::Old, Phase::ConcurrentMark)
            .is_ok()
    );
    assert!(PhaseSnapshot::from_bits(u64::MAX).is_err());
}

#[test]
fn c_and_rust_layouts_and_reference_bits_match() {
    use std::fmt::Write;
    use std::mem::{align_of, offset_of, size_of};
    use std::process::Command;

    struct Scratch(std::path::PathBuf);
    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
    let scratch = Scratch(std::env::temp_dir().join(format!(
        "klassic-gc-abi-{}-{}", std::process::id(),
        std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()
    )));
    std::fs::create_dir(&scratch.0).unwrap();
    let mut source =
        String::from("#include \"gc_abi.h\"\n#include <assert.h>\n#include <stdio.h>\n");
    macro_rules! layout {
        ($ty:ty $(, $field:ident)*) => {{
            writeln!(source, "_Static_assert(sizeof({}) == {}, \"size\");", stringify!($ty), size_of::<$ty>()).unwrap();
            writeln!(source, "_Static_assert(_Alignof({}) == {}, \"alignment\");", stringify!($ty), align_of::<$ty>()).unwrap();
            $(writeln!(source, "_Static_assert(offsetof({}, {}) == {}, \"offset\");", stringify!($ty), stringify!($field), offset_of!($ty, $field)).unwrap();)*
        }};
    }
    layout!(GcRef);
    layout!(PhaseSnapshot);
    layout!(ObjectId);
    layout!(TypeDescId);
    layout!(Generation);
    layout!(Phase);
    layout!(GcError);
    layout!(ThreadState);
    layout!(
        HandshakeState,
        generation,
        members,
        native,
        expected,
        acknowledged,
        active,
        last_identity,
        head
    );
    layout!(
        ThreadRegistration,
        context,
        identity,
        acknowledged_generation,
        registered,
        registry,
        previous,
        next
    );
    layout!(RootStorage, registration, segment, journal);
    layout!(RootStorageSpec, owner, cells, entries, bytes);
    layout!(RootHandle, cell, reuse_generation, scope);
    layout!(RootCell, value, reuse_generation, active, reserved);
    layout!(
        RootSegment,
        owner,
        scope,
        cells,
        capacity,
        used,
        state,
        journal,
        registration
    );
    layout!(RootChange, previous, next, scope, phase);
    layout!(RootRequest, segment, handle, value);
    layout!(RootScan, segment, cursor, output, capacity, written);
    layout!(
        RootRegistry,
        last_scope,
        last_lease,
        active,
        retiring,
        leases,
        head,
        phase,
        inventory,
        walkers
    );
    layout!(
        RootRegistration,
        segment,
        scope,
        registered,
        leases,
        limit,
        cursor,
        phase,
        required_mask,
        claim_serial,
        claim_end,
        previous,
        next,
        cleanup_cursor,
        inventory_previous,
        inventory_next,
        context,
        owner_previous,
        owner_next,
        journal_serial,
        journal_end
    );
    layout!(RootLease, registration, scope, serial, kind, cursor);
    layout!(
        RootLifeRequest,
        registration,
        lease,
        serial,
        scope,
        context,
        begin,
        end,
        mask,
        phase
    );
    layout!(RootScanJob, scan, lease, serial);
    layout!(DetachRequest, roots, handshake, participant);
    layout!(
        RootWalk,
        registry,
        phase,
        active,
        limit,
        cursor,
        mask,
        completed_mask,
        lease,
        pending,
        pending_reference,
        pending_observed,
        pending_index
    );
    layout!(RootWalkScan, walk, serial, output, capacity, written);
    layout!(RootWalkHeal, walk, serial, index, expected, desired);
    layout!(RootWalkItem, walk, serial, index);
    layout!(RelocationGate, head, active, last_serial, epoch, closed);
    layout!(RelocationPermit, gate, serial, epoch, previous, next, job);
    layout!(RelocationGateRequest, permit, serial, epoch);
    layout!(PinPageRecord, lifetime, admission);
    layout!(PinPageRegistry, pool, records, capacity);
    layout!(RelocationCompletion, owner, job, copy, stage);
    layout!(RelocationDiscard, owner, source, copy, cursor, bytes);
    layout!(AllocationShape, descriptor_id, payload_bytes, allocation);
    layout!(AllocationInto, allocation, root);
    layout!(AllocationStart, registry, shape, root);
    layout!(
        GuardedScalar,
        scope,
        page,
        registry,
        byte_offset,
        phase,
        value
    );
    layout!(InitializationBytes, registry, byte_offset, source, bytes);
    layout!(
        InitializationField,
        registry,
        byte_offset,
        value,
        is_reference
    );
    layout!(
        RelocationCancellation,
        owner,
        permit,
        serial,
        epoch,
        target,
        discard,
        stage
    );
    layout!(MarkedRelocation, claim, queue, record, epoch);
    layout!(PinRelocationClaim, lease, selection, movers, work);
    layout!(PinSourceDrain, registry, page, incarnation, epoch, scan);
    layout!(PinSourceUncommit, registry, epoch, work);
    layout!(PinSourceRequest, lease, selection, work);
    layout!(
        RelocationClaim,
        permit,
        source,
        source_offset,
        incarnation,
        ticket,
        epoch
    );
    layout!(
        RelocationJob,
        owner,
        gate,
        permit,
        serial,
        epoch,
        source,
        source_offset,
        incarnation,
        ticket,
        destination,
        state
    );
    layout!(
        RootAudit, serial, last_scope, phase, state, context, queue, epoch, generation, walk
    );
    layout!(
        MarkPhaseControl,
        handshake,
        audit,
        generation,
        state,
        rendezvous,
        phase,
        epoch,
        outcome,
        remembered
    );
    layout!(CycleLedger, requested, started, completed);
    layout!(CycleTicket, ledger, kind, cycle);
    layout!(CyclePressure, heap, sample, horizon_ns);
    layout!(CycleMarkStart, registry, control);
    layout!(
        RelocationPhaseControl,
        handshake,
        gate,
        generation,
        state,
        rendezvous,
        phase,
        gate_epoch
    );
    layout!(
        RememberedSet,
        start,
        limit,
        incarnation,
        bits,
        words,
        current,
        epoch,
        previous_epoch,
        last_phase,
        current_count,
        pending,
        cursor,
        last_serial,
        active_serial,
        active_offset
    );
    layout!(
        RememberedJob,
        offset,
        incarnation,
        phase,
        serial,
        keep,
        budget
    );
    layout!(MarkEndRequest, handshake, audit, generation, remembered);
    layout!(
        RememberedInventory,
        head,
        members,
        last_identity,
        epoch,
        previous_epoch,
        unflipped,
        pending,
        claims,
        last_phase,
        walkers
    );
    layout!(
        RememberedRegistration,
        registry,
        set,
        identity,
        previous,
        next,
        epoch
    );
    layout!(RememberedInventoryRequest, registration, job, phase);
    layout!(
        RememberedWalk,
        registry,
        registration,
        identity,
        limit,
        epoch
    );
    layout!(RootDrain, buffer, output, capacity, written);
    layout!(
        RootJournalRequest,
        registration,
        lease,
        serial,
        scope,
        begin,
        end,
        output,
        capacity,
        written
    );
    layout!(
        TypeDescriptor,
        id,
        alignment,
        fixed_bytes,
        element_bytes,
        fixed_bitmap,
        element_bitmap,
        fixed_bitmap_words,
        element_bitmap_words
    );
    layout!(DescriptorRegistry, slots, capacity);
    layout!(
        DescriptorScan,
        descriptor_id,
        payload_bytes,
        next_offset,
        slot_budget,
        output,
        output_capacity,
        written,
        examined
    );
    layout!(
        AllocationPage,
        heap_base,
        start,
        limit,
        incarnation,
        identity_base,
        owner,
        generation,
        state,
        starts,
        starts_words,
        allocated_top,
        run_head,
        metadata_leases,
        forward_table
    );
    layout!(
        HeapPool,
        heap_base,
        pages,
        page_count,
        hard_max,
        soft_max,
        relocation_reserve,
        committed,
        pending,
        metadata_max,
        metadata_committed,
        metadata_pending,
        last_identity,
        last_incarnation,
        pressure
    );
    layout!(
        PageLease,
        page_count,
        generation,
        relocation,
        head_index,
        incarnation,
        identity_base,
        stage,
        starts,
        committed_mapping
    );
    layout!(MetadataLeasePool, heap, slots, capacity, free_head, active);
    layout!(MetadataLeaseSlot, generation, page, incarnation, next_free);
    layout!(MetadataLeaseRequest, page, incarnation, token);
    layout!(RememberedLease, registration, claim, lease);
    layout!(ReferenceLease, reference, lease);
    layout!(
        RelocationCopy,
        heap_base,
        source_incarnation,
        ticket,
        source_limit,
        destination,
        destination_limit,
        budget,
        cursor,
        stage
    );
    layout!(ResolveRequest, reference, destination, scope);
    layout!(ForwardTable, cells, capacity, used);
    layout!(ForwardLookup, source_offset, incarnation, cell);
    layout!(ForwardCell, source_offset, incarnation, word, access);
    layout!(
        ForwardIncoming,
        incarnation,
        used,
        target_start,
        target_limit,
        cursor,
        budget,
        examined,
        source_offset
    );
    layout!(
        SourceAccessRequest,
        context,
        source_offset,
        incarnation,
        held,
        ticket
    );
    layout!(
        ForwardRequest,
        source_offset,
        incarnation,
        ticket,
        destination
    );
    layout!(MetadataCharge, bytes, owner, allocation, stage);
    layout!(
        AllocationSample,
        context,
        identity,
        timestamp_ns,
        total_bytes,
        elapsed_ns,
        delta_bytes
    );
    layout!(
        NativeStartupRequest,
        roots,
        descriptors,
        cells,
        entries,
        capacity,
        quantum
    );
    layout!(RootFollow, context, root, cleanup);
    layout!(RootAccess, follow, access);
    layout!(
        RootPin, registry, stage, address, reference, pending, cancel, root, permit, token
    );
    writeln!(
        source,
        "_Static_assert(offsetof(RootPin, self) == {}, \"offset\");",
        offset_of!(RootPin, self_)
    )
    .unwrap();
    layout!(RootPinStart, registry, context, source, reserved);
    layout!(PinRuntime, pages, heap_lock, lock, unlock);
    layout!(
        NativePin, registry, context, charge, storage, stage, reserved, pin
    );
    writeln!(
        source,
        "_Static_assert(offsetof(NativePin, self) == {}, \"offset\");",
        offset_of!(NativePin, self_)
    )
    .unwrap();
    layout!(NativePinStart, registry, context, charge);
    layout!(NativeMetadataOps, quantum, map, unmap);
    layout!(
        RootFinishNotification,
        context,
        event,
        notify,
        stage,
        last_error
    );
    layout!(
        NativePinRuntime,
        pins,
        registry,
        registry_lock,
        lock,
        unlock,
        heap,
        native
    );
    layout!(
        RootedLoadAdmission,
        registry,
        handshake,
        participant,
        queues
    );
    layout!(
        RootedLoad,
        observation,
        marks,
        publication,
        stage,
        last,
        cancel,
        fault,
        retry_event,
        admitted
    );
    layout!(
        RootedMarkSet,
        context,
        root,
        queues,
        epochs,
        phase,
        state,
        required,
        completed,
        current,
        jobs
    );
    layout!(WeakLoadGate, pool, closed, registry, handshake);
    layout!(WeakMarkControl, gate, mark, stage, processor, scan);
    layout!(WeakSlot, gate, serial, reference, lease);
    layout!(WeakEntry, slot, next_free);
    layout!(
        WeakTable,
        gate,
        entries,
        capacity,
        free_head,
        active,
        last_serial
    );
    layout!(WeakHandle, table, index, serial);
    layout!(WeakScanStart, table, owner, context);
    layout!(
        WeakScan,
        table,
        owner,
        context,
        entries,
        capacity,
        last_serial,
        phase,
        epoch,
        generation,
        rendezvous,
        cursor,
        stage,
        work
    );
    layout!(WeakProcessStart, table, owner, context, handle);
    layout!(
        WeakProcess,
        table,
        owner,
        pool,
        context,
        handle,
        source,
        phase,
        epoch,
        generation,
        rendezvous,
        stage,
        result,
        current,
        retired,
        access
    );
    layout!(WeakTableRequest, handle, admission, context, output_root);
    layout!(
        WeakCreateRequest,
        admission,
        context,
        source_root,
        output_handle
    );
    layout!(WeakFollowRequest, index, serial, retired);
    layout!(
        WeakIncoming,
        table,
        entries,
        gate,
        pool,
        capacity,
        last_serial,
        target_start,
        target_limit,
        cursor,
        stage,
        handle,
        observed
    );
    layout!(
        WeakLoadRequest,
        admission,
        context,
        slot,
        serial,
        output_root
    );
    layout!(
        PendingMark,
        context,
        current,
        retired,
        access,
        queue,
        generation,
        epoch,
        phase,
        stage,
        route_result,
        offer_result,
        route
    );
    layout!(
        RootedMark,
        access,
        queue,
        generation,
        epoch,
        phase,
        stage,
        route_result,
        offer_result,
        route
    );
    layout!(
        RootedObservation,
        receiver,
        registry,
        byte_offset,
        output,
        stage,
        result
    );
    layout!(
        RootedScalar,
        receiver,
        registry,
        byte_offset,
        value,
        operation,
        stage,
        result
    );
    layout!(
        RootedStoreAttempt,
        receiver,
        marks,
        commit,
        entries,
        stage,
        result,
        release_index
    );
    layout!(
        RootedStore,
        value,
        observation,
        attempt,
        stage,
        last,
        cancel,
        admitted,
        result
    );
    layout!(
        RootedStoreRuntime,
        heap_pool,
        admission,
        heap_lock,
        registry_lock,
        queue_locks,
        lock,
        unlock
    );
    layout!(WorkerRequest, handshake, participant);
    layout!(MarkRetarget, job, destination, retired);
    layout!(
        GuardedMarkPrepare,
        scope,
        page,
        phase,
        reference,
        lease,
        output
    );
    layout!(GuardedMarkRoute, generation, prepare);
    layout!(
        RememberedTransfer,
        source,
        claim,
        destination,
        offset,
        incarnation
    );
    layout!(
        RememberedOwner,
        registration,
        claim,
        page,
        owner,
        byte_offset,
        cell
    );
    layout!(
        PromotionRemember,
        registry,
        registration,
        owner,
        descriptor_id,
        payload_bytes,
        phase,
        cursor
    );
    layout!(
        RelocationCoordinator,
        pending,
        active,
        turn,
        serial,
        gate,
        gate_epoch
    );
    layout!(
        HeapIncoming,
        scope,
        page,
        registry,
        phase,
        target_start,
        target_limit,
        cursor,
        observed
    );
    layout!(HeapWalk, pool, heap, pages, page_count, cursor, lease);
    layout!(
        ObjectWalk,
        lease,
        page,
        incarnation,
        table,
        used,
        cursor,
        source_offset
    );
    layout!(
        HeapAuditSlice,
        page,
        context,
        reference,
        registry,
        phase,
        target_start,
        target_limit,
        cursor,
        observed,
        access,
        stage,
        pending_cursor,
        pending_observed,
        pending_result
    );
    layout!(
        HeapAudit,
        context,
        registry,
        gate,
        epoch,
        phase,
        target_start,
        target_limit,
        stage,
        findings,
        walk,
        object,
        slice
    );
    layout!(RelocationTurn, coordinator, generation, serial, gate);
    layout!(
        ForwardCompress,
        cell,
        source_offset,
        incarnation,
        observed,
        destination,
        epoch
    );
    layout!(RememberedLoad, registration, claim, source_owner, load);
    layout!(
        GuardedHeal,
        scope,
        page,
        registry,
        byte_offset,
        expected,
        desired,
        phase
    );
    layout!(
        GuardedLoad,
        scope,
        page,
        registry,
        byte_offset,
        phase,
        output
    );
    layout!(
        GuardedStore,
        scope,
        page,
        registry,
        byte_offset,
        expected,
        desired,
        phase,
        transferred,
        satb,
        remembered
    );
    layout!(
        SourceUncommit,
        head_index,
        page_count,
        incarnation,
        stage,
        owner,
        committed_mapping
    );
    layout!(
        SourceRetire,
        head_index,
        page_count,
        incarnation,
        stage,
        owner,
        starts,
        forward_table,
        cursor
    );
    layout!(
        PageRelease,
        head_index,
        page_count,
        incarnation,
        stage,
        owner,
        starts,
        forward_table,
        committed_mapping
    );
    layout!(
        MarkRecord,
        object_id,
        young_epoch,
        old_epoch,
        young_spill,
        old_spill,
        age_epoch,
        age
    );
    layout!(
        SourceDrain,
        incarnation,
        top,
        used,
        cursor,
        budget,
        examined
    );
    layout!(
        MarkScan,
        job,
        heap_base,
        registry,
        output,
        capacity,
        next_cursor,
        written,
        examined,
        locations
    );
    layout!(
        MarkTask,
        next,
        serial,
        state,
        record,
        reference,
        incarnation,
        metadata_lease,
        cursor
    );
    layout!(
        MarkQueue,
        tasks,
        capacity,
        free_head,
        head,
        tail,
        queued,
        in_flight,
        last_serial,
        epoch,
        generation,
        overflow_head,
        overflow_tail,
        overflow_count
    );
    layout!(
        MarkRequest,
        record,
        reference,
        incarnation,
        metadata_lease,
        index,
        serial,
        cursor
    );
    layout!(
        ObjectHeader,
        payload_bytes,
        descriptor_id,
        mark_record,
        state
    );
    layout!(AllocationRequest, page, descriptor, elements, output, marks);
    layout!(MarkArena, records, capacity, used);
    layout!(AllocationResult, reference, object_id);
    layout!(FieldLoc, owner, byte_offset);
    layout!(
        BufferedSlot,
        owner,
        byte_offset,
        page_incarnation,
        record_epoch,
        metadata_lease
    );
    layout!(BarrierEntry, previous, slot);
    layout!(BarrierDrain, buffer, output, capacity, written);
    layout!(BufferCursor, entries, capacity, top, published, consumed);
    layout!(BufferConsumer, last_serial, serial, end, cursor);
    layout!(
        BarrierJob, identity, selector, serial, begin, end, output, capacity, written
    );
    layout!(
        MutatorContext,
        abi_version,
        state,
        thread_id,
        adopted_phase,
        tlab_start,
        tlab_top,
        tlab_end,
        satb,
        remembered,
        root_segments,
        retiring_roots,
        access_epoch,
        root_revision,
        satb_consumer,
        remembered_consumer,
        root_audit_serial,
        root_audit_identity,
        root_audit_revision,
        allocated_bytes,
        private_producers
    );
    for (name, value) in [
        ("GC_ABI_VERSION", ABI_VERSION as u64),
        ("GC_MAX_AGE", MAX_AGE),
        ("GC_DESCRIPTOR_SCAN_MAX_SLOTS", DESCRIPTOR_SCAN_MAX_SLOTS),
        ("GC_ROOT_SEGMENT_ACTIVE", ROOT_SEGMENT_ACTIVE),
        ("GC_ROOT_SEGMENT_PREPARED", ROOT_SEGMENT_PREPARED),
        ("GC_ROOT_SEGMENT_CLEANING", ROOT_SEGMENT_CLEANING),
        ("GC_ROOT_SEGMENT_RETIRING", ROOT_SEGMENT_RETIRING),
        ("GC_PAGE_ALLOCATING", PAGE_ALLOCATING),
        ("GC_PAGE_FREE", PAGE_FREE),
        ("GC_PAGE_PREPARING", PAGE_PREPARING),
        ("GC_PAGE_READY", PAGE_READY),
        ("GC_PAGE_RUN_TAIL", PAGE_RUN_TAIL),
        ("GC_PAGE_CLOSED", PAGE_CLOSED),
        ("GC_PAGE_RELEASING", PAGE_RELEASING),
        ("GC_PAGE_EVACUATED", PAGE_EVACUATED),
        ("GC_PAGE_UNCOMMITTED", PAGE_UNCOMMITTED),
        ("GC_HEAP_PAGE_SHIFT", HEAP_PAGE_SHIFT as u64),
        ("GC_HEAP_PAGE_BYTES", HEAP_PAGE_BYTES),
        ("GC_OBJECT_INITIALIZING", OBJECT_INITIALIZING),
        ("GC_OBJECT_ALIGNMENT", OBJECT_ALIGNMENT),
        ("GC_MAX_OFFSET", MAX_OFFSET),
        ("GC_METADATA_MASK", METADATA_MASK as u64),
        ("GC_REMEMBERED_MASK", REMEMBERED_MASK as u64),
        ("GC_YOUNG_MARK_MASK", YOUNG_MARK_MASK as u64),
        ("GC_OLD_MARK_MASK", OLD_MARK_MASK as u64),
        ("GC_REMAP_MASK", REMAP_MASK as u64),
        ("GC_YOUNG", Generation::Young as u64),
        ("GC_OLD", Generation::Old as u64),
        ("GC_RUNNING", ThreadState::Running as u64),
        ("GC_AT_SAFEPOINT", ThreadState::AtSafepoint as u64),
        ("GC_BLOCKED_NATIVE", ThreadState::BlockedNative as u64),
        ("GC_DETACHING", ThreadState::Detaching as u64),
        ("GC_DETACHED", ThreadState::Detached as u64),
        ("GC_IDLE", Phase::Idle as u64),
        ("GC_MARK_START", Phase::MarkStart as u64),
        ("GC_CONCURRENT_MARK", Phase::ConcurrentMark as u64),
        ("GC_MARK_END_CHECK", Phase::MarkEndCheck as u64),
        ("GC_SELECT", Phase::Select as u64),
        ("GC_RELOCATE_START", Phase::RelocateStart as u64),
        ("GC_CONCURRENT_RELOCATE", Phase::ConcurrentRelocate as u64),
        ("GC_RETIRE", Phase::Retire as u64),
        ("GC_INVALID_SIZE", GcError::InvalidSize as u64),
        ("GC_INVALID_REFERENCE", GcError::InvalidReference as u64),
        ("GC_OUT_OF_MEMORY", GcError::OutOfMemory as u64),
        ("GC_UNSUPPORTED_CONFIG", GcError::UnsupportedConfig as u64),
        ("GC_INVALID_PHASE", GcError::InvalidPhase as u64),
        ("GC_EPOCH_EXHAUSTED", GcError::EpochExhausted as u64),
    ] {
        writeln!(
            source,
            "_Static_assert({name} == UINT64_C({value}), \"constant\");"
        )
        .unwrap();
    }
    source.push_str(
        r#"
int main(void) {
    uint64_t endian = 1;
    assert(*(unsigned char *)&endian == 1);
    uintptr_t address;
    assert(gc_abi_decode_ref((GcRef){0}, UINTPTR_MAX, &address) && address == 0);
    unsigned valid = 0;
    for (unsigned meta = 0; meta <= UINT16_MAX; meta++) {
        GcRef r;
        if (gc_abi_encode_ref(16, (uint16_t)meta, &r)) valid++;
    }
    assert(valid == 48);
    for (uint64_t epoch = 0; epoch < 32; epoch++) {
        PhaseSnapshot p = {epoch << 8};
        assert(gc_abi_valid_phase(p));
        GcRef forgotten;
        assert(gc_abi_encode_ref(32, gc_abi_color_unremembered(p), &forgotten));
        assert((forgotten.bits & GC_REMEMBERED_MASK) == GC_REMEMBERED_MASK);
        GcRef r;
        assert(gc_abi_encode_ref(32, gc_abi_color(p), &r));
        assert(gc_abi_decode_ref(r, 4096, &address));
        assert(!gc_abi_decode_ref(r, UINTPTR_MAX - 31, &address));
        assert(!gc_abi_decode_ref(r, 1, &address));
        assert(!gc_abi_encode_ref(0, gc_abi_color(p), &r));
        assert(!gc_abi_encode_ref(17, gc_abi_color(p), &r));
        assert(!gc_abi_encode_ref(UINT64_C(1) << 48, gc_abi_color(p), &r));
        assert(gc_abi_decode_ref(r, 4096, &address));
        printf("%llu %llu\n", (unsigned long long)r.bits, (unsigned long long)address);
    }
    assert(!gc_abi_valid_phase((PhaseSnapshot){UINT64_MAX}));
    return 0;
}
"#,
    );
    let source_path = scratch.0.join("abi.c");
    let executable = scratch.0.join("abi-test");
    std::fs::write(&source_path, source).unwrap();
    let cc = std::env::var("KLASSIC_CLANG").unwrap_or_else(|_| {
        ["clang-15", "clang", "cc"]
            .into_iter()
            .find(|cc| {
                Command::new(cc)
                    .arg("--version")
                    .output()
                    .is_ok_and(|o| o.status.success())
            })
            .expect("GC ABI conformance requires a C11 compiler; no skip-success")
            .to_owned()
    });
    let output = Command::new(&cc)
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
        .arg("-I")
        .arg(concat!(env!("CARGO_MANIFEST_DIR"), "/runtime/gc"))
        .arg(&source_path)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = Command::new(&executable).output().unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let expected: String = (0..32)
        .map(|epochs| {
            let p = PhaseSnapshot::new(Phase::Idle, Phase::Idle, epochs, 0).unwrap();
            let r = encode_ref(32, p.color()).unwrap();
            format!("{} {}\n", r.bits(), decode_ref(r, 4096).unwrap())
        })
        .collect();
    // Windows' C text stdout expands LF to CRLF; reference bits and decoded
    // offsets must match exactly after normalizing that platform encoding.
    let actual = String::from_utf8(output.stdout)
        .unwrap()
        .replace("\r\n", "\n");
    assert_eq!(actual, expected);
}

#[test]
fn phase_masks_respect_target_generation_and_both_remap_epochs() {
    let initial = PhaseSnapshot::initial();
    let old_mark = initial
        .transition(Generation::Old, Phase::MarkStart)
        .unwrap();
    let young_mark = initial
        .transition(Generation::Young, Phase::MarkStart)
        .unwrap();
    assert_eq!(
        initial.required_mask(Generation::Young, Access::Load),
        REMAP_MASK
    );
    assert_eq!(
        old_mark.required_mask(Generation::Young, Access::Load),
        REMAP_MASK | OLD_MARK_MASK
    );
    assert_eq!(
        young_mark.required_mask(Generation::Old, Access::Load),
        REMAP_MASK
    );
    assert_eq!(
        young_mark.required_mask(Generation::Young, Access::Load),
        REMAP_MASK | YOUNG_MARK_MASK
    );
    for generation in [Generation::Young, Generation::Old] {
        for remap_epochs in [4, 8, 12] {
            let stale = PhaseSnapshot::new(Phase::Idle, Phase::Idle, remap_epochs, 0).unwrap();
            assert!(!initial.is_good(
                encode_ref(16, stale.color()).unwrap(),
                generation,
                Access::Load
            ));
        }
    }
    let exhausted = PhaseSnapshot::new(Phase::Idle, Phase::Idle, 0, MAX_OFFSET).unwrap();
    assert_eq!(
        exhausted.transition(Generation::Young, Phase::MarkStart),
        Err(GcError::EpochExhausted)
    );
}

#[test]
fn every_phase_edge_and_roundtrip_obeys_the_transition_table() {
    let phases = [
        Phase::Idle,
        Phase::MarkStart,
        Phase::ConcurrentMark,
        Phase::MarkEndCheck,
        Phase::Select,
        Phase::RelocateStart,
        Phase::ConcurrentRelocate,
        Phase::Retire,
    ];
    for generation in [Generation::Young, Generation::Old] {
        for (index, current) in phases.into_iter().enumerate() {
            let snapshot = PhaseSnapshot::new(current, current, 0, 17).unwrap();
            assert_eq!(PhaseSnapshot::from_bits(snapshot.bits()), Ok(snapshot));
            for (next_index, next) in phases.into_iter().enumerate() {
                let expected = next_index == (index + 1) % 8 || (index == 3 && next_index == 2);
                let transition = snapshot.transition(generation, next);
                assert_eq!(
                    transition.is_ok(),
                    expected,
                    "{generation:?} {current:?} -> {next:?}"
                );
                if let Ok(next_snapshot) = transition {
                    assert_eq!(next_snapshot.phase(generation), next);
                    assert_eq!(next_snapshot.bits() >> 16, 18);
                }
            }
        }
    }
}

#[test]
fn forgotten_remembered_stamp_never_fast_paths_after_parity_wrap() {
    let initial = PhaseSnapshot::initial();
    // A forgotten slot needs a persistent slow-path stamp, not an old parity
    // that becomes current again after two Young cycles.
    let forgotten = encode_ref(32, initial.color_unremembered()).unwrap();
    for epochs in 0..32 {
        let phase =
            PhaseSnapshot::new(Phase::ConcurrentMark, Phase::ConcurrentMark, epochs, 0).unwrap();
        assert!(!phase.is_good(forgotten, Generation::Old, Access::Store));
    }
    assert_eq!(decode_ref(forgotten, 0x1000), Ok(0x1020));
}
