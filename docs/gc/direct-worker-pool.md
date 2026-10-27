# Direct GC worker pool

This is the first worker-pool stage of issue #678. The direct runtime starts
two OS GC workers. Each has a stable, unregistered owner view, its own captured
phase/bank epoch, and independent stack/TLS or thread/event resources. Mutator
registration and language-thread semantics are unchanged.

The fair collector ticket still serializes graph quanta, root/journal cursors,
and collector copy scratch. Two GC threads do not establish parallel graph
tracing or work stealing. Those require independently claimed jobs and explicit
in-flight ownership before work can leave the ticket.

The common runtime gives each OS callback its actual worker-slot pointer.
Callbacks and park returns retain that pointer instead of borrowing the primary
mutator's phase view. Every worker raw-ticket acquisition/release validates its
own epoch and updates aggregate active/entered/completed accounting. Existing
phase and bank-transition checks consult that aggregate; they cannot certify
quiescence by observing just worker zero. Metadata-growth services recognize
either pool worker and do not attempt to register it as a Native mutator.

Launch failure rolls back only the failed slot. A successfully launched peer
retains its resources and can continue collection. The existing scheduler's
worker-alive flag still means that at least one worker exists; a separate stop
request prevents confusing a live count of two with shutdown. Wakeup broadcasts
to each live worker, including separate Windows auto-reset events. Shutdown
releases the collector ticket before joining every launched worker, and frees
each slot's OS resources only after that thread terminates. Repeated shutdown
does not join or release twice.

Directed tests exercise distinct OS thread IDs/resources, actual Young and Old
trace work attributed to both workers, a mutator and one worker progressing
while the second waits before its ticket, failure of the second clone/bootstrap,
and invalid phase/bank changes made by the second actual worker. These tests
retain their generated executable through Rust-side assertions. Validation
on Rust 1.99 passed the four new Linux pool tests, seven copy-claim tests,
twelve Darwin ARM64 emitted-instruction model tests, and six Windows production
ABI/lifecycle model tests. The existing worker-focused run passed its 65 Linux
and other tests; its two OS model failures were corrected and passed in the
platform reruns above. The final full native-library run passed all 622 tests in 548.19 seconds.
OS models do not establish native Windows or Darwin execution.

The pool is initially fixed at two workers, with the existing zero-worker
fallback if neither launch succeeds. Adaptive/configurable worker counts,
parallel tracing, remaining stop-time work, and complete #678 acceptance remain
separate work. The original #676 moving failure is not resolved by this change.
