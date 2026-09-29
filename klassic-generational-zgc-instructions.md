# Klassic Generational ZGC — Full Implementation Plan

> 実装エージェントへの指示書。最終目標は **Generational ZGC**。単世代 GC、incremental GC、背景スレッドを追加しただけの collector を完成扱いにしない。
> 各 Task をチェックボックス単位で実施する。superpowers が利用できる場合は subagent-driven-development または executing-plans を使える。利用できない場合も本書の仕様・検証条件を満たすこと。

**Goal:** Klassic の標準 native 出力に、Young / Old の世代別回収、並行マーキング・再配置、低停止時間、実運用に必要なメモリ管理と診断を備えた Generational ZGC を実装する。

**Architecture:** 既存の PortableAsm と C runtime を拡張し、共通の GC ABI・状態機械・バリア契約を実装する。C は sanitizer で検査する実装、direct emitter は外部コンパイラ／リンカ不要の本番経路として、同じ conformance suite を通す。OpenJDK のアルゴリズムを参照し、Klassic の実行系に適応する。

**Tech Stack:** Rust 2024 workspace、手書き x86-64 / AArch64 codegen、C11 runtime、LLVM IR、既存 ELF / PE64 / Mach-O writer。

**Spec:** 本書 §1–§8 が設計仕様、§9 が実装計画、§10–§12 が検証・完了条件。

**調査日:** 2026-09-26 JST。ユーザーは「Generational ZGC 実装する方針」と明示した。

**Klassic baseline:** main commit **9346bff7ecb9a29e26efed17069567ba29cc91c1**。

**OpenJDK 参照 baseline:** tag **jdk-25+36**、commit **6c48f4ed707bf0b15f9b6098de30db8aae6fa40f**。最新版という意味ではなく、比較する実装を固定するための参照点。

## Global Constraints

- 通常の klassic build が対象。LLVM backend だけの実装では完了しない。
- Linux x86-64 ELF、Windows x86-64 PE64、macOS AArch64 Mach-O の direct build を維持し、各 OS で実行検証する。
- default build に clang / cc / as / ld / link.exe / JVM を追加しない。C / LLVM の検証ジョブでは既存同様 clang を使用できる。
- evaluator を意味論の oracle として維持し、GC のために無関係な言語仕様を変更しない。
- AGENTS.md / CLAUDE.md を読む。ただし GC の現状はコードで確認する。古い「COMPLETE」「GC がない」という説明を根拠にしない。
- 本書の新規ファイル、API、CLI 名は実装先の提案であり、baseline に存在するという意味ではない。
- 無制限 STW fallback、永久 pin、回収不能 arena、単世代モードへの黙った退避で成功を装わない。
- 必須テストの skip、未計測のゼロ値、未対応 backend を完成の証拠にしない。

## Review Focus

1. Old → Young の唯一の参照が store / promotion / remset flip と競合する。
2. 同一 slot の実更新と self-heal、copy 完了と forwarding publication が競合する。
3. safepoint / blocking I/O / callback / attach を越えて raw/interior pointer が残る。
4. 巨大配列・深い stack・残存 mark frontier が停止区間を長くする。
5. to-space 不足、色の周回、同じアドレスの再利用、large-object churn が破損やリークを起こす。

## 1. 完成条件となる capability contract

ZGC は独立した言語規格の名前ではない。本書では **Klassic に適用できる Generational ZGC の中核機能と運用機能**を次の契約として固定する。

| ID | 必須機能 | 完了の証拠 |
|---|---|---|
| G01 | Young / Old、age、promotion | young-only cycle と promotion 後の参照・identity |
| G02 | N mutator + GC workers の並行実行 | 1 / 2 / 8 mutator、background phase 中の mutator progress |
| G03 | 両世代の並行マーキング | SATB / roots / queue / in-flight を含む termination |
| G04 | 両世代の並行再配置・圧縮 | reader / writer と競合しても論理 object が一意 |
| G05 | 世代別 colored references と load barrier | Young/Old remap の組合せ、古い参照修復、null |
| G06 | generational store barrier | overwritten value の SATB、slot の remset 登録 |
| G07 | 二重化 remembered sets | current/previous、flip 前 buffers、移動する slot |
| G08 | 並行 root 処理 | 全 stack / 全 roots を global pause 中に走査しない |
| G09 | Old mark 中の Young collection | Old mark を破棄せず、Young move / promotion と調停 |
| G10 | heap / TLAB / metadata 管理 | hard/soft max、large objects、uncommit、pre-touch |
| G11 | exhaustion と progress | relocation reserve、allocation stall、健全な OOM |
| G12 | 適応制御と複数 worker | 開始時期、世代容量、tenuring、worker 数の調整 |
| G13 | native boundary | attach/detach、blocked state、handles、scoped pin |
| G14 | runtime weak handle | strong mark 後の clear、weak-load keep-alive race |
| G15 | backend parity・診断 | direct 三ターゲット、C、既存 LLVM subset、実測 |

Java の class unloading、JVMTI、ReferenceQueue API、Java finalization、JIT deoptimization、virtual-thread stack chunks は対応する言語/VM機能がないため今回新設しない。将来追加する場合は別 capability とする。Generational GC の必須機能を削る理由にはしない。

weak handle は runtime 内部の最小 API とし、言語表面に WeakReference / finalizer を追加しない。ephemeron は別機能であり、weak key/value と同一視しない。

## 2. baseline の実態

| 経路 | 主な実装 | 現状 |
|---|---|---|
| direct ELF / PE | lib.rs、portable_asm.rs、gc_layout.rs、pe.rs | precise roots、3色、region、allocation が駆動する incremental mark/relocate |
| direct Mach-O | aarch64.rs、macho.rs、portable_asm.rs | **既に共有 emitter の moving GC を使用**。GC なしという古い説明は不正確 |
| C runtime | runtime/gc/klassic_gc.c / .h | background GC thread、N-mutator table、TLAB、全員停止の handshake |
| LLVM | crates/klassic-native/src/llvm.rs | scalar と scalar-field record 中心の subset、C GC ABI、atomic field load/poll |
| evaluator | crates/klassic-eval/ | 別の Rust 表現。native の意味論 oracle |
| portable C backend | cbackend.rs | 別の限定的出力経路。GC を利用する経路を追加するなら同じ C ABI に接続 |

### 2.1 設計の前提を誤らないための注意

- PortableAsm は既に direct GC の共有先。新規機能は隣接 module に追加し、lib.rs の既存命令生成を全面分割しない。
- AArch64 に PortableAsm 実装と allocation / barrier / root / relocation の接続がある。Windows / AArch64 は baseline の pause timing が無効な箇所を持つ。ゼロ値は測定結果ではない。
- lib.rs の compile_thread は callback を queue に置き、後で逐次実行する。GC 用 OS worker は別途必要。
- direct forwarding は旧 word0 を new_user|FWD、旧 word1 を original size に置換する。C は word0 の size を保ち、word1 に forwarding を入れる。**同じ header layout ではない。**
- heap は基本 128 KiB × 512 = 64 MiB。これを最終容量上限に固定しない。
- direct の large object は管理・走査・回収される。ただし tail allocation 中心で free-run 再利用・合体が不足する。C の allocation は region より大きい object を拒否する。
- compile_gc_write / AArch64 __gc_write は実在する。immutable という説明だけで store-barrier coverage を省略しない。
- direct の mask は既に動的。固定 M0 と誤認して修正しない。
- marking quantum は object 数で区切るが、巨大 object の field 走査は分割されない。

### 2.2 最初に再現・修正する項目

以下は静的監査による指摘であり、この指示書作成時に crash / sanitizer failure を再現したという意味ではない。

1. C gc_finish_mark_action → gc_mark_end は停止中に無制限 drain、ghost freeing、全 object sweep、selection、root target evacuation を行う。O(roots) という説明と一致しない。direct MarkEnd も同種の処理を持つ。
2. C pause timer は全 mutator が park した後に始まり、time-to-safepoint と先に park した thread の待機を除外する。
3. C klassic_gc_write は color-on-store のみ。self-heal / trace recolor は unconditional atomic store。mutable slot に対しては実更新を上書きしうる。atomic store は CAS ではない。
4. C mark_visit は header を atomic OR する一方、gc_trace の size read などに plain read がある。並行アクセスを統一する。
5. C collection_count / live_region_count は concurrent write と整合する読み方か監査する。
6. C align16(size + 16) を checked arithmetic 化する。UINT64_MAX 近傍の wrap を拒否する。
7. zero-payload object の user address が次 region の先頭になる場合、owner region は header/object-start metadata から求める。
8. C gc_read は暗黙 attach し、attach は handshake を待ちうる。raw interior address を受け取る poll-free API と矛盾する。
9. payload memcpy の安全性には「source へ書く actor がいない」ことの証明が必要。現行の正しい契約下で race が実証されたとは断定しない。GC self-heal も actor に含める。
10. gc_mt_test.c の thread ごとの survivor だけでは、共有 slot・cross-thread publication・同じ object の forwarding 競合を試せない。

## 3. 採用するアーキテクチャ

比較した選択肢は、A: 既存 direct/C を共通契約で拡張、B: 外部リンク runtime に統一、C: HotSpot をそのまま移植。

**A を採用する。** B は toolchain-free default と衝突し、C は VM の object/thread/compiler model まで持ち込む。既存 PortableAsm を生かし、以下を共通化する。

- gc_abi.rs: ABI version、layout、世代、epoch、mask、状態遷移、public contract。
- gc_model.rs: ObjectId と logical graph を用いた決定的な小モデル。
- C runtime と emitted machine code を同じケースで検証する conformance harness。
- Rust 定数と C header の生成または機械的な一致検査。

旧 collector は移行・回帰比較用に保持できる。最終 default を Generational に切り替え、明示指定 --gc=generational でも同じ経路を通す。C だけ先に完成させて direct の移植を最後に一括実施する進め方は避ける。

## 4. GC ABI・参照表現

### 4.1 型と lifetime

| 型 | 契約 |
|---|---|
| GcRef(u64) | null または色付き managed reference。直接 dereference 不可 |
| ObjectId(u64) | debug 用の移動しても変わらない identity |
| RootHandle | nonmoving atomic root cell の識別子。cell reuse generation と所有scopeを含む |
| FieldLoc { owner: RootHandle, byte_offset: u64 } | 同期呼出し中だけ借用する field 指定。呼出し中は owner の論理値を固定 |
| BufferedSlot | enqueue 時点の owner GcRef、field offset、page incarnation、record epoch、metadata-lifetime lease |
| MutatorContext | TLAB、buffers、root segments、phase cache、thread state |
| PhaseSnapshot | Young/Old mark/remap、remset epoch、phase の整合した組 |
| TypeDescId(u32) | size / alignment / pointer bitmap / array layout の descriptor |
| ResolvedAddress | barrier 後の一時アドレス。safepoint / blocking / callback を越えない |

旧 tag 1/2/4 は TypeDesc に対応付ける。mixed scalar/reference record、list length、enum discriminant、closure capture、static roots を正確に扱う。pointer らしい整数を conservative root にしない。

### 4.2 colored reference

旧 M0/M1/R だけで Generational の epoch を表さない。初期実装は ISA 共通の単純な encoding とする。

- 上位 48 bit: heap-base からの byte offset。下位 16 bit: metadata。
- encode(offset, meta) = (offset << 16) | meta。範囲・alignment を検査し、heap 先頭の guard granule は allocation に使わない。null は 0。
- decode(0) は heap-base を足す前に null address 0 を返す。nonzero reference は nonzero offset、許可 metadata/one-hot remap、checked heap_base+offset、committed object-start metadata を検証する。
- bit 4–5: remembered parity、8–9: Young marked、10–11: Old marked、12–15: Young/Old remap 4組の one-hot。0–3 と 6–7 は予約。
- load_good / mark_good / store_good と許可する色の組合せは Task 1 の phase table から生成する。
- **null の store fast path は特別扱いする。** 常に 0 とするこの ABI では、OpenJDK の colored-null 最適化をそのまま移植しない。previous==0 は現在の store/remset epoch の証拠にならない。初回に限らず全ての zero→nonnull store は owner/page metadata で必要な記録を確認するか slow path に入る。promotion と null clear 後の再書込みも含む。
- 48-bit offset の表現範囲と OS が予約可能な容量を区別する。大きい heap の設定失敗は明示診断する。

これは Klassic 独自の ABI。OpenJDK の x86 向け shift 最適化や AArch64 の補数 remap 表現と bit 互換とは主張しない。まず意味論を固定し、その後 ISA 別 fast path を最適化する。

色の parity 周回には、古い参照の存続上限と reclamation protocol が必要。metadata に 64-bit cycle ID を追加するだけでは、古い GcRef の識別は保証できない。色を再利用できる条件をモデルで定義する。

### 4.3 object/page metadata

- size と descriptor は forwarding によって壊さない。
- forwarding は非移動 table に置き、(source page incarnation, object offset) → destination を原子的に公開する。empty / claimed / published / retained を区別する。
- mark bitmap、live bytes、age/generation、page state、pin count、remembered bitmap を持つ。
- source page と forwarding table の寿命を分ける。同じ仮想アドレスを再利用して古い参照が別 object に化けないようにする。
- **初期実装では仮想アドレスの再利用を保守的に遅延する。** GcRef 自体は incarnation を持たないため、forwarding key に incarnation を足すだけでは stale reference を判別できない。旧 incarnation を参照しうる live heap slots/roots/buffers/jobs/native handles が remap 済みまたは quiescent と確認できるまで同じ virtual range を再利用しない。physical page recycling と virtual-address reuse を区別する。より早い再利用は color→incarnation の一意な解決をモデルで証明してから導入する。debug ObjectId を release build の安全性の代替にしない。
- identity と structural equality を区別する。必要な identity hash を物理 address だけから作らない。
- Rust Vec/String/Box 等の所有権を持つ構造体を丸ごと二重 copy/drop しない。managed payload と外部 resource の所有権を分離する。

## 5. 並行バリアと状態機械

### 5.1 load / store / publication

**Load barrier**

1. receiver を canonical 化してから field address を作る。
2. slot を atomic load し、phase snapshot と照合する。
3. slow path は Young/Old forwarding を解決し、必要なら relocation を助ける。
4. self-heal は CAS(slot, observed, healed)。失敗時に他 thread の実更新を上書きしない。
5. raw address は有限の no-safepoint access 区間だけで使う。巨大処理を一つの区間に閉じ込めない。

**Store barrier**

1. owner と新値を root に保持し、必要な poll 後に canonical な owner / field を再解決する。
2. **上書き前の値**を読み、対象世代の SATB に必要な old referent を記録する。
3. remembered-set の記録は **source slot の位置**。新 target の記録だけで代替しない。
4. bookkeeping が termination / remset handoff から観測できるようにして、新しい store-good 値を格納する。
5. null store にも pre-barrier が必要。null だから old referent の保護を削除しない。

初期化中と公開済み object を区別する。TLAB 内の未公開/未走査区間、zeroing、header、fields、root publication の順序を決める。allocate-black だけで未初期化 field の走査や child の保護が解決したと思わない。

atomic CAS/exchange と bulk copy も専用経路を持つ。CAS は比較する logical reference を正規化し、色だけ違う同じ object を不一致としない。OpenJDK の「slot を store-good に heal してから CAS」を参照する。bulk copy は source load barrier と destination pre-store barrier の両方を満たす。

GC 自身の slot 更新にも別の契約を与える。collector の recolor を mutator の論理 store と混同しない。language-level Release/SeqCst semantics と GC bookkeeping の順序はそれぞれ満たす。

### 5.2 SATB と remembered sets

- mutator ごとに bounded buffer。entry は previous referent、BufferedSlot、記録 epoch を含む。
- FieldLoc の RootHandle を buffer に保存しない。cell が非移動でも後で overwrite/release/reuse されれば別 object を指す。enqueue 時の owner 値を BufferedSlot に固定し、flush まで必要な page/forwarding metadata を lease で保護する。
- buffered owner の metadata 保護と strong reachability を区別する。dead owner を永久 strong-root にせず、死亡確定した entry は安全に破棄する。entry 処理後に lease を解放し、epoch retirement は全該当 buffer の完了を待つ。
- 満杯時は flush / assist / backpressure。entry を捨てず、GC-managed allocation に再帰しない。
- Old page に current / previous の precise slot bitmap。current を mutator が更新し、previous を Young worker が走査する。
- Young MarkStart の epoch adoption と flip は同期する。二重化しただけで同期不要にはならない。
- flip 前に local buffer に残った entry は、その epoch の root として処理する。flip 後に previous へ遅れて書いても安全とは限らない。
- 生きた Young を指し続ける slot は次 cycle にも保持する。走査済みだから一律に忘れない。
- Old owner が移動した時、slot の owner/offset を destination に移す。
- promotion では全 reference slot と null slot を新しい Old 契約に合わせ、Young child を漏らさず登録する。
- dead owner/page の metadata を回収し、remset が無制限の retention を起こさないようにする。

### 5.3 marking / termination / 世代間調停

各世代の状態は次を基本とする。

~~~text
Idle -> MarkStart -> ConcurrentMark -> MarkEndCheck
     -> Select -> RelocateStart -> ConcurrentRelocate -> Retire -> Idle
~~~

この名称は新実装用であり、baseline の関数名ではない。

- MarkStart: epoch adoption、TLAB/buffer/root-segment 境界を公開。全 root traversal はしない。
- ConcurrentMark: roots / heap / SATB / remset を分割処理。work stealing と in-flight jobs を数える。
- MarkEndCheck: 全 producer の publication と scanner 完了を確認。仕事が残るなら再開して ConcurrentMark に戻す。
- 停止中に while(work remains) で drain しない。bounded metadata check のみ許可する。
- 大きい pointer array は field/byte 単位で job を分割する。object 数だけの quantum にしない。
- root deletion、new allocation、promotion、detach ごとの marking invariant を記述する。

Young-only cycle は Old 全 object の traversal を必要としない。Old mark 中にも Young cycle を実行し、Old mark queue / SATB buffer に入った古い Young reference を解決する。

初期 implementation は二世代の relocation を同時にしなくてもよい。ただし Old mark を保ったまま Young を回収できることは必須。phase 排他は明示し、Old relocation の間に Young が starvation しないことを測る。

### 5.4 concurrent relocation

- live statistics と relocation-set selection は concurrent に行う。
- set を公開する境界では raw register 値を invalidate し、以後 root-load barrier から再取得する。選択した source に書き込める mutator が残らないことを証明する。
- この quiescence は GC worker にも必要。root/remset scanner、mark job、healer を含む全 actor が新 epoch を採用し、旧 raw-address lease を解放してから evacuation を開始する。worker job は yield/phase adoption を越えて raw slot を保持せず、owner reference+offset から再解決する。
- mutable published object にも canonical receiver protocol を適用する。reference field だけでなく scalar field のアクセスも対象。
- copy 完了後に forwarding を release publish。reader は acquire。CAS loser の未公開 copy を安全に reclaim する。
- GC self-heal を含め source に書ける actor を列挙する。atomic memcpy だけでは lost update を解決できない。
- source と destination を二つの論理 object として観測させない。
- 巨大 object を mutator barrier で一括 copy しない。large object は専用 page の nonmoving / in-place promotion を許可するが、dead large object は回収する。
- to-space 不足時は未移動 source の retention 等で健全性を保つ。公開済み forwarding を取り消さず、無制限 retry をしない。
- retiring page、forwarding table、色の再利用を同じ epoch protocol で調停する。

### 5.5 memory ordering と lock discipline

| 共有データ | 条件 |
|---|---|
| PhaseSnapshot | 一つの release publication と acquire adoption。mask を混在させない |
| object / forwarding | 初期化・copy 完了後に publish。利用者は acquire |
| mark bit / queue | bit 獲得から enqueue までを termination の in-flight に含める |
| slots / roots / headers | concurrent plain access と atomic access を混在させない |
| self-heal | observed value に対する CAS |
| TLAB / page / thread | 所有権移行と happens-before を field ごとに明記 |
| stats | race-free snapshot。未同期統計に正しさを依存させない |

C11、LLVM IR、x86-64、AArch64 の四者で同じ契約を実現する。Rust compiler 内の atomic と、**出力した実行ファイルの atomic 命令**は別。x86 で動いたことを ARM ordering の検証に代えない。

worker が mutator の park を待つ間に、mutator が必要とする data lock を保持しない。NoSafepoint/heap-access guard を持って worker 完了を待たない。safe Rust の &T / &mut T を移動や並行更新をまたいで公開せず、unsafe ごとの lifetime と根拠を記載する。

## 6. roots、safepoints、native boundary

既存の「C stack 上の void* slot のアドレスを並べ、停止中に全走査する」方式を、そのまま concurrent scan にしない。

**採用:** segmented nonmoving root cells。各 cell は atomic GcRef、compiler の heap local / argument / return value は RootHandle で保持する。collector は普通の stack slot を勝手に並行書き換えない。

- frame ごとの segment / active range を phase transition で公開。走査は worker/mutator が分割する。
- root read は relocation を解決し、root write/clear は active marking の deletion/publication barrier を持つ。
- frame pop、cleanup、return、tail call、異常終了で scanner が触っている segment を再利用しない。active mark ごとの deletion barrier 相当の仕事を retirement queue に渡し、全 cell 処理が終わるまで segment を保持する。巨大 frame を pop する際に全走査を同期実行しない。
- global native handles と weak handles は別 table に置く。weak を strong root に数えない。
- polls: function entry、loop backedge、allocation slow path、blocking/native 遷移前後。
- 長い builtin loop、string scan、large copy は chunk 単位で poll と address 再解決を行う。
- 引数評価途中、nested allocation、return、closure capture の live temporary も rooting する。
- interior pointer は owner handle + offset で保持し、poll 後に再計算する。
- call/return では caller の result root に値を移してから callee segment を退役させる。
- raw-address helper は attach 済み thread 専用で、暗黙 attach/park をしない。

Mutator state は Running / AtSafepoint / BlockedNative / Detaching / Detached。blocked I/O の thread を永遠に待たない。native に入る前に roots/buffers を公開し、戻る時に epoch を取り込む。

OS/FFI に raw buffer を渡す場合は copy-out または scoped pin。pin は解除を保証し、対象 page を relocation set に入れない。全 heap の永久 pin は禁止する。

pin は selection と forwarding publication の両方と原子的に調停する。すでに selected/moving なら rooted handle を保持して移動を assist または安全に待ち、canonical destination を pin する。旧 source address を返さない。pin 要求直前/直後に selection が入る二方向の競合をテストする。

GC 用 OS worker と既存 Klassic thread の言語意味論は分ける。N-mutator 契約は runtime harness で検証できる。この作業だけで言語の thread を並列意味論へ変更しない。

**API の返値受渡し規則:** 本書の「->GcRef」は論理的な返値を表す。safepoint しうる allocation/load/weak-load の機械語・C ABI は、呼出し前に確保した空の output RootHandle に結果を publish する形式とする。出力 cell の確保自体も、入力値を root に置いてから行う。例えば実 ABI は gc_alloc_into(ctx,desc,size,out_root)、gc_load_ref_into(ctx,loc,out_root)、gc_weak_load_into(ctx,weak,out_root)->bool。C helper で直接 GcRef を返せるのは、返却から root adoption までとその adoption 自体が no-safepoint だと証明できるものに限定する。gc_root_new(ctx,value) も、empty root の予約→保護された入力値の転記という順で lower する。API ごとの may-safepoint 表を gc_abi に置く。

## 7. heap、scheduler、運用機能

- Young/Old は shared pool から需要に応じて page を確保し、固定 50:50 split にしない。
- small/medium/large policy を持つ。初期 small page は既存 128 KiB を維持できる。large-run free / coalescing / reuse は必須。
- hard_max / soft_max / min_committed / initial_committed を区別する。初期 defaults は min=initial=16 MiB、soft=max=256 MiB。CLI で変更可能とする。
- TLAB、evacuation reserve、mark buffers、metadata を別計上。metadata OOM や worklist overflow で到達可能 object を捨てない。
- allocation rate、GC throughput、survival、promotion、headroom から次 cycle を開始する。
- tenuring threshold、Young capacity、worker 数を適応調整する。上下限と hysteresis を持つ。
- 1 worker や固定 age は中間段階として許可するが G12 完了ではない。
- idle free page を遅延 uncommit。min 未満に縮めず、source/forwarding/pinned page を誤解放しない。recommit failure を注入テストする。
- pre-touch と OS large pages を設定可能にする。未対応 OS は明示診断し、成功を装わない。
- OOM は既存と同様、明確な診断と制御された非ゼロ終了にできる。SIGSEGV や永久待機で代用しない。

新 CLI: --gc=generational、--gc-min-heap、--gc-max-heap、--gc-soft-max-heap、--gc-workers、--gc-uncommit-delay、--gc-pre-touch、--gc-large-pages=off|try|required、--gc-verify、--gc-log-format=text|json。

既存 --gc-log / --gc-stress / --gc-poison を維持する。単位、overflow、min<=initial<=soft<=max、worker 数の制約を CLI tests にする。initial は当初 min と等しくしてよい。

poison は ISA 別に設計する。x86 の非 canonical fault を AArch64 に仮定せず、強制 slow path、page protection、ObjectId/incarnation verifier を併用する。

## 8. ファイル構成

新規 C module を分割したら llvm.rs の link command と run_tests.sh の source list も同時に更新する。

| パス | 責務 |
|---|---|
| crates/klassic-native/src/gc_abi.rs | 新規。共通 ABI/layout/phase |
| crates/klassic-native/src/gc_model.rs | 新規。deterministic interleaving model |
| crates/klassic-native/src/gc_layout.rs | 旧 layout からの移行 |
| crates/klassic-native/src/portable_asm.rs | atomic / fence / TLS / worker / park primitives |
| crates/klassic-native/src/gc_codegen/mod.rs | 新規。Generational runtime emitter 入口 |
| gc_codegen/{barrier,mark,relocate,remembered,roots,heap,scheduler}.rs | 上記配下に新規。機能別 emitter |
| crates/klassic-native/src/{lib,aarch64}.rs | adapter、heap access funnels、runtime 接続 |
| crates/klassic-native/src/{pe,macho}.rs | OS import、startup、worker/TLS/wait |
| runtime/gc/klassic_gc.{c,h} | compatibility entry points、新 API |
| runtime/gc/gc_{abi,heap,barrier,mark,relocate,remembered,roots,threads,scheduler,verify}.{c,h} | 必要な .c/.h を新規。ABI の .h は生成/検査 |
| crates/klassic-native/src/llvm.rs | atomic IR、root-cell ABI、barriers/polls |
| src/{cli,main}.rs | config と診断 |
| tests/{gc_abi,gc_generational,gc_differential,gc_latency}.rs | 新規検証 |
| runtime/gc/gc_{model,barrier,generational,relocation,roots,oom}_test.c | 新規 deterministic regressions |
| tests/cross-exec/、.github/workflows/ci.yml | 三 OS での実行検証 |
| docs/gc/{generational-design,invariants,capabilities,benchmarks}.md | 仕様、証明責務、到達点、再現条件 |
| docs/{zgc-plan,true-zgc-plan,architecture-rust}.md、docs/book/、CLAUDE.md | 古い説明と完了主張の更新 |

全て空 scaffold として先に作らず、該当 Task で使うものだけ追加する。

## 9. 実装タスク

各 Task は、失敗するテスト → failure 理由の確認 → 実装 → targeted test → 関連回帰 → independent review → 小さな commit の順。競合は sleep 頼みではなく test-only hook / latch で順序を固定する。

下記 API は新しい ABI の契約。C は opaque type / out parameter 等で同等に表現できる。関数名・引数の対応表を gc_abi.rs と gc_abi.h に固定し、呼び出し側ごとに変更しない。

### Task 0 — baseline と境界不具合

**Files:** 既存 C GC/tests、tests/cli_smoke.rs、docs/gc/capabilities.md。
**Consumes:** baseline の既存 ABI。
**Produces:** test-only gc_test_hook(event, object_id)、現状の coverage matrix。

- [x] HEAD、dirty tree、target matrix、ツールを記録。baseline が違えば §2 を再確認する。
- [x] overflow / zero-size boundary / shared marked header / concurrent stats / first-read attach の regression を作り、期待する失敗を確認する。
- [x] overflow、境界、atomic access、stats、attach 契約の局所修正を行い、同じテストで成功を確認する。STW 全走査の除去、Generational barrier、全共有競合対応は後続 Task の gate として記録し、Task 0 に全機能を詰め込まない。
- [ ] 全 allocation / load / store / root / blocking call site を inventory にする。
- [x] 既存 pause の計測条件と O(heap) の仕事を記録する。未再現の疑いを「再現済み」と書かない。
- [ ] commit: fix: harden GC boundaries before generational migration。

### Task 1 — 状態モデルと共通 ABI

**Files:** gc_abi.rs、gc_model.rs、gc_abi.h、tests/gc_abi.rs、design/invariants。
**Consumes:** §4 の layout と §5 の invariant。
**Produces:** GcRef / PhaseSnapshot / TypeDescId、encode_ref(offset:u64, metadata:u16)->Result<GcRef,GcError>、decode_ref(value:GcRef, heap_base:usize)->Result<usize,GcError>。

- [ ] 2 generations、2 mutators、2 workers、最大4 objects/8 slots のモデルを作る。alloc/root/load/store/mark/move/flip/retire を step 実行する。
- [ ] reachable object が reclaim されない、identity が一意、完了した store が失われないことを assert する。
- [ ] Old mark 中の Young move/promotion、色の複数周、source address 再利用を探索する。
- [ ] phase × generation × color の transition table、null、remset handoff、color reuse 条件を固定する。
- [ ] ABI size/alignment/endianness と Rust–C 一致を検査する。
- [ ] MutatorContext の共有 layout と buffer interface を固定する。Task 2/3 が別々の context 定義を作らないようにする。
- [ ] targeted command: cargo test --test gc_abi。model tests は cargo test -p klassic-native gc_model。全 invariant が PASS になること。
- [ ] commit: feat: define generational GC ABI and state model。

### Task 2 — atomic 命令・OS worker・thread lifecycle

**Files:** portable_asm.rs、x86/ARM adapters、gc_threads.*、PE/Mach-O writers。
**Consumes:** Task 1 ABI。
**Produces:** gc_thread_attach()->MutatorContext*、gc_thread_detach(ctx)、gc_enter_native(ctx)、gc_leave_native(ctx)。PortableAsm に acquire load/release store/CAS/fetch_or/fence/TLS/worker-start/park/wake。

- [x] 2 threads の publication と CAS winner をチェックする native harness を作る。
- [ ] Linux は emitted worker startup/stack/TLS/futex、Windows は OS thread/wait API、macOS は OS thread/synchronization API を実装する。
- [ ] Mach-O は現在 external import を前提としないため、必要なら dylib import/binding machinery も writer に追加する。clang/ld を呼ぶ代替は禁止。
- [ ] target ABI の alignment/callee-saved/TLS/worker exit、attach/detach と連続 handshake、startup failure を検証する。
- [ ] Windows / AArch64 の monotonic clock を実装し、既存の timing=false / clock stub を測定対象経路から除去する。経過時間が単調に増える実機テストを追加する。
- [ ] AArch64 実機の publication tests を通す。compiler 内の Rust atomic だけを変えて完了しない。
- [ ] commit: feat: add portable GC concurrency primitives。

### Task 3 — heap・descriptor・TLAB

**Files:** gc_heap.*、gc_codegen/heap.rs、gc_layout.rs、gc_abi.rs。
**Consumes:** thread context、descriptor/phase ABI。
**Produces:** gc_alloc(ctx, desc_id, payload_bytes)->GcRef、gc_publish(ctx, object)、gc_reserve_relocation(bytes)->Result。

- [ ] checked size/alignment、zero size、mixed layout、TLAB rollover、large-run reuse の failing tests を作る。
- [ ] Young allocation と unpublished/black allocation policy を実装する。
- [ ] size/descriptor/object-start metadata と page incarnation を接続する。
- [ ] hard/soft budget、relocation reserve、metadata accounting を追加する。
- [ ] 全 allocation point へ failure を注入し、half-initialized object が公開されないことを確認する。
- [ ] commit: feat: add generational heap allocation and metadata。

### Task 4 — concurrent roots と native boundary

**Files:** gc_roots.*、gc_codegen/roots.rs、direct/LLVM root lowering、gc_roots_test.c。
**Consumes:** GcRef、MutatorContext、page/phase。
**Produces:** gc_root_new(ctx,value)->RootHandle、gc_root_load(ctx,handle)->GcRef、gc_root_store(ctx,handle,value)、gc_root_release(ctx,handle)、gc_poll(ctx)。

- [ ] deep recursion、nested allocation、引数 staging、return、cleanup、static root、blocked I/O のテストを作る。
- [ ] root cells/segmented scan/retirement を実装する。phase 境界に全 root traversal を入れない。
- [ ] root overwrite/clear と scanner、frame pop と scanner、detach と buffer flush を強制競合させる。
- [ ] raw read/write から暗黙 attach を除去し、unattached access を検証 build で失敗させる。
- [ ] RootHandle の release 後・reuse 前の寿命と global handle を検証する。
- [ ] commit: feat: add concurrent roots and native transitions。

### Task 5 — load/store/CAS barriers

**Files:** gc_barrier.*、gc_codegen/barrier.rs、direct funnels、LLVM IR、gc_barrier_test.c。
**Consumes:** FieldLoc、RootHandle、PhaseSnapshot、buffer interface。
**Produces:** gc_load_ref(ctx,loc)->GcRef、gc_store_ref(ctx,loc,value)、gc_compare_exchange_ref(ctx,loc,expected,desired)->CasResult。

- [ ] healer が A を読んだ後に mutator が B を store し、heal が B を消さないテストを作る。
- [ ] null overwrite、previous null、same-value store、CAS success/failure、bulk copy をテストする。
- [ ] Old.slot=null → Young A → null → remset flip → Young B の順で書き、B がその slot だけから生存するケースを固定する。
- [ ] SATB/remset recording と store-good publication、fast/slow path を実装する。
- [ ] scalar field access も receiver canonicalization を通す。
- [ ] LLVM shared reference/phase load の atomic IR を確認し、unsafe な readnone/readonly 属性を付けない。
- [ ] 色だけ違う同じ reference の CAS が正しく比較されることを検証する。
- [ ] commit: feat: implement generational reference barriers。

### Task 6 — SATB marking と termination

**Files:** gc_mark.*、gc_codegen/mark.rs、model、runtime tests。
**Consumes:** roots、barriers、work buffer、PhaseSnapshot。
**Produces:** gc_request_cycle(kind)->CycleTicket、gc_await_cycle(ctx,ticket)。kind は Young/Major。

- [ ] 最後の edge の削除、black object 初期化、queue empty 直後の enqueue を再現する。
- [ ] private/shared queues、bounded flush、work stealing、in-flight accounting を実装する。
- [ ] MarkEndCheck が新しい仕事を見つけたら concurrent phase に戻す。
- [ ] 大きな pointer array を slot/byte budget で分割する。
- [ ] collect は要求前から動いていた cycle が終わっただけで充足扱いにしない。CycleTicket で要求時点を表す。
- [ ] commit: feat: add concurrent generational marking。

### Task 7 — remembered sets・young-only cycle・promotion

**Files:** gc_remembered.*、gc_codegen/remembered.rs、gc_generational_test.c。
**Consumes:** SATB/marking、FieldLoc、epoch handoff。
**Produces:** remember(ctx,loc,previous,epoch)、flip_remembered(epoch)、promote(object,destination_generation)。

- [ ] Old → Young だけで生きる object、old array/closure、繰り返し minor のテストを作る。
- [ ] local buffer にだけ残った entry を置き、flip しても取りこぼさないことを検証する。
- [ ] enqueue → caller root の overwrite/release/reuse → 元 owner の移動 → flush を固定し、元 field が記録されることを確認する。
- [ ] old owner relocation と slot remap、promotion と Young child、null field の初回 store を試す。
- [ ] minor の Old scanned bytes が Old heap size に比例しないことを counter で確認する。
- [ ] dead owner の metadata が回収され、反復 cycle で retention が増え続けないことを確認する。
- [ ] commit: feat: add remembered sets and young collection。

### Task 8 — concurrent relocation・failure・retirement

**Files:** gc_relocate.*、gc_codegen/relocate.rs、gc_relocation_test.c。
**Consumes:** canonical access、page incarnation、forwarding、phase transition。
**Produces:** gc_resolve(ctx,reference)->GcRef、gc_relocate_object(ctx,reference)->GcRef、gc_retire_page(page_id,incarnation,epoch)。

- [ ] 同一 object を2 actors が動かす順序を固定し、destination/identity が一つになることを検証する。
- [ ] field store と move、nested forwarding、heal failure、遅延 reader と source reclaim を試す。
- [ ] copy publication、loser reclaim、source retention、forwarding retirement を実装する。
- [ ] to-space failure を全段階に注入し、partial relocation でも heap が valid なことを確かめる。
- [ ] page protection/quarantine で論理解放後のアクセスを検出する。通常 ASan だけで managed-heap UAF 検査済みとしない。
- [ ] commit: feat: add concurrent relocation and page retirement。

### Task 9 — Old mark と Young collection の調停

**Files:** generation state、scheduler、mark/relocate/remset、model。
**Consumes:** 各世代の mark/relocate と phase publication。
**Produces:** Young/Old 別 GenerationState、CycleCoordinator。

- [ ] Old queue に Young reference を残し、Young を移動してから Old mark を継続する。
- [ ] promotion during Old mark、old SATB 中の移動済み参照、double-remap-bad reference をテストする。
- [ ] phase の短い排他を実装し、全 cycle を巨大 lock で直列化しない。
- [ ] Young 反復による Old starvation と Old relocation による Young starvation を試す。
- [ ] G01–G09 を model/runtime 両方で確認する。二つの heap を作っただけで完了しない。
- [ ] commit: feat: coordinate young cycles with old marking。

### Task 10 — weak/native handles と scoped pin

**Files:** roots/barrier/page selection、gc_roots_test.c。
**Consumes:** mark termination、root barrier、page lifecycle。
**Produces:** gc_weak_new(ctx,value)->WeakHandle、gc_weak_load(ctx,handle)->Option<GcRef>、gc_weak_release(ctx,handle)、gc_pin(ctx,root)->PinToken、gc_unpin(ctx,token)。

- [ ] strong root 消失後の weak clear、weak load と termination の競合を固定する。
- [ ] weak load で strong に戻る場合の keep-alive と返値 rooting を実装する。
- [ ] pin 中の selection、unpin 後の回収/移動、native callback をテストする。
- [ ] selection 済み object への pin と、pin 中に始まる selection の両方で、返る address が canonical/pinned であることを確認する。
- [ ] エラー/cleanup/blocked I/O でも pin と handle が漏れないことを確認する。
- [ ] commit: feat: add GC handles and scoped pinning。

### Task 11 — adaptive scheduler・OOM・uncommit

**Files:** gc_scheduler.*、gc_heap.*、CLI、gc_oom_test.c。
**Consumes:** generations、stats、headroom、worker lifecycle。
**Produces:** GcConfig、GcStatsSnapshot、schedule_next_cycle(stats,config)->CycleDecision。

- [ ] sustained allocation、live-set 急増、全部生存、soft-max 超過、metadata OOM、worker failure を試す。
- [ ] stall の wait/wake と poll/blocked state を接続し、deadlock しないことを確認する。
- [ ] age/survival による tenuring、generation capacity、worker 数の調整を実装する。
- [ ] uncommit/recommit failure、min=max、pre-touch、large-pages=required 失敗を検証する。
- [ ] commit: feat: add GC ergonomics and pressure handling。

### Task 12 — direct 三ターゲットと LLVM の統合

**Files:** compiler funnels、writers、CLI、cross-exec、CI。
**Consumes:** 共通 runtime ABI / GcConfig。
**Produces:** default Generational native build と明示 --gc=generational。

- [ ] inventory と照合し、enum/closure/list/map/string/equality/literal roots の経路を確認する。
- [ ] LLVM の現在対応する subset を新 C ABI に移す。未対応構文の診断を維持する。
- [ ] Linux で cross-build した PE/Mach-O を対象 OS 実機で実行する。
- [ ] 生成時に clang/cc/as/ld/link.exe が使えない環境でも direct build が成功することを確認する。
- [ ] stress+poison+verify を三 OS に広げ、default を切り替える。flag を無視する backend を残さない。
- [ ] commit: feat: enable generational GC across native targets。

### Task 13 — 計測・性能・最終監査

**Files:** stats/verifier、tests/gc_latency.rs、benchmark scripts、CI/docs。
**Consumes:** 全 capabilities。
**Produces:** versioned JSON events、coverage と測定 report。

- [ ] JSON に cycle_id/generation/phase/timestamp_ns と §11 の指標を出す。
- [ ] §10 の failure matrix と §11 の performance matrix を実行する。
- [ ] runtime、compiler roots、OS adapters の独立 review を受ける。
- [ ] fast-path disassembly に global lock / allocation がないことを確認する。
- [ ] G01–G15 ごとの実装・テスト・実測を対応付け、stale docs を更新する。
- [ ] commit: test: verify generational GC correctness and latency。

依存順は 0 → 1 → 2/3 → 4 → 5 → 6 → 7 → 8 → 9 → 10/11 → 12 → 13。Task 1 の context layout 固定後、Task 3 の heap 単体試験では明示した context stub を使うことで2/3を並行担当できる。後半の10/11も独立に担当できる。将来 Task の機能が必要な中間テストには明示した model stub を使い、stub の通過を runtime 完成扱いにしない。

GcError は InvalidSize / InvalidReference / OutOfMemory / UnsupportedConfig を最低限含む。CasResult は success と observed reference。GcConfig は §7 の設定、GenerationState は §5.3 の状態/epoch、CycleTicket は要求時点と completion を表す。宣言だけで利用されない interface を残さない。

## 10. correctness 検証

### 10.1 既存コマンド

~~~bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo build -p klassic-runtime
cargo test --workspace
runtime/gc/run_tests.sh
cargo test -p klassic-native
cargo test --test cli_smoke native_gc_
cargo test --test cli_smoke native_pointer_arg_staging
cargo test --test cli_smoke windows_gc_stress_and_poison_stay_correct
cargo test --test cli_smoke build_target_aarch64_apple_darwin_gc_
cargo test --test llvm_gc
cargo test --test llvm_gc_link
cargo test --test llvm_backend
~~~

各コマンドの exit 0 と実行件数を記録する。host-gated test は対象 runner で実施し、0 tests を成功の証拠にしない。LLVM/C 必須 CI は clang を用意し、未検出を skip-success にしない。

cargo build -p klassic-runtime は C-backend link tests に必要な static library を作るための既存 CI 前提。cargo test だけで libklassic_runtime.a が生成されるとは仮定しない。

新しい Rust integration tests を作成後は、cargo test --test gc_abi、--test gc_generational、--test gc_differential を必須とする。gc_latency は dedicated machine の測定 runner で実行する。追加する C suite は run_tests.sh に登録し、各 suite の実行・失敗数を表示する。

既存 run_tests.sh の setarch は存在だけでなく使用可否を確認する。sanitizer runtime の起動失敗と collector の不具合を分け、試験不能を成功として隠さない。手書き LLVM link fixture の plain field load も現行 emitter の atomic IR と合わせる。

### 10.2 必須の failure matrix

| 検査 | PASS 条件 |
|---|---|
| deterministic model | 探索した schedule 全てで reachability/identity/store preservation が成立 |
| C ASan+UBSan | single/multi-mutator、全追加 suite の終了0、verifier error0 |
| C TSan | separate build で race0。既知 race の suppression で通さない |
| emitted-code contention | shared slot / moving graph の値・identity・ObjectId が一致 |
| differential | evaluator/native の stdout、exit、言語上の error が一致 |
| poison/stress/verify | 単独と組合せ、三 backend、複数 cycle |
| logical heap verifier | reference が object start / valid forwarding を指し、dead page を参照しない |
| page reuse / epoch wrap | 同アドレスを再利用しても古い reference が新 object に化けない |
| OOM subprocess | 制御された診断と非ゼロ終了。timeout/SIGSEGV/silent success なし |
| concurrent progress | background phase 中に mutator が進み、mutator/worker が starvation しない |
| large-object churn | freed run を再利用し、最大 heap 未満の live set で tail exhaustion しない |
| blocking/native | 長いI/O、attach/detach、callback が GC を永久に止めない |

C runtime の sanitizer と、emitted runtime の deterministic harness / instruction tests / verifier / 実機を組み合わせる。C が通っただけで機械語版の atomic encoding を検証したことにしない。

## 11. 低停止時間の測定

以下は **新実装の合格目標**であり、baseline の実測結果ではない。OpenJDK の性能を自動的に継承するとは扱わない。

別々に測定する指標:

- handshake request → 全対象が safe state に至る time_to_safepoint_ns。
- 各 mutator の park → resume、global stopped interval。
- allocation stall、barrier slow path、mutator assist、page fault、cycle 全体。
- Young/Old scanned bytes、remset visited slots、copied/promoted/live bytes。
- throughput、GC CPU、barrier hit/slow rate、reserved/committed/resident/metadata bytes。

**構造上の合格条件:** global stop に全 heap / 全 roots の走査、無制限 drain、relocation-set 全コピーを置かない。thread 数に依存する bounded rendezvous と、heap/live/stack size に依存する traversal を区別する。

**性能合格目標:** dedicated reference machines 上、十分な headroom のある定常負荷で global pause p99.9 ≤ 1 ms。最大値、time-to-safepoint、各 mutator park、allocation stall も必ず併記する。未達なら機能実装済みと latency 未達を区別し、full release 完了とは書かない。共有 CI に1msの hard assertion は置かない。

| 軸 | 比較 |
|---|---|
| heap max | 32 MiB / 256 MiB / 2 GiB |
| live set | 小/中/高 occupancy。OOM stress は別枠 |
| mutators | 1 / 2 / 8 |
| roots/stack | 同じ heap で root 数・recursion depth を増加 |
| graph | 短命、長寿命、Old→Young 多数、共有 DAG、cycles |
| allocation | 小 record、大 pointer array、large strings、large-object churn |
| phase overlap | Old mark 中の複数 Young cycle、promotion burst |
| platform | Linux x86-64 / Windows x86-64 / macOS AArch64 |

warmup 後に測定し、p99.9 を出す負荷は少なくとも10,000 pause samplesを目標にする。件数不足なら sample count と限界を記す。throughput は同じ compiler の legacy GC と比較し、slowdown を隠さない。

既存 native_gc_pause_stays_bounded の約250ms閾値は回帰検査として維持できるが、Generational ZGC の低遅延・heap-size independence の証拠にはしない。

## 12. 完了報告

- [ ] G01–G15 全てに実装パス、テスト名、実行結果が対応する。
- [ ] default direct ELF/PE/Mach-O が Generational collector を使用する。
- [ ] young-only、old cycle、old-mark/young overlap の実行証拠がある。
- [ ] store/promotion/remset flip/weak load/move の競合試験がある。
- [ ] O(heap/roots/residual graph) の停止処理が除去されている。
- [ ] C/LLVM/x86/ARM の memory-ordering review を完了した。
- [ ] allocation/relocation/metadata OOM、large-run reuse を検証した。
- [ ] 必須 sanitizer/verifier/cross-exec を skip していない。
- [ ] latency/stall/throughput/memory と環境を記録した。
- [ ] 古い backend 説明と完了主張を更新した。

最終報告には実装 commit、変更点、コマンドと実行件数、backend matrix、performance table、未達項目を記載する。単世代や一部 backend だけなら中間到達点と明示する。別の指示がない限り、指示書を読んだだけで push/merge/release はしない。

## 13. 参照資料

### Klassic — 調査した固定 baseline

- [repository](https://github.com/klassic/klassic/tree/9346bff7ecb9a29e26efed17069567ba29cc91c1)
- [portable_asm.rs](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/crates/klassic-native/src/portable_asm.rs)
- [x86 integration](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/crates/klassic-native/src/lib.rs)
- [AArch64 integration](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/crates/klassic-native/src/aarch64.rs)
- [C runtime](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/runtime/gc/klassic_gc.c) / [C ABI](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/runtime/gc/klassic_gc.h)
- [LLVM emitter](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/crates/klassic-native/src/llvm.rs)
- [native backend strategy](https://github.com/klassic/klassic/blob/9346bff7ecb9a29e26efed17069567ba29cc91c1/docs/native-backend-strategy.md)

### OpenJDK — jdk-25+36 の固定 source

設計を参照して Klassic の ABI に合わせて実装する。ソースをコピーする場合は各ファイルの license/header を確認し、由来を残す。C++ を機械翻訳しただけで正しさを主張しない。

| source | 確認事項 |
|---|---|
| [zAddress.hpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zAddress.hpp) | 世代別metadata、remap組合せ、CPU差 |
| [zBarrierSet.inline.hpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zBarrierSet.inline.hpp) | pre-store、CAS、bulk/native access |
| [zBarrier.inline.hpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zBarrier.inline.hpp) | previous value、double remap、promotion/null |
| [zBarrier.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zBarrier.cpp) | mark/remember slow path |
| [zRemembered.hpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zRemembered.hpp) | slot位置の記録、flip、owner移動 |
| [zRememberedSet.inline.hpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zRememberedSet.inline.hpp) | 二重bitmap、slot粒度、atomic更新 |
| [zStoreBarrierBuffer.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zStoreBarrierBuffer.cpp) | 古いepochのbufferと移動するslot |
| [zGeneration.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zGeneration.cpp) | phase、termination retry、世代間調停 |
| [zRelocate.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zRelocate.cpp) | copy勝者、loser reclaim、retained pages |
| [zForwarding.inline.hpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zForwarding.inline.hpp) | release/acquireとpage保持 |
| [zStackWatermark.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zStackWatermark.cpp) | root/epoch/frame処理 |
| [zDirector.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zDirector.cpp) | 開始時期・allocation予測 |
| [zPageAllocator.cpp](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zPageAllocator.cpp) | stall/capacity/uncommit |

背景資料: [JEP 333](https://openjdk.org/jeps/333)、[376](https://openjdk.org/jeps/376)、[439](https://openjdk.org/jeps/439)、[474](https://openjdk.org/jeps/474)、[490](https://openjdk.org/jeps/490)。

Generational の設計意図は JEP 439、全 stack を停止中に処理しない方針は JEP 376 を読む。今回の実装詳細の調査根拠は固定 source。JEP ページの本文取得はこの環境で403だったため、JEP の文言を読了済みとして引用していない。

[Oracle JDK 24 ZGC tuning guide](https://docs.oracle.com/en/java/javase/24/gctuning/z-garbage-collector.html) は soft max / uncommit / pre-touch / large pages の運用参考。記載されている性能を Klassic の実測値に転記しない。

---

**実装担当への最終指示:** generation bit の追加では終わらない。mutator・marker・relocator・root scanner が同時に進んでも参照の意味が一致することを、phase table、反例テスト、実測で示す。G01–G15 と測定条件を満たして完成とする。
