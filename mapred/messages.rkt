#lang typed/racket/base

(provide
 (struct-out MRInit)
 (struct-out StartMapPhase)
 (struct-out StartReducePhase)
 (struct-out TaskMsg)
 (struct-out MapTaskReqResp))

(require
 (only-in "types.rkt"
          DynFn Block
          Mapper Writer Sorter Grouper))

(struct: TaskMsg () #:prefab)

(struct: MapTaskReqResp TaskMsg ([loc : Path]
                                 [sod : Natural]
                                 [eod : Natural]) #:prefab)

(struct: BlockMap ([src : Block]
                   [dest : (Listof Block)]))

(struct: (A B) MRInit TaskMsg ([parser  : DynFn]
                               [mapper  : DynFn]
                               [writer  : DynFn]
                               [sorter  : DynFn]
                               [grouper : DynFn]
                               [partitions : Index]) #:prefab)

(struct: StartMapPhase    TaskMsg ([partition-count : Index]) #:prefab)
(struct: StartReducePhase TaskMsg () #:prefab)

