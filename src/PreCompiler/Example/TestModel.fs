module SomeNamespace 

open FSharp.Data.Adaptive

[<ModelType>]
type Value =
    { value : int }

[<ModelType>]
type SimpleUnion =
    | CaseA of value : int
    | CaseB of float : float * int : IndexList<Value>
    | CaseC

[<ModelType>]
type Record =
    {
        opt : Option<HashSet<int>>
        foo : IndexList<int>
        bar : IndexList<Value>
        un : SimpleUnion
        list : IndexList<SimpleUnion>
    }

