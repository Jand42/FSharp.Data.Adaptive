namespace SomeNamespace 

open FSharp.Data.Adaptive

[<ModelType>]
type Value =
    { value : int }

[<ModelType>]
type Record =
    {
        foo : IndexList<int>
        bar : IndexList<Value>
    }


[<ModelType>]
type SimpleUnion =
    | CaseA of value : int
    | CaseB of float : float * int : IndexList<Value>
