// Learn more about F# at http://fsharp.org

open System
open FSharp.Data.Adaptive
open SomeNamespace

[<EntryPoint>]
let main argv =
    let test = AdaptiveRecord.create { foo = IndexList.single 10; bar = IndexList.empty }

    let _ : alist<int> = test.foo
    let _ : alist<AdaptiveValue> = test.bar

    let test = AdaptiveSimpleUnion.create (CaseA 20)
    match test.GetValue AdaptiveToken.Top with
    | AdaptiveCaseA v -> ()
    | AdaptiveCaseB(a, b) -> ()




    0
