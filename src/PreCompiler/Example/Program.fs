// Learn more about F# at http://fsharp.org

open System
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

[<AutoOpen>]
module ProgramAdaptor = 
    /// Adaptive representation for `Value`
    type AdaptiveValue private(__initial : Value) =
        let _value = cval(__initial.value)
        /// The current value of value as `aval<int>`.
        member __.value = _value :> aval<_>
        /// Updates all values in the `AdaptiveValue` to the given `Value`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : Value) : unit =
            let __value = value
            _value.Value <- __value.value
        /// Creates a new `AdaptiveValue` using the given `Value`.
        static member create(value : Value) : AdaptiveValue =
            AdaptiveValue(value)
    /// Adaptive representation for `Record`
    type AdaptiveRecord private(__initial : Record) =
        let _foo = clist(__initial.foo)
        let _bar = clist(__initial.bar |> IndexList.map (fun v -> AdaptiveValue.create(v)))
        /// The current value of foo as `alist<int>`.
        member __.foo = _foo :> alist<_>
        /// The current value of bar as `amap<_>`.
        member __.bar = _bar :> alist<_>
        /// Updates all values in the `AdaptiveRecord` to the given `Record`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : Record) : unit =
            let __value = value
            _foo.Value <- __value.foo
            let updateValue t v =
                let t = unbox<AdaptiveValue> t
                t.update(v); t
            let merge k (o : option<_>) (n : option<_>) =
                match o with
                | Some t ->
                    match n with
                    | Some v -> updateValue t v |> Some
                    | None -> None
                | None ->
                    match n with
                    | Some v -> Some ((fun v -> AdaptiveValue.create(v)) v)
                    | None -> None
            _bar.Value <- IndexList.choose2 merge _bar.Value __value.bar
        /// Creates a new `AdaptiveRecord` using the given `Record`.
        static member create(value : Record) : AdaptiveRecord =
            AdaptiveRecord(value)

[<EntryPoint>]
let main argv =
    let test = AdaptiveRecord.create { foo = IndexList.single 10; bar = IndexList.empty }

    let _ : alist<int> = test.foo
    let _ : alist<AdaptiveValue> = test.bar



    0
