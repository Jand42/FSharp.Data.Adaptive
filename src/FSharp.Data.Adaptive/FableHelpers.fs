namespace FSharp.Data.Adaptive

type UncheckedEqualityComparer<'T> private() =
    static let cmp =
        { new System.Collections.Generic.IEqualityComparer<'T> with
            member __.GetHashCode(o : 'T) = Unchecked.hash o
            member __.Equals(l : 'T, r : 'T) = Unchecked.equals l r
        }
    static member Instance = cmp

type ReferenceEqualityComparer<'T when 'T : not struct> private() =
    static let cmp =
        { new System.Collections.Generic.IEqualityComparer<'T> with
            member __.GetHashCode(o : 'T) = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode o
            member __.Equals(l : 'T, r : 'T) = System.Object.ReferenceEquals(l, r)
        }
    static member Instance = cmp

module UncheckedDictionary =
    let inline create<'Key, 'Value> () =
        System.Collections.Generic.Dictionary<'Key, 'Value>(UncheckedEqualityComparer<'Key>.Instance)

module UncheckedHashSet =
    let inline create<'T> () =
        System.Collections.Generic.HashSet<'T>(UncheckedEqualityComparer<'T>.Instance)


module ReferenceHashSet =
    let inline create<'T when 'T : not struct> () =
        System.Collections.Generic.HashSet<'T>(ReferenceEqualityComparer<'T>.Instance)



#if FABLE_COMPILER
namespace System.Collections.Generic

namespace System

[<AllowNullLiteral>]
type WeakReference<'a when 'a : not struct>(value : 'a) =
    member x.TryGetTarget() =
        (true, value)

namespace System.Threading

type Monitor =
    static member inline Enter (_o : obj) = ()
    static member inline Exit (_o : obj) = ()
    static member inline IsEntered (_o : obj) = true
    static member inline TryEnter (_o : obj) = true

namespace System.Runtime.CompilerServices


[<AutoOpen>]
module private WeakTableHelpers =
    open WebSharper
    open WebSharper.JavaScript

    type [<AllowNullLiteral; AbstractClass; Stub>] WeakMap<'K, 'V>() =
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract set: key: 'K * ?value: 'V -> WeakMap<'K, 'V>
        [<Inline("new WeakMap($0)")>]
        static member Create<'K, 'V> (iterable: seq<'K * 'V>) = X<WeakMap<'K, 'V>>

type ConditionalWeakTable<'K, 'V when 'K : not struct and 'V : not struct>() =
    
    let m = WeakMap.Create<'K, 'V> []

    member x.TryGetValue(key : 'K) =
        if m.has key then (true, m.get key)
        else (false, Unchecked.defaultof<_>)

    member x.Add(key : 'K, value : 'V) =
        m.set(key, value) |> ignore

    member x.Remove(key : 'K) =
        m.delete key



#endif
