namespace FSharp.Data.Adaptive

[<AutoOpen>]
module WebSharperProxies =
    open WebSharper
    open WebSharper.JavaScript
    
    module Unchecked =
        [<Constant null>]
        let defaultof<'T> = X<'T> 

    module Array =
        let zeroCreate<'T> (i: int) = As<'T[]>(Array(i))

    module Operators =
        [<Constant null>]
        let typeof<'T> = X<System.Type>
