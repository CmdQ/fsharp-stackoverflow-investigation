namespace GeneticLbp

open System

type KissRandom(?x, ?y, ?z, ?c) =
    let mutable x = defaultArg x 123456789u
    let mutable y = defaultArg y 362436000u
    let mutable z = defaultArg z 521288629u
    let mutable c = defaultArg c 7654321u
    let mutable a = 698769069uL

    let next () =
        x <- 69069u * x + 12345u
        y <- y ^^^ (y <<< 13)
        y <- y ^^^ (y >>> 17)
        y <- y ^^^ (y <<< 5)
        let t = a * uint64 z + uint64 c
        c <- t >>> 32 |> uint32
        z <- uint32 t
        x + y + z

    let nextSingle =
        let oneOverMax = 1.f / float32 UInt32.MaxValue
        fun () ->
            float32 (next()) * oneOverMax

    let gauss mean stdDev =
        let rec gauss ready = seq {
            match ready with
            | Some spare ->
                yield spare * stdDev + mean
                yield! gauss None
            | _ ->
                let rec loop () =
                    let u = nextSingle() * 2.f - 1.f
                    let v = nextSingle() * 2.f - 1.f
                    let s = pown u 2 + pown v 2
                    if s >= 1.f || s = 0.f then loop() else
                    u, v, s
                let u, v, s = loop()
                let mul = (*)(sqrt(-2.f * log s / s))
                yield mul u * stdDev + mean
                yield! gauss <| Some(mul v)
        }
        gauss None

    member __.Next() = next()

    member __.Next(maxValue) =
        if maxValue = 0u then 0u else
        // Fight modulo bias!
        // From: http://funloop.org/post/2015-02-27-removing-modulo-bias-redux.html
        let threshold = (UInt32.MaxValue - maxValue) % maxValue
        let rec loop () =
            let rand = next()
            if rand < threshold then loop() else
            rand % maxValue
        loop()

    member me.Next(maxValue:int) = maxValue |> uint32 |> me.Next |> int

    member __.NextSingle() = nextSingle()

    member __.Normal(?mu, ?sigma) =
        let mu = defaultArg mu 0.f
        let sigma = defaultArg sigma 1.f
        gauss mu sigma
