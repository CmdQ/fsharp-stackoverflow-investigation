module GeneticLbp.Program

[<EntryPoint>]
let main _ =
    let r = KissRandom()
    let n = r.Normal()
    Seq.item 20000 n |> printfn "%f"
    0