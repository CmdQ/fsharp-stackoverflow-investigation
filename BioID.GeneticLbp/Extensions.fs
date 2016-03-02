[<AutoOpen>]
module GeneticLbp.Extensions

open Biometrics
open System

module PArray = Array.Parallel

type LbpArea with
    /// Used to glue chromosomes together.
    static member Splitter = ";"

    /// Serialize a genome array to a string.
    static member SerializeGenome (genome:LbpArea[]) = genome |> Array.map string |> String.concat LbpArea.Splitter

    /// De-serialize a genome string to an array of parameter areas.
    static member GenomeToArray (splitArray:char[]) (Genome gen) =
        gen.Split(splitArray, StringSplitOptions.RemoveEmptyEntries)
        |> PArray.choose (fun gen ->
            match LbpArea.TryParse(gen) with
            | true, area when area.IsValid -> Some area
            | _ -> None
        )
