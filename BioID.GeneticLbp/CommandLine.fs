module GeneticLbp.CommandLine

open System.IO
open System.Reflection

type Options = {
    DataDirectory:string
    Files:string[]
    Quiet:bool
}

let defaultOptions = {
    DataDirectory = "./data"
    Files = Array.empty
    Quiet = false
}

let splitArgs = Array.collect (fun (str:string) -> str.Split([| '=' |], 2)) >> Array.toList

let parse =
    let untilNextDash =
        let rec untilNextDash without withdash = function
            | [] -> without, withdash
            | (x:string)::xs ->
                if x.StartsWith("-") then
                    without, x::xs
                else
                    untilNextDash (x::without) withdash xs
        untilNextDash [] []

    let rec parse acc = function
        | [] -> Choice1Of2 acc
        | "--data"::dir::argv
        | "-d"::dir::argv ->
            argv |> parse { acc with DataDirectory = dir }
        | "--quiet"::argv
        | "-q"::argv ->
            argv |> parse { acc with Quiet = true }
        | "--files"::argv
        | "-f"::argv ->
            let noDash, dash = untilNextDash argv
            dash |> parse { acc with Files = noDash |> List.toArray }
        | _ ->
            Choice2Of2 <| sprintf "USAGE: %s --data data_directory --files genome_dump1.txt genome_dump2.txt --quiet"
                (Path.GetFileName (Assembly.GetExecutingAssembly().Location))

    splitArgs >> parse defaultOptions
