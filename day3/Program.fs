open System.IO

let stringToSet (str: string) =
    (Set.empty<char>, str) ||> Seq.fold (fun s -> s.Add)

module Bag =
    let parse (str: string) =
            
        let midpoint = str.Length/2
        (stringToSet str[..midpoint-1], stringToSet str[midpoint..])

let calculatePriority (ch:char) =
    match ch with
    | c when 'a' <= c && c <= 'z' -> int c - int 'a' + 1
    | c when 'A' <= c && c <= 'Z' -> int c - int 'A' + 27
    | _ -> failwithf $"Unsupported bag item '%c{ch}'"

File.ReadLines("./day3/input.txt")
|> Seq.map (
    fun str -> str
               |> Bag.parse
               ||> Set.intersect
               |> Set.map calculatePriority
               |> Set.toList
               |> List.sum)
|> Seq.sum
|> printfn "Total priority of bag items %d"

File.ReadLines("./day3/input.txt")
|> Seq.map stringToSet
|> Seq.chunkBySize 3
|> Seq.map (fun group -> group
                         |> Set.intersectMany
                         |> Set.map calculatePriority
                         |> Set.toList
                         |> List.sum )
|> Seq.sum
|> printfn "Total priority of badge item types %d"