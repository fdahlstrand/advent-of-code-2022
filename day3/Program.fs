open System.IO

module Bag =
    let parse (bagString: string) =
        let stringToSet (str: string) =
            (Set.empty<char>, str) ||> Seq.fold (fun s -> s.Add)
            
        let midpoint = bagString.Length/2
        (stringToSet bagString[..midpoint-1], stringToSet bagString[midpoint..])

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
|> printfn "%A"