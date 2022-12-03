open System.IO

let calculatePriority (ch: char) =
    match ch with
    | c when 'a' <= c && c <= 'z' -> int c - int 'a' + 1
    | c when 'A' <= c && c <= 'Z' -> int c - int 'A' + 27
    | _ -> failwithf $"Unsupported bag item '%c{ch}'"

let findCommonItem (bag: string) =
    (bag
     |> Seq.chunkBySize (bag.Length / 2)
     |> Seq.map Set.ofArray
     |> Set.intersectMany
     |> Set.toArray)[0]

let findCommonBadgeItemType (group: Set<char> []) =
    (group |> Set.intersectMany |> Set.toArray)[0]

let collectGroupsBySize (groupSize: int) (list: string seq) =
    list |> Seq.map Set.ofSeq |> Seq.chunkBySize groupSize

File.ReadLines("./day3/input.txt")
|> Seq.map findCommonItem
|> Seq.map calculatePriority
|> Seq.sum
|> printfn "Total priority of bag items %d"

File.ReadLines("./day3/input.txt")
|> collectGroupsBySize 3
|> Seq.map findCommonBadgeItemType
|> Seq.map calculatePriority
|> Seq.sum
|> printfn "Total priority of badge item types %d"