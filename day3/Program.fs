open System.IO

type InventoryList = string list
type Item = char

module InventoryList =
    let fromFile (path: string): InventoryList = File.ReadLines(path) |> List.ofSeq
   
    let private findCommonItem (bag: string) =
        (bag
         |> Seq.chunkBySize (bag.Length / 2)
         |> Seq.map Set.ofArray
         |> Set.intersectMany
         |> Set.toArray)[0]

    let findCommonItemPerBag (list: InventoryList): Item list =
        list |> List.map findCommonItem
        
    let collectGroupsBySize (groupSize: int) (list: InventoryList): InventoryList list =
        list |> List.chunkBySize groupSize
        
    let findCommonBadgeItemType (list: InventoryList) =
        (list |> List.map Set.ofSeq |> Set.intersectMany |> Set.toArray)[0]

let calculatePriority (ch: char) =
    match ch with
    | c when 'a' <= c && c <= 'z' -> int c - int 'a' + 1
    | c when 'A' <= c && c <= 'Z' -> int c - int 'A' + 27
    | _ -> failwithf $"Unsupported bag item '%c{ch}'"

let findCommonBadgeItemType (group: Set<char> []) =
    (group |> Set.intersectMany |> Set.toArray)[0]

InventoryList.fromFile("./day3/input.txt")
|> InventoryList.findCommonItemPerBag
|> List.sumBy calculatePriority
|> printfn "Total priority of bag items %d"

InventoryList.fromFile("./day3/input.txt")
|> InventoryList.collectGroupsBySize 3
|> List.map InventoryList.findCommonBadgeItemType
|> List.sumBy calculatePriority
|> printfn "Total priority of badge item types %d"