open System

let rowFromString = Seq.map string >> Seq.map Int32.Parse >> Seq.toArray

let fromArrayOfColArrays arr =
    let cols = Array.length arr
    let rows = Array.length arr[0]

    Array2D.init rows cols (fun i j -> arr.[j].[i])

module HeightMap =
    open System.IO

    let fromLines: string seq -> int [,] =
        Seq.map rowFromString >> Seq.toArray >> array2D

    let fromFile = File.ReadAllLines >> fromLines

type HeightMap = int [,]

let heightMap = HeightMap.fromFile "./day8/input.txt"


let treeVisible H h =
    if (H >= h) then (false, H) else (true, h)

let treeVisibleRev h H =
    if (H >= h) then (false, H) else (true, h)

let combine (m1: bool [,]) (m2: bool [,]) =
    Array2D.init (Array2D.length1 m1) (Array2D.length2 m1) (fun i j -> m1[i, j] || m2[i, j])


let rows = [| 0 .. Array2D.length1 heightMap - 1 |]
let cols = [| 0 .. Array2D.length2 heightMap - 1 |]

let west =
    rows
    |> Array.map (fun row -> (-1, heightMap[row, *]) ||> Array.mapFold treeVisible |> fst)
    |> array2D

let east =
    rows
    |> Array.map (fun row -> (heightMap[row, *], -1) ||> Array.mapFoldBack treeVisibleRev |> fst)
    |> array2D

let north =
    cols
    |> Array.map (fun col -> (-1, heightMap[*, col]) ||> Array.mapFold treeVisible |> fst)
    |> fromArrayOfColArrays

let south =
    cols
    |> Array.map (fun col -> (heightMap[*, col], -1) ||> Array.mapFoldBack treeVisibleRev |> fst)
    |> fromArrayOfColArrays

let visibleTrees = west |> combine east |> combine north |> combine south

rows
|> Array.map (fun r -> visibleTrees[r, *] |> Array.sumBy (fun visible -> if visible then 1 else 0))
|> Array.sum
|> printfn "Number of visible trees: %A"

let takeUnblocked H (rRange,cRange) map =
    seq {
        let mutable blocked = false
        for r in rRange do
            for c in cRange do
            if not blocked then
                if heightMap[r, c] < H then
                    yield 1
                else
                    blocked <- true
                    yield 1
    }


let eastFrom (map: HeightMap) r c = (map[r,c], ([r..r],[c + 1 .. Array2D.length2 map - 1]), map)
let westFrom (map: HeightMap) r c = (map[r,c], ([r..r],[c - 1 .. -1 .. 0]), map)
let northFrom (map: HeightMap) r c = (map[r,c], ([r - 1 .. -1 ..0],[c .. c]), map)
let southFrom (map: HeightMap) r c = (map[r,c], ([r + 1 .. Array2D.length1 map - 1],[c .. c]), map)

let countEast = eastFrom heightMap
let countWest = westFrom heightMap
let countNorth = northFrom heightMap
let countSouth = southFrom heightMap
let score r c =
    let scores = [
        countEast r c |||> takeUnblocked |> Seq.length
        countWest r c |||> takeUnblocked |> Seq.length
        countSouth r c |||> takeUnblocked |> Seq.length
        countNorth r c |||> takeUnblocked |> Seq.length ]
    List.fold (fun acc s -> acc * s) 1 scores
                      
let scores = heightMap |> Array2D.mapi (fun i j _ -> score i j)
rows |> Array.map (fun r -> scores[r,*] |> Array.max ) |> Array.max |> printfn "Largest scenic score is %A"