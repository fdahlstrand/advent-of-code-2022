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

// let heightMap =
//     seq {
//         "30373"
//         "25512"
//         "65332"
//         "33549"
//         "35390"
//     }
//     |> HeightMap.fromLines

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
