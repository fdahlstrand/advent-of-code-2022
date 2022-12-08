open System

let scan state height =
    if (height > fst state) then
        (height, [| yield! snd state; true |])
    else
        (fst state, [| yield! snd state; false |])

let scanBack height state =
    if (height > fst state) then
        (height, [| true; yield! snd state |])
    else
        (fst state, [| false; yield! snd state |])


let rowFromString =
    Seq.map string
    >> Seq.map Int32.Parse
    >> Seq.toArray


let scanLeft arr = Array.fold scan (-1, [||]) arr |> snd

let scanRight arr =
    Array.foldBack scanBack arr (-1, [||]) |> snd

let fromArrayOfRowArrays arr =
    let rows = Array.length arr
    let cols = Array.length arr[0]

    Array2D.init rows cols (fun i j -> arr.[i].[j])

let fromArrayOfColArrays arr =
    let cols = Array.length arr
    let rows = Array.length arr[0]

    Array2D.init rows cols (fun i j -> arr.[j].[i])


module HeightMap =
    open System.IO

    let fromLines: string seq -> int [,] =
        Seq.map rowFromString
        >> Seq.toArray
        >> fromArrayOfRowArrays

    let fromFile = File.ReadAllLines >> fromLines

    let rows heightMap =
        [| for i in 0 .. Array2D.length1 heightMap - 1 -> heightMap[i, *] |]

    let cols heightMap =
        [| for i in 0 .. Array2D.length2 heightMap - 1 -> heightMap[*, i] |]


let rayCast row col map =
    let rows = Array2D.length1 map
    let cols = Array2D.length2 map
    let height = map[row, col]

    let mutable cost = 0
    let mutable i = row - 1

    while i >= 0 do
        if (map[i, col] < height) then
            cost <- cost + 1
            i <- i + 1
        else
            i <- -1




let heightMap = HeightMap.fromFile "./day8/input.txt"
// seq {
//     "30373"
//     "25512"
//     "65332"
//     "33549"
//     "35390"
// }
// |> HeightMap.fromLines


let leftScan =
    heightMap
    |> HeightMap.rows
    |> Array.map scanLeft
    |> fromArrayOfRowArrays

let rightScan =
    heightMap
    |> HeightMap.rows
    |> Array.map scanRight
    |> fromArrayOfRowArrays

let topScan =
    heightMap
    |> HeightMap.cols
    |> Array.map scanLeft
    |> fromArrayOfColArrays

let bottomScan =
    heightMap
    |> HeightMap.cols
    |> Array.map scanRight
    |> fromArrayOfColArrays



let combine (m1: bool [,]) (m2: bool [,]) =
    Array2D.init (Array2D.length1 m1) (Array2D.length2 m1) (fun i j -> m1[i, j] || m2[i, j])


leftScan
|> combine rightScan
|> combine topScan
|> combine bottomScan
|> HeightMap.rows
|> Array.sumBy (Array.sumBy (fun x -> if x then 1 else 0))
|> printfn "%A"
