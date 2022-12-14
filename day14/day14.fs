﻿type Position = {
    X: int
    Y: int
}

type Path = Position list

type ScanData = Path list

type ScanCell = | Empty | Block | Sand

type Scan = ScanCell[,]

let position x y = { X = x; Y = y }

let blockCell (scan:Scan) x y = 
    scan[x, y] <- Block

let horizontalLine scan p1 p2 =
    let y = p1.Y
    let sx = min p1.X p2.X
    let ex = max p1.X p2.X
    for x in sx..ex do
        blockCell scan x y
    scan

let verticalLine scan p1 p2 =
    let x = p1.X
    let sy = min p1.Y p2.Y
    let ey = max p1.Y p2.Y
    for y in sy..ey do
        blockCell scan x y
    scan
    

let line scan p1 p2 =
    match p1, p2 with
    | a, b when a.X = b.X -> verticalLine scan a b
    | a, b when a.Y = b.Y -> horizontalLine scan a b
    | _ -> failwith "Not a horizontal or vertical line"

let printScan scan startx =
    for y in 0..Array2D.length2 scan - 1 do
        for x in startx..Array2D.length1 scan - 1 do
            let ch = match scan[x,y] with
                     | Empty -> '.'
                     | Block -> '#'
                     | Sand -> 'o'
            printf $"%c{ch}"
        printfn ""

open System.Text.RegularExpressions
let (|Path|_|) input =
    let m = Regex.Match(input, @"^((?<pos>(?<x>[1-9][0-9]*),(?<y>[1-9][0-9]*))(\s->\s)?)*$")
    let toInt = System.Int32.Parse

    if (m.Success) then
        Some
            [ for i in 0..m.Groups["pos"].Captures.Count - 1 do
                let x = m.Groups["x"].Captures[i].Value |> toInt
                let y = m.Groups["y"].Captures[i].Value |> toInt

                yield position x y]
    else
        None

let scan = Array2D.create 505 11 Empty

let pos = 
    [
        (position 498 4)
        (position 498 6)
        (position 496 6)
    ] |> List.pairwise

pos |> List.map (fun (p1, p2) -> line scan p1 p2) |> ignore

let parse str =
    match str with
    | Path p -> p
    | _ -> failwith "Not a valid path"

let sample:ScanData =
    [ "498,4 -> 498,6 -> 496,6"
      "503,4 -> 502,4 -> 502,9 -> 494,9" ] |> List.map parse
 

sample |> List.map (fun s -> List.pairwise s |> List.map (fun (p1, p2) -> line scan p1 p2) |> ignore) |> ignore


printScan scan 493
