type Position = {
    X: int
    Y: int
}

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

let scan = Array2D.create 500 10 Empty

let pos = [
    (position 498 4), (position 498 6)
    (position 498 6), (position 496 6)
]

pos |> List.map (fun (p1, p2) -> line scan p1 p2) |> ignore

printScan scan 490