type Coordinate = int * int
type Rock = Coordinate list

let rocks: Rock[] =
    [| [ (0, 0); (1, 0); (2, 0); (3, 0) ]
       [ (1, 0); (0, 1); (1, 1); (2, 1); (1, 2) ] |]

let rockGenerator = Seq.initInfinite (fun i -> rocks[i % rocks.Length])

let push (dx: int) (rock: Rock) : Rock =
    rock |> List.map (fun (x, y) -> (x + dx, y))

let pushLeft = push -1
let pushRight = push 1

let isValid (rock: Rock) : bool =
    let isFree (pos: Coordinate) : bool =
        match pos with
        | (x, _) when 0 <= x && x <= 6 -> true
        | _ -> false

    rock |> List.map isFree |> List.reduce (&&)

let isBlocked (rock: Rock) : bool =
    let isPosBlocked (pos: Coordinate) : bool =
        match pos with
        | _, y when y <= 0 -> true
        | _ -> false

    rock |> List.map isPosBlocked |> List.reduce (||)

let fall (rock: Rock) : Rock option =
    let movedRock = rock |> List.map (fun (x, y) -> (x, y - 1))

    match isBlocked movedRock with
    | true -> None
    | false -> Some movedRock

let pop source = (Seq.head source, Seq.skip 1 source)

let initRock (h: int) (rock: Rock) : Rock =
    rock |> List.map (fun (x, y) -> (x + 2, y + h + 3))

let step (rock: Rock) : (Rock * Rock) option =
    match fall rock with
    | Some r -> Some(r, r)
    | None -> None


let simulateRock (h: int) (rock: Rock) : int =
    let newRock = rock |> initRock h

    newRock |> List.unfold step |> List.last |> List.map snd |> List.max |> max h


rockGenerator |> Seq.take 1 |> Seq.fold simulateRock 0 |> printfn "%A"
