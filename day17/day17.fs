type Coordinate = int * int

type RockShape = Shape of (int * int) list

type JetDirection =
    | Left
    | Right

type RoomState =
    { Height: int
      Blocked: bool[,]
      JetClock: int
      JetGenerator: int -> JetDirection
      RockClock: int
      RockGenerator: int -> RockShape }

type Room<'a> = Room of (RoomState -> 'a * RoomState)

type LiveRock =
    | Falling of (int * int) list
    | Resting of (int * int) list

let offsetCoord xoffset yoffset (x, y) = (x + xoffset, y + yoffset)

let makeLiveRock rock =
    let addLiveRock room =
        let startOffset = offsetCoord 2 (room.Height + 3)
        let (Shape shape) = rock
        Falling(shape |> List.map startOffset), room

    Room addLiveRock

let getJetDirection room =
    let dir = room.JetClock |> room.JetGenerator

    dir, { room with JetClock = room.JetClock + 1 }

let getRock room =
    let rock = room.RockClock |> room.RockGenerator

    rock, { room with RockClock = room.RockClock + 1 }

let pushRock dir rock =
    let doPushRock room =
        let pushLeft = offsetCoord -1 0
        let pushRight = offsetCoord 1 0
        let isInRoom (x, _) = 0 <= x && x < 7
        let isAllInRoom = List.map isInRoom >> List.reduce (&&)

        let push =
            match dir with
            | Left -> pushLeft
            | Right -> pushRight

        let pushedRock =
            match rock with
            | Falling r ->
                let nextRock = r |> List.map push

                match isAllInRoom nextRock with
                | true -> Falling r
                | false -> rock
            | _ -> rock

        pushedRock, room

    Room doPushRock

let fallOneUnit rock =
    let doFallOneUnit room =
        let fall = offsetCoord 0 -1
        let isBlocked (x, y) = room.Blocked[x, y]
        let isSomePartBlocked = List.map isBlocked >> List.reduce (||)

        let fallingRock =
            match rock with
            | Falling r ->
                let nextRock = r |> List.map fall

                match isSomePartBlocked nextRock with
                | true -> Resting r
                | false -> Falling nextRock
            | _ -> rock

        fallingRock, room

    Room doFallOneUnit

let putRockToRest rock =
    let doPutRockToRest room =
        let h =
            match rock with
            | Resting r -> r |> List.map snd |> List.max |> max room.Height
            | _ -> room.Height

        match rock with
        | Resting r -> r |> List.iter (fun (x, y) -> room.Blocked[x, y] <- true)
        | _ -> ()

        (), { room with Height = h }

    Room doPutRockToRest

let rocks: RockShape[] =
    [| Shape [ (0, 0); (1, 0); (2, 0); (3, 0) ]
       Shape [ (1, 0); (0, 1); (1, 1); (2, 1); (1, 2) ]
       Shape [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ]
       Shape [ (0, 0); (0, 1); (0, 2); (0, 3) ]
       Shape [ (0, 0); (1, 0); (0, 1); (1, 1) ] |]

let jetPattern = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" |> Seq.map (fun ch -> 
    match ch with
    | '>' -> Right
    | '<' -> Left
    | _ -> failwith $"Unexpected character ('%c{ch}') is stream definition") |> Seq.toArray

let createRoom () =
    { Blocked = Array2D.create 7 10_000 false
      Height = 0
      RockClock = 0
      RockGenerator = fun i -> rocks[i % rocks.Length]
      JetClock = 0
      JetGenerator = fun i -> jetPattern[i % jetPattern.Length] }
















//
// let rockGenerator = Seq.initInfinite (fun i -> rocks[i % rocks.Length])
//
// let jetStream =
//     Seq.initInfinite (fun i ->
//         match jetPattern[i % jetPattern.Length] with
//         | '<' -> Left
//         | '>' -> Right
//         | ch -> failwith $"Unexpected character ('%c{ch}') in jet pattern")
//
// let pushRock (dx: int) (rock: Rock) : Rock =
//     rock |> List.map (fun (x, y) -> (x + dx, y))
//
// let pushLeft = pushRock -1
// let pushRight = pushRock 1
//
// let isBlocked (blocked: bool[,]) (rock: Rock) : bool =
//     let isPosBlocked (blocked: bool[,]) (pos: Coordinate) : bool =
//         match pos with
//         | _, y when y < 0 -> true
//         | x, _ when x < 0 || x >= 7 -> true
//         | x, y when blocked[x, y] -> true
//         | _ -> false
//
//     rock |> List.map (isPosBlocked blocked) |> List.reduce (||)
//
//
// let push (blocked: bool[,]) (dir: JetDirection) (rock: Rock) : Rock =
//     let movedRock =
//         match dir with
//         | Left -> pushLeft rock
//         | Right -> pushRight rock
//
//     match isBlocked blocked movedRock with
//     | true -> rock
//     | false -> movedRock
//
// let fall (blocked: bool[,]) (rock: Rock) : Rock option =
//     let movedRock = rock |> List.map (fun (x, y) -> (x, y - 1))
//
//     match isBlocked blocked movedRock with
//     | true -> None
//     | false -> Some movedRock
//
// let pop source = (Seq.head source, Seq.skip 1 source)
//
// let initRock (h: int) (rock: Rock) : Rock =
//     rock |> List.map (fun (x, y) -> (x + 2, y + h + 3))
//
// let step (blocked: bool[,]) (state: JetDirection seq * Rock) : (Rock * (JetDirection seq * Rock)) option =
//     let dir, pattern = pop (fst state)
//
//     let pushedRock = push blocked dir (snd state)
//
//     match fall blocked pushedRock with
//     | Some r -> Some(r, (pattern, r))
//     | None -> None
//
//
// let step2 (blocked: bool[,]) (jetStream: JetDirection seq) (rock: Rock) =
//     let mutable movingRock = rock
//     let mutable isFalling = true
//     let mutable pattern = jetStream
//
//     while isFalling do
//         let dir = pattern |> Seq.head
//         pattern <- pattern |> Seq.skip 1
//         movingRock <- push blocked dir movingRock
//
//         match fall blocked movingRock with
//         | Some r -> movingRock <- r
//         | None -> isFalling <- false
//
//     movingRock, pattern
//
//
// let simulateRock (state: State) (rock: Rock) : State =
//     let restingRock, pattern =
//         rock |> initRock state.Height |> step2 state.Blocked state.JetPattern
//
//     printf "."
//     restingRock |> List.iter (fun (x, y) -> state.Blocked[x, y] <- true)
//
//     { Blocked = state.Blocked
//       Height = restingRock |> List.map snd |> List.max |> (+) 1 |> max state.Height
//       JetPattern = pattern }
//
//
//
//
//
//
// let printBlocked (height: int) (blocked: bool[,]) =
//     for y in height .. -1 .. 0 do
//         printf "|"
//
//         for x in 0..6 do
//             let ch =
//                 match blocked[x, y] with
//                 | true -> '#'
//                 | false -> '.'
//
//             printf $"%c{ch}"
//
//         printfn "|"
//
//     printfn "+-------+"
//
//
// let startState =
//     { Blocked = Array2D.create 7 10000 false
//       Height = 0
//       JetPattern = jetStream }
//
// rockGenerator |> Seq.take 2022 |> // Seq.fold simulateRock startState
//
// //s.Blocked |> printBlocked s.Height
// // printfn $"Height: %d{s.Height}"
