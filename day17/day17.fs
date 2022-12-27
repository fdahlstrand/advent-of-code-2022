type CircularBuffer<'a> = { Index: int; Data: 'a[] }

type JetDirection =
    | Left
    | Right

type Rock = Rock of (int * int) list

type Room =
    { JetSource: CircularBuffer<JetDirection>
      RockSource: CircularBuffer<Rock>
      Blocked: Set<int * int>
      ActiveRock: Rock option
      Height: int }

module CircularBuffer =
    let create data = { Index = 0; Data = Seq.toArray data }

    let next buffer =
        buffer.Data[buffer.Index], { buffer with Index = (buffer.Index + 1) % buffer.Data.Length }

module Room =
    let getNextJet room =
        let jet, src = room.JetSource |> CircularBuffer.next
        jet, { room with JetSource = src }

    let getNextRock room =
        let rock, src = room.RockSource |> CircularBuffer.next
        rock, { room with RockSource = src }

    let injectRock (Rock rock) room =
        let offset dx dy (x, y) = (x + dx, y + dy)

        let newRock = rock |> List.map (offset 2 (room.Height + 3))

        { room with ActiveRock = Some(Rock newRock) }

    let isRockAtBottom rock =
        rock |> List.fold (fun acc (_, y) -> acc || (y < 0)) false

    let isBlocked rock room =
        rock |> List.map (fun c -> Set.contains c room.Blocked) |> List.reduce (||)

    let inBounds rock room =
        match isBlocked rock room with
        | true -> false
        | false -> rock |> List.map (fun (x, _) -> 0 <= x && x < 7) |> List.reduce (&&)


    let pushRock room =
        match room.ActiveRock with
        | Some(Rock r) ->
            let jet, room' = getNextJet room

            let pushedRock =
                match jet with
                | Left -> r |> List.map (fun (x, y) -> (x - 1, y))
                | Right -> r |> List.map (fun (x, y) -> (x + 1, y))

            { room' with ActiveRock = Some(Rock(if (inBounds pushedRock room) then pushedRock else r)) }
        | None -> room

    let fuseActiveRock room =
        match room.ActiveRock with
        | Some(Rock r) ->
            { room with
                Blocked = r |> Set.ofList |> Set.union room.Blocked
                Height = r |> List.maxBy snd |> snd |> (+) 1 |> max room.Height
                ActiveRock = None }
        | None -> room


    let dropRock room =
        // Look into using Option.map
        let room' = pushRock room

        match room'.ActiveRock with
        | Some(Rock r) ->
            let newRock = r |> List.map (fun (x, y) -> (x, y - 1))

            match (isRockAtBottom newRock) || (isBlocked newRock room') with
            | true ->
                { room' with
                    Blocked = r |> Set.ofList |> Set.union room.Blocked
                    Height = r |> List.maxBy snd |> snd |> (+) 1 |> max room.Height
                    ActiveRock = None }
            | false -> { room' with ActiveRock = Some(Rock newRock) }
        | None -> room'


    let rec dropRockToBottom room =
        match room.ActiveRock with
        | Some _ -> dropRockToBottom (dropRock room)
        | None -> room

    let nextRock room =
        room |> getNextRock ||> injectRock |> dropRockToBottom

module JetDirection =
    let ofChar ch =
        match ch with
        | '<' -> Left
        | '>' -> Right
        | ch -> failwith $"Unexpected direction character '%c{ch}'"


let printRoom room =
    for y in room.Height .. - 1 .. 0 do
        printf "|"

        for x in 0..6 do
            match Set.contains (x, y) room.Blocked with
            | true -> printf "#"
            | false -> printf "."

        printfn "|"

    printfn "+-------+"

let data = System.IO.File.ReadLines "./day17/input.txt" |> Array.ofSeq

let initialRoom =
    { JetSource = data[0] |> Seq.map JetDirection.ofChar |> CircularBuffer.create
      RockSource =
        [ Rock [ (0, 0); (1, 0); (2, 0); (3, 0) ]
          Rock [ (1, 0); (0, 1); (1, 1); (2, 1); (1, 2) ]
          Rock [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ]
          Rock [ (0, 0); (0, 1); (0, 2); (0, 3) ]
          Rock [ (0, 0); (1, 0); (0, 1); (1, 1) ] ]
        |> CircularBuffer.create
      Height = 0
      Blocked = Set.empty
      ActiveRock = None }

let folder s _ = Room.nextRock s

[ 1..2022 ]
|> List.fold folder initialRoom
|> (fun r -> r.Height)
|> printfn "%d"
