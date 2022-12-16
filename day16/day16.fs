type Valve = { FlowRate: int; Tunnels: string list }

type Cave = Map<string, Valve>

type Action =
    | MoveTo of string
    | Open of string

let valve rate tunnels = { FlowRate = rate; Tunnels = tunnels }

let cave =
    Map
        [ ("AA", valve 0 [ "DD"; "II"; "BB" ])
          ("BB", valve 13 [ "CC"; "AA" ])
          ("CC", valve 2 [ "DD"; "BB" ])
          ("DD", valve 20 [ "CC"; "AA"; "EE" ])
          ("EE", valve 3 [ "FF"; "DD" ])
          ("FF", valve 0 [ "EE"; "GG" ])
          ("GG", valve 0 [ "FF"; "HH" ])
          ("HH", valve 22 [ "GG" ])
          ("II", valve 0 [ "AA"; "JJ" ])
          ("JJ", valve 21 [ "II" ]) ]

module Valve =
    let tunnels valve = valve.Tunnels

module Cave =
    let moves src cave = Map.find src cave |> Valve.tunnels



type State =
    { OpenValves: Set<string>
      Location: string }

let start =
    { OpenValves = Set.empty
      Location = "AA" }

let actions src valves cave =
    let v = Map.find src cave
    let moves = Valve.tunnels v |> List.map (fun s -> MoveTo s)

    match v.FlowRate with
    | v when v > 0 && not (Set.contains src valves) -> Open src :: moves
    | _ -> moves

let doAction state action =
    match action with
    | MoveTo v -> { state with Location = v }
    | Open v -> { state with OpenValves = Set.add v state.OpenValves }

type TickState =
    { Cave: Cave
      State: State
      Actions: Action list }

let tick state =
    let action = state.Actions |> List.tryHead

    match action with
    | None -> None
    | Some a ->
        let s = doAction state.State a
        let next = actions s.Location state.State.OpenValves state.Cave

        Some(
            s,
            { state with
                State = s
                Actions = List.concat [ next; state.Actions.Tail ] }
        )


// { Cave = cave
//   State = start
//   Actions = actions "AA" Set.empty<string> cave }
// |> Seq.unfold tick
// |> printfn "%A"

let indices = cave |> Map.keys |> Array.ofSeq |> Array.sort
let indexOf str = indices |> Array.findIndex ((=) str)
printfn "%A" (indexOf "CC")

let isConnected i j =
    let vi = indices[i]
    let vj = indices[j]

    (Map.find vi cave).Tunnels |> List.contains vj


let valves = Map.count cave
let V = Array2D.create valves valves 1000


for i in 0 .. valves - 1 do
    for j in 0 .. valves - 1 do
        V[i, j] <-
            match isConnected i j with
            | true -> 1
            | false -> 1000

for i in 0 .. valves - 1 do
    V[i, i] <- 0

for k in 0 .. valves - 1 do
    for i in 0 .. valves - 1 do
        for j in 0 .. valves - 1 do
            if V[i, j] > V[i, k] + V[k, j] then
                V[i, j] <- V[i, k] + V[k, j]



printfn "%A" V
