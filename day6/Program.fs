open System.IO

type StreamState = { Buffer: string; Pos: int }

let packetizer N state =
    if (state.Pos + N >= String.length state.Buffer) then
        None
    else
        Some((state.Pos + N, state.Buffer[state.Pos .. state.Pos + (N - 1)]), { state with Pos = state.Pos + 1 })

let countUniqueChars = Set.ofSeq >> Set.count

let detectMarker N str =
    let gen = packetizer N

    { Buffer = str; Pos = 0 }
    |> Seq.unfold gen
    |> Seq.map (fun (p, s) -> (p, countUniqueChars s))
    |> Seq.filter (fun (p, c) -> c = N)
    |> Seq.map (fun (p, c) -> p)
    |> Seq.min

File.ReadAllText("./day6/input.txt")
|> detectMarker 4
|> printfn "Start-of-packet marker @%d"

File.ReadAllText("./day6/input.txt")
|> detectMarker 14
|> printfn "Start-of-packet marker @%d"
