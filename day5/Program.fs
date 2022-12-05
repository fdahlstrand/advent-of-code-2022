open System.Text.RegularExpressions

type Crate = char
type Stack = Crate list
type Ship = Map<int, Stack>

module Crate =
    let tryParse (str: string) : Crate option =
        if (str[1] = ' ') then
            None
        else
            Some str[1]

module Ship =
    open System.IO

    type private CrateRow = (int * Crate option) []

    [<Literal>]
    let private crateRowPattern = @"^((?<slot>(\[[A-Z]\])|(\s\s\s))\s?)+"

    let private (|CrateRow|_|) input : CrateRow option =
        let m = Regex.Match(input, crateRowPattern)

        if (m.Success) then
            let crates =
                [| for i in 0 .. m.Groups.["slot"].Captures.Count - 1 ->
                       Crate.tryParse m.Groups.["slot"].Captures.[i].Value |]
                |> Array.indexed
                |> Array.map (fun (ix, c) -> (ix + 1, c))

            Some crates
        else
            None

    let private parseRow (row: string) =
        match row with
        | CrateRow cr -> Some cr
        | _ -> None

    let private empty: Ship = Map.empty<int, Stack>

    let foldRow (acc: Ship) (ix: int, c: Crate option) : Ship =
        match c with
        | Some x when Map.containsKey ix acc -> Map.add ix ((Map.find ix acc) @ [ x ]) acc
        | Some x -> Map.add ix [ x ] acc
        | None -> acc


    let private foldRows (acc: Ship) = Array.fold foldRow acc

    let ofSeq: string seq -> Ship =
        Seq.map parseRow
        >> Seq.choose id
        >> Seq.fold foldRows empty

    let fromFile = File.ReadAllLines >> ofSeq


// [<Literal>]
// let movePattern =
//     @"^move\s(?<count>[1-9][0-9]*)\sfrom\s(?<source>[1-9][0-9]*)\sto\s(?<target>[1-9][0-9]*)"



// let (|CrateMap|_|) input =
//     let m = Regex.Match(input, cratePattern)

//     if (m.Success) then
//         let crates =
//             [| for i in 0 .. m.Groups.["slot"].Captures.Count - 1 -> Crate.tryParse m.Groups.["slot"].Captures.[i].Value |]
//             |> Array.indexed
//             |> Array.map (fun (ix, c) -> (ix + 1, c))

//         Some crates
//     else
//         None

// let (|MoveInstruction|_|) input =
//     let m = Regex.Match(input, movePattern)

//     if (m.Success) then
//         Some m.Value
//     else
//         None



// let stacks: Crate list list = []

// let folder (acc: Map<int, Crate list>) (row: (int * Crate option) []) =
//     let innerFold (a: Map<int, Crate list>) (ix: int, c: Crate option) =
//         match c with
//         | Some x when Map.containsKey ix a -> Map.add ix ((Map.find ix a) @ [ x ]) a
//         | Some x -> Map.add ix [ x ] a
//         | None -> a

//     Array.fold innerFold acc row


// sampleData
// |> Seq.map (fun s ->
//     match s with
//     | CrateMap crates -> Some crates
//     | _ -> None)
// |> Seq.choose id
// |> Seq.fold folder Map.empty<int, Crate list>
// |> printfn "%A"

let sampleData =
    seq {
        "    [D]    "
        "[N] [C]    "
        "[Z] [M] [P]"
        " 1   2   3 "
        ""
        "move 1 from 2 to 1"
        "move 3 from 1 to 3"
        "move 2 from 2 to 1"
        "move 1 from 1 to 2"
    }

sampleData |> Ship.ofSeq |> printfn "%A"

Ship.fromFile "./day5/input.txt" |> printf "%A"
