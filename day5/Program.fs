type Crate = char
type Stack = Crate list
type Ship = Map<int, Stack>

type Instruction =
    { Quantity: int
      Source: int
      Target: int }

type Procedure = Instruction list

module Crate =
    let tryParse (str: string) : Crate option =
        if (str[1] = ' ') then
            None
        else
            Some str[1]

module Ship =
    open System.IO
    open System.Text.RegularExpressions

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

    let rec move ship instruction =
        if (instruction.Quantity = 0) then
            ship
        else
            let newShip =
                match Map.find instruction.Source ship with
                | x :: xs ->
                    ship
                    |> Map.add instruction.Source xs
                    |> Map.add instruction.Target (x :: (Map.find instruction.Target ship))
                | [] -> failwith "Try to collect from empty stack"

            move newShip { instruction with Quantity = instruction.Quantity - 1 }

module Procedure =
    open System
    open System.IO
    open System.Text.RegularExpressions

    [<Literal>]
    let private movePattern =
        @"^move\s(?<count>[1-9][0-9]*)\sfrom\s(?<source>[1-9][0-9]*)\sto\s(?<target>[1-9][0-9]*)"

    let private (|Instruction|_|) input =
        let m = Regex.Match(input, movePattern)

        if (m.Success) then
            Some
                { Quantity = Int32.Parse(m.Groups.["count"].Value)
                  Source = Int32.Parse(m.Groups.["source"].Value)
                  Target = Int32.Parse(m.Groups.["target"].Value) }
        else
            None

    let parseRow (row: string) =
        match row with
        | Instruction i -> Some i
        | _ -> None

    let ofSeq: string seq -> Procedure = Seq.map parseRow >> Seq.choose id >> Seq.toList
    let fromFile = File.ReadAllLines >> ofSeq

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

let sampleShip = sampleData |> Ship.ofSeq


sampleData
|> Procedure.ofSeq
|> List.fold Ship.move sampleShip
|> Map.toList
|> List.map (fun (_, crates) ->
    match crates with
    | c :: cs -> c
    | [] -> ' ')
|> System.String.Concat
|> printfn "%s"




let ship = Ship.fromFile "./day5/input.txt"

Procedure.fromFile "./day5/input.txt"
|> List.fold Ship.move ship
|> Map.toList
|> List.map (fun (_, crates) ->
    match crates with
    | c :: cs -> c
    | [] -> ' ')
|> System.String.Concat
|> printfn "%s"
