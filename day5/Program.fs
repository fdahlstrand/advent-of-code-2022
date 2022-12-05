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
        match str[1] with
        | ' ' -> None
        | c -> Some c

module Ship =
    open System.IO
    open System.Text.RegularExpressions

    type private CrateRow = (int * Crate option) []

    [<Literal>]
    let private crateRowPattern = @"^((?<slot>(\[[A-Z]\])|(\s\s\s))\s?)+"

    let private (|CrateRow|_|) input : CrateRow option =
        let m = Regex.Match(input, crateRowPattern)

        if m.Success then
            let captures = m.Groups["slot"].Captures
            let crates =
                [| for i in 0 .. captures.Count - 1 ->
                       Crate.tryParse captures[i].Value |]
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

    let private foldRow (acc: Ship) (ix: int, c: Crate option) : Ship =
        match c with
        | Some x when Map.containsKey ix acc -> Map.add ix ((Map.find ix acc) @ [ x ]) acc
        | Some x -> Map.add ix [ x ] acc
        | None -> acc

    let private foldRows (acc: Ship) = Array.fold foldRow acc

    let ofSeq: string seq -> Ship =
        Seq.map parseRow >> Seq.choose id >> Seq.fold foldRows empty

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

    let batchMove ship instruction =
        let sourceStack = Map.find instruction.Source ship
        let batch = List.take instruction.Quantity sourceStack
        let reminder = List.skip instruction.Quantity sourceStack

        ship
        |> Map.add instruction.Source reminder
        |> Map.add instruction.Target (batch @ (Map.find instruction.Target ship))
        
    let topCrateString:Ship -> string = Map.toList >> List.map (fun (_, crates) -> match crates with | c :: _ -> c | [] -> ' ') >> System.String.Concat
    
    let rearrange ship crane = List.fold crane ship

module Procedure =
    open System
    open System.IO
    open System.Text.RegularExpressions

    [<Literal>]
    let private movePattern =
        @"^move\s(?<count>[1-9][0-9]*)\sfrom\s(?<source>[1-9][0-9]*)\sto\s(?<target>[1-9][0-9]*)"

    let private (|Instruction|_|) input =
        let m = Regex.Match(input, movePattern)

        if m.Success then
            Some
                { Quantity = Int32.Parse(m.Groups["count"].Value)
                  Source = Int32.Parse(m.Groups["source"].Value)
                  Target = Int32.Parse(m.Groups["target"].Value) }
        else
            None

    let parseRow (row: string) =
        match row with
        | Instruction i -> Some i
        | _ -> None

    let ofSeq: string seq -> Procedure = Seq.map parseRow >> Seq.choose id >> Seq.toList
    let fromFile = File.ReadAllLines >> ofSeq
    
module Crane =
    let rec crateMover9000 ship instruction =
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

            crateMover9000 newShip { instruction with Quantity = instruction.Quantity - 1 }

    let crateMover9001 ship instruction =
        let sourceStack = Map.find instruction.Source ship
        let batch = List.take instruction.Quantity sourceStack
        let reminder = List.skip instruction.Quantity sourceStack

        ship
        |> Map.add instruction.Source reminder
        |> Map.add instruction.Target (batch @ (Map.find instruction.Target ship))

let ship = Ship.fromFile "./day5/input.txt"

Procedure.fromFile "./day5/input.txt"
|> Ship.rearrange ship Crane.crateMover9000 
|> Ship.topCrateString
|> printfn "Rearranged with CrateMover 9000: %s"

Procedure.fromFile "./day5/input.txt"
|> Ship.rearrange ship Crane.crateMover9001 
|> Ship.topCrateString
|> printfn "Rearranged with CrateMover9001: %s"
