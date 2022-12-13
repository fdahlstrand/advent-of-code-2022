type PacketElement =
    | List of Packet
    | Number of int

and Packet = PacketElement list

module Parser =
    type Token =
        | Number of int
        | StartOfList
        | EndOfList

    let isDigit ch = '0' <= ch && ch <= '9'

    let parseNumber (str: char seq) =
        let num = str |> Seq.takeWhile isDigit |> System.String.Concat |> System.Int32.Parse

        (str |> Seq.skipWhile isDigit, num)

    let rec tokenize (str: char seq) =
        if Seq.isEmpty str then
            None
        else
            match Seq.head str with
            | ch when isDigit ch ->
                let reminder, d = parseNumber str
                Some(Number d, reminder)
            | '[' -> Some(StartOfList, Seq.skip 1 str)
            | ']' -> Some(EndOfList, Seq.skip 1 str)
            | ',' -> tokenize (Seq.skip 1 str)
            | ch -> failwith $"Unexpected character '%c{ch}'"

    let parse (stack: PacketElement list) (token: Token) : PacketElement list =
        match token with
        | StartOfList -> List [] :: stack
        | Number n ->
            match stack with
            | List l :: rest -> List(l @ [ PacketElement.Number n ]) :: rest
            | e -> failwith $"Expected a list found [%A{e}] "
        | EndOfList ->
            match stack with
            | List inner :: rest ->
                match rest with
                | List outer :: t -> List(outer @ [ List inner ]) :: t
                | [] -> [ List inner ]
                | e -> failwith $"Expected a list found [%A{e}] "
            | e -> failwith $"Expected a list found [%A{e}] "

    let fromString (str: string) =
        (List.Empty, Seq.unfold tokenize str) ||> Seq.fold parse |> List.head

let rec compare (left, right) =
    match left, right with
    | Number l, Number r when l = r -> 0
    | Number l, Number r when l < r -> -1
    | Number l, Number r -> 1
    | List l, Number r -> compare (List l, List [ Number r ])
    | Number l, List r -> compare (List [ Number l ], List r)
    | List (l :: ls), List (r :: rs) ->
        let cmp = compare (l, r)
        if cmp = 0 then compare (List ls, List rs) else cmp
    | List [], List (_ :: _) -> -1
    | List (_ :: _), List [] -> 1
    | List [], List [] -> 0

let fromStrings (strings: string []) =
    seq {
        for i in 0..3 .. Array.length strings - 1 do
            (Parser.fromString strings[i], Parser.fromString strings[i + 1])
    }

let fromFile = System.IO.File.ReadAllLines >> Seq.toArray >> fromStrings

let sample = fromFile "./day13/sample.txt"
let input = fromFile "./day13/input.txt"

input
|> Seq.map compare
|> Seq.mapi (fun i v -> i + 1, v)
|> Seq.filter (fun p -> snd p = -1)
|> Seq.sumBy (fun p -> fst p)
|> printfn "Sum of correctly ordered indices: %d"

let dividers = [ List [ List[Number 2] ]
                 List [ List[Number 6] ] ]

input
|> Seq.collect (fun (a, b) -> [ a; b ])
|> Seq.toList
|> List.append dividers 
|> List.sortWith (fun a b -> compare (a, b))
|> List.mapi (fun i p -> i + 1, p)
|> List.filter (fun (_, p) -> List.contains p dividers)
|> List.map fst
|> List.reduce (*)
|> printfn "Decoder key for distrss signal: %d"