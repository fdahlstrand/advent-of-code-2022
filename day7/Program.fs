type File = { Name: string; Size: int }
type Directory = { Name: string }

type FilesystemContent =
    | File of File
    | Directory of Directory

type OutputLine =
    | ChangeDirectory of string
    | ListContents
    | DirectoryEntry of string
    | FileEntry of File

module Terminal =
    open System
    open System.Text.RegularExpressions

    let private parseInt (str: string) =
        match Int32.TryParse(str) with
        | (true, value) -> value
        | (false, _) -> failwith $"'%s{str}' is not a valid number"

    let (|ChangeDirectory|_|) input =
        let m = Regex.Match(input, @"^\$\s+cd\s+(?<path>.+)$")

        if m.Success then
            Some m.Groups["path"].Value
        else
            None

    let (|ListContents|_|) input =
        let m = Regex.Match(input, @"^\$\s+ls$")

        if m.Success then Some() else None

    let (|DirectoryEntry|_|) input =
        let m = Regex.Match(input, @"^dir\s+(?<path>.+)$")

        if m.Success then
            Some m.Groups["path"].Value
        else
            None

    let (|FileEntry|_|) input =
        let m = Regex.Match(input, @"^(?<size>[1-9][0-9]*)\s+(?<name>.+)$")

        if m.Success then
            Some
                { Name = m.Groups["name"].Value
                  Size = parseInt m.Groups["size"].Value }
        else
            None

    let private parseOutputLine str =
        match str with
        | ChangeDirectory path -> ChangeDirectory path
        | ListContents -> ListContents
        | DirectoryEntry path -> DirectoryEntry path
        | FileEntry file -> FileEntry file
        | _ -> failwith $"Unknown terminal output '%s{str}'"

    let ofLines (lines: string seq) = lines |> Seq.map parseOutputLine

    let fromFile = System.IO.File.ReadAllLines >> ofLines

type ScanState =
    { Cwd: string list
      Filesystem: Map<string list, FilesystemContent list> }

let scanOutput state line =
    match line with
    | ChangeDirectory dir ->
        match dir with
        | "/" -> { state with Cwd = [ "/" ] }
        | ".." ->
            match state.Cwd with
            | _ :: ds -> { state with Cwd = ds }
            | [] -> state
        | d -> { state with Cwd = [ d ] @ state.Cwd }
    | ListContents -> state
    | DirectoryEntry dir ->
        let folder = Map.tryFind state.Cwd state.Filesystem

        let content =
            match folder with
            | Some content -> [ Directory { Name = dir } ] @ content
            | None -> [ Directory { Name = dir } ]

        { state with Filesystem = Map.add state.Cwd content state.Filesystem }
    | FileEntry f ->
        let folder = Map.tryFind state.Cwd state.Filesystem

        let content =
            match folder with
            | Some content -> [ File f ] @ content
            | None -> [ File f ]

        { state with Filesystem = Map.add state.Cwd content state.Filesystem }


let rec sum path fs =
    let folder = Map.find path fs

    folder
    |> List.sumBy (fun e ->
        match e with
        | File f -> f.Size
        | Directory d -> sum ([ d.Name ] @ path) fs)

let rec directories path fs =
    (path)
    :: (Map.find path fs
        |> List.collect (fun d ->
            match d with
            | File f -> []
            | Directory d -> directories ([ d.Name ] @ path) fs))

let fs =
    ({ Cwd = List.empty
       Filesystem = Map.empty },
     Terminal.fromFile "./day7/input.txt")
    //  output)
    ||> Seq.fold scanOutput
    |> (fun s -> s.Filesystem)

let dirs =
    fs
    |> directories [ "/" ]
    |> List.map (fun p -> (p, fs |> sum p))

dirs
|> List.filter (fun d -> snd d < 100_000)
|> List.sumBy snd
|> printfn "%A"

let totalUsed =
    dirs
    |> List.find (fun d -> (d |> fst) = [ "/" ])
    |> snd

let unusedSpace = 70000000 - totalUsed
let neededSpace = 30000000 - unusedSpace

dirs
|> List.map (fun d -> (snd d - neededSpace, snd d))
|> List.filter (fun s -> fst s > 0)
|> List.minBy (fun s -> fst s)
|> snd
|> printf "%A"
