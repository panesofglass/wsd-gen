open System
open System.IO

module WSD = WebSequenceDiagrams

type Diagram =
    {
        Description : string
        Format : WSD.Format
        Style : WSD.Style
        File : string
    }

// read the wsd 
let readWSD file =
    if File.Exists file then
        let description = File.ReadAllText(file, Text.Encoding.UTF8)
        if String.IsNullOrEmpty(description) then
            Error("Missing WSD")
        else Ok(description)
    else
        Error(sprintf "File not found: %s" file)

// write the png
let writeWSD diag =
    async {
        try
            let! buf = WSD.diagram(diag.Description, diag.Style, diag.Format)
            let outfile = diag.File.Replace(WSD.Format.Wsd.Extension, diag.Format.Extension)
            printfn "writing %s" outfile
            File.WriteAllBytes(outfile, buf)
        with err -> eprintfn "%A" err
    }
    |> Async.RunSynchronously

// do the work
let wsdgen file =
    match readWSD file with
    | Ok description ->
        let diag =
            { 
                Description = description
                File = file
                Format = WSD.Format.Png
                Style = WSD.Style.VS2010
            }
        writeWSD diag
    | Error msg -> eprintfn "%s" msg

[<EntryPoint>]
let main argv =
    match argv with
    | [|file|] ->
        wsdgen file
    | _ ->
        printfn "Usage:"
        printfn "wsdgenerator <file>"
    0 // return an integer exit code
