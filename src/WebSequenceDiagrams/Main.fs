/// Web Sequence Diagrams module
/// Inspired by https://github.com/hildjj/node-websequencediagrams/blob/master/lib/wsd.js
module WebSequenceDiagrams

open System
open System.Net.Http
open Newtonsoft.Json.Linq

[<RequireQualifiedAccess>]
type Format =
    | Pdf
    | Png
    | Svg
    | Wsd

    member this.Extension =
        match this with
        | Pdf -> ".pdf"
        | Png -> ".png"
        | Svg -> ".svg"
        | Wsd -> ".wsd"

    override this.ToString() =
        match this with
        | Pdf -> "pdf"
        | Png -> "png"
        | Svg -> "svg"
        | Wsd -> "wsd"

[<RequireQualifiedAccess>]
type Style =
    | Default
    | Earth
    | ModernBlue
    | Mscgen
    | Omegapple
    | Qsd
    | Rose
    | Roundgreen
    | Napkin
    | Magazine
    | VS2010
    | Patent

    override this.ToString() =
        match this with
        | Default -> "default"
        | Earth -> "earth"
        | ModernBlue -> "modern-blue"
        | Mscgen -> "mscgen"
        | Omegapple -> "omegapple"
        | Qsd -> "qsd"
        | Rose -> "rose"
        | Roundgreen -> "roundgreen"
        | Napkin -> "napkin"
        | Magazine -> "magazine"
        | VS2010 -> "vs2010"
        | Patent -> "patent"

let root = Uri("http://www.websequencediagrams.com/")

let diagramUrl (description:string, style:Style, format:Format) (client:HttpClient) =
    async {
        let query = dict [
            "style", style.ToString()
            "message", description
            "apiVersion", "1"
            "format", format.ToString()
        ]
        let uri = Uri(root, "/index.php")
        use content = new FormUrlEncodedContent(query)
        use! response = client.PostAsync(uri, content) |> Async.AwaitTask
        response.EnsureSuccessStatusCode() |> ignore
        match response.Content.Headers.ContentType.MediaType with
        | "application/x-json" | "application/json" ->
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            let json = JObject.Parse content
            match json.TryGetValue("img") with
            | true, img -> return Uri(root, img.ToString())
            | _ -> return failwithf "Invalid JSON:\n%s" content
        | contentType ->
            return failwithf "Invalid MIME type for JSON: %s" contentType
    }

let diagram (description:string, style:Style, format:Format) : Async<byte[]> =
    async {
        use client = new HttpClient()
        let! uri = client |> diagramUrl(description, style, format)
        use! response = client.GetAsync(uri) |> Async.AwaitTask
        response.EnsureSuccessStatusCode() |> ignore
        return! response.Content.ReadAsByteArrayAsync() |> Async.AwaitTask
    }
