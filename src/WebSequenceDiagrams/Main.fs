module WebSequenceDiagrams

[<RequireQualifiedAccess>]
type Format =
    | Png
    | Wsd

    member this.Extension =
        match this with
        | Png -> ".png"
        | Wsd -> ".wsd"

    override this.ToString() =
        match this with
        | Png -> "png"
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

let diagram (description:string, style:Style, format:Format) : Async<byte[]> =
    async {
        return [||]
    }
