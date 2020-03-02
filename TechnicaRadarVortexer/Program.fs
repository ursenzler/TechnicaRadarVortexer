open Argu
open FSharp.Data
open FSharp.Data.CsvExtensions
open System
open System.IO

let SummaryIndex = 3
let StateIndex = 8
let ClassificationIndex = 11
let TypeIndex = 15
let CompetenceIndex = 16
let FeasibilityIndex = 21
let RadarIndex = 26

type RadarRing = Use | Evaluate | Reconsider
type RadarSegment = ``Concept or theme`` | ``Method or technique`` | ``Libraries, Frameworks, Programming Languages`` | Tools

type Radar = ``Would be missed`` | ``Trend (upwards)`` | ``Trend (downwards)`` | ``Breaking change`` | ``Not on radar`` | Detailed
type Feasibility = ``Better alternative exists`` | ``Situatively okay`` | ``End of life`` | ``Do not use`` | ``Recommended`` | ``Not ready`` | None
type TopicType = Library | Framework | ``Tool-Software`` | Product | Service | Theme | Concept| Method | Process | ``Engineering Practice`` | ``Programming Language`` | OS | Undefined
type Classification = Know | Master | Experiment | Observe | ``Do not use`` | ``Situatively okay`` | None

type Bucket =
    | RingSegment of RadarRing * RadarSegment
    | NotOnRadar

type Topic = { Text : string; Bucket : Bucket; Competence : string; Annotation : Radar; DetailedOnly : bool; Draft : bool }

let getSegments(topicTypes:TopicType[]) =
    topicTypes
    |> Array.map(fun x ->
        match x with
        | Concept | Theme -> Option.Some(``Concept or theme``)
        | Method | Process | ``Engineering Practice`` -> Option.Some(``Method or technique``)
        | Library | Framework | ``Programming Language`` | OS -> Option.Some(``Libraries, Frameworks, Programming Languages``)
        | ``Tool-Software`` | Product | Service -> Option.Some(Tools)
        | Undefined -> Option.None)
    |> Array.distinct

let extract(row:CsvRow) =
    let summary = row?Summary
    let state = row?State
    let classification =
        match row?Classification with
        | "Know" -> Know
        | "Master" -> Master
        | "Experiment" -> Experiment
        | "Observe" -> Observe
        | "do not use" -> ``Do not use``
        | "situatively okay" -> ``Situatively okay``
        | "No technik scouting klassifizierung" -> Classification.None
        |  unknown -> raise (Exception("unknown classification value: " + unknown))

    let types = ((string)row?``TP Type``).Split(',') |> Array.map (fun x ->
        match x.Trim() with
        | "Library" -> Library
        | "Framework" -> Framework
        | "Tool-Software" -> ``Tool-Software``
        | "Product" -> Product
        | "Service" -> Service
        | "Theme" -> Theme
        | "Concept" -> Concept
        | "Method" -> Method
        | "Process" -> Process
        | "Engineering Practice" -> ``Engineering Practice``
        | "Programming Language" -> ``Programming Language``
        | "OS" -> OS
        | "Undefined" -> Undefined
        |  unknown -> raise (Exception("unknown TP type value: " + unknown)))

    let competence = row?Competence

    let feasibility =
        match row?Feasibility with
        | "better alternative exists" -> ``Better alternative exists``
        | "situatively okay" -> Feasibility.``Situatively okay``
        | "end of life" -> ``End of life``
        | "do not use" -> Feasibility.``Do not use``
        | "recommended" -> ``Recommended``
        | "not ready" -> ``Not ready``
        | "No feasability" -> Feasibility.None
        |  unknown -> raise (Exception("unknown feasibility value: " + unknown))

    let radar =
        match row?Radar with
        | "would be missed" -> ``Would be missed``
        | "trend (upwards)" -> ``Trend (upwards)``
        | "trend (downwards)" -> ``Trend (downwards)``
        | "breaking change" -> ``Breaking change``
        | "not on radar" | "No radar" -> ``Not on radar``
        | "detailed" -> Detailed
        | unknown -> raise (Exception("unknown radar value: " + unknown))

    let segments = getSegments(types)

    let buckets =
        match radar with
        | ``Not on radar`` -> Array.singleton NotOnRadar
        | _ -> segments |> Array.map(fun segment ->
            match segment with
            | Option.None -> NotOnRadar
            | Option.Some s ->
                match (classification, feasibility) with
                | (_, ``Better alternative exists``)
                | (_, ``End of life``)
                | (_, Feasibility.``Situatively okay``)
                | (_, Feasibility.``Do not use``) -> RingSegment(Reconsider, s)
                | (Experiment, _)
                | (Observe, _) -> RingSegment(Evaluate, s)
                | (_, ``Not ready``) -> RingSegment(Reconsider, s)
                | (_, Recommended) -> RingSegment(Use, s)
                | (_, Feasibility.None) -> NotOnRadar)

    let detailedOnly =
        match radar with
        | Detailed -> true
        | _ -> false

    buckets
    |> Array.map (fun x ->
        {
            Text = summary
            Bucket = x
            Competence = competence
            Annotation = radar
            DetailedOnly = detailedOnly
            Draft = not (state = "Klassifiziert")
        } : Topic)

let raster = seq [
    (Use, ``Concept or theme``);
    (Use, ``Method or technique``);
    (Use, ``Libraries, Frameworks, Programming Languages``);
    (Use, Tools);
    (Evaluate, ``Concept or theme``);
    (Evaluate, ``Method or technique``);
    (Evaluate, ``Libraries, Frameworks, Programming Languages``);
    (Evaluate, Tools);
    (Reconsider, ``Concept or theme``);
    (Reconsider, ``Method or technique``);
    (Reconsider, ``Libraries, Frameworks, Programming Languages``);
    (Reconsider, Tools)
]

let annotate annotation =
    match annotation with
    | ``Trend (upwards)`` -> "^"
    | ``Trend (downwards)`` -> "v"
    | ``Breaking change`` -> "!"
    | _ -> ""

let dump topics =
    raster
    |> Seq.collect (fun (ring, segment) ->
        let output =
            topics
            |> Seq.filter (fun x ->
                match x.Bucket with
                | RingSegment (rr, rs) when rr = ring && rs = segment -> true
                | _ -> false)
            |> Seq.sortBy (fun x -> x.Text.ToLowerInvariant())

        seq {
            yield ""
            yield String.Format("### {0} - {1}", ring, segment)
            yield!
                output
                |> Seq.map (fun x ->
                     String.Format(
                         "{0} {1}{2}",
                         x.Text,
                         annotate x.Annotation,
                         if (x.Draft) then "°" else ""))
        })

type CLIArguments =
    | [<MainCommand>]File of path:string
    | [<AltCommandLine("-o")>]Output of path:string
    | [<AltCommandLine("-r")>]Radar of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "specify an input csv file"
            | Radar _ -> "select the radar to be created. Possible values: global or a comma-separated list of competences: .Net Technologies, Agile, ..."
            | Output _ -> "specify the output markdown file. If no file is specified, the text is dumped to the console"

let trim (s : string) =
    s.Trim()

[<EntryPoint>]
let main argv =

    let parser = ArgumentParser.Create<CLIArguments>(programName = "TechnicaRadarVortexer.exe")
    let results = parser.Parse argv

    let inputFilePath = results.GetResult File
    let radar = results.TryGetResult Radar
    let output = results.TryGetResult Output

    let csv = CsvFile.Load(inputFilePath)

    let topics =
        csv.Rows
        |> Seq.collect extract

    let chosenTopics =
        match radar with
        | Some x when not (x = "global") ->
            let competences = x.Split(',') |> Array.map trim
            topics
            |> Seq.filter (fun x -> Array.contains x.Competence competences)
        | _ ->
            topics
            |> Seq.filter (fun x -> not x.DetailedOnly)


    let markdown =
        seq {
            yield String.Format("## {0}", if (radar.IsSome) then radar.Value else "global")
            yield! dump chosenTopics
            yield ""
            yield ""
            yield "### Legend:"
            yield "- ° = Draft"
            yield "- ^ = trend upwards"
            yield "- v = trend downwards"
            yield "- ! = breaking change"
        }
        |> (fun x -> String.Join("\n\r", x))

    match output with
    | Some path -> File.WriteAllText(path, markdown)
    | _ -> Console.WriteLine(markdown)
    |> ignore

    Console.WriteLine("built radar with {0} topics.", Seq.length chosenTopics)

    0 // return an integer exit code