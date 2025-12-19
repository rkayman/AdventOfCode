open System
open System.Globalization
open System.IO

module Example =
    let given = """
        11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
        """
    let given2 = """245284-286195,797927-983972,1-21,85618-110471,307-1038"""

    let expected = 1227775554
    let expected2 = 4174379265L

module Problem =
    type Record = {
        bounds: string * string
        lower: Int64
        upper: Int64
    }

    type Dup =
        | Found of int64
        | TooLow
        | TooHigh

    let parseInput (input: string) =
        input
            .Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s ->
                let parts = s.Split('-', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                parts[0], parts[1]
                )
            |> Array.map (fun rng -> { bounds = rng
                                       lower = Int64.Parse(fst rng)
                                       upper = Int64.Parse(snd rng)
                                     })

    let (|IsEvenLen|IsOddLen|) (s: string) =
        let len = s.Length
        if (len &&& 1) = 0 then IsEvenLen else IsOddLen

    let makeDup n (s: string) =
        let len = s.Length
        let mid = len >>> 1
        let half = s.AsSpan(0, mid)
        let incr = Int64.Parse(half, NumberStyles.None, CultureInfo.InvariantCulture) + n
        let formatted = incr.ToString(CultureInfo.InvariantCulture)
        let len' = formatted.Length
        String.Create(len' * 2, formatted, fun dest state ->
            state.CopyTo(dest.Slice(0, state.Length))
            state.CopyTo(dest.Slice(state.Length, state.Length))
            )

    let tryDup min max (dup: string)=
        let dupL = dup |> Int64.Parse
        match Int64.Clamp(dupL, min, max) with
        | x when x = dupL -> Found dupL
        | x when x = min -> TooLow
        | x when x = max -> TooHigh
        | _ -> failwith "Unreachable"

    let findDups min max (s: string) =
        let rec loop t n acc =
            match t with
            | IsOddLen ->
                let nearestEven = pown 10 t.Length |> int64
                if nearestEven > max then
                    List.rev acc
                else
                    let t' = nearestEven.ToString(CultureInfo.InvariantCulture)
                    loop t' 0L acc
            | _ ->
                match t |> makeDup n |> tryDup min max with
                | Found d -> loop t (n + 1L) (d :: acc)
                | TooLow -> loop t (n + 1L) acc
                | TooHigh -> List.rev acc
        loop s 0L []

    let findInvalid (record: Record) =
        let b, e = record.bounds
        let fdups = findDups record.lower record.upper
        match b, e with
        | IsOddLen, IsOddLen -> Array.empty
        | _ -> b |> fdups |> List.toArray

//Example.given
// Example.given2
File.ReadAllText "2025/input-02.txt"
|> Problem.parseInput
|> Array.map Problem.findInvalid
|> Array.concat
|> Array.sum
