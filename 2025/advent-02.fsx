open System
open System.Globalization
open System.IO

module Example =
    let given = """
        11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
        """
    let given2 = """245284-286195,797927-983972,115-282,1-21,85618-110471,307-1038"""
    let given3 = """115-282,1-21,307-1038"""

    let expected = 1227775554
    let expected2 = 4174379265L

module Problem =
    let dump str a = printfn $"%s{str} --> %A{a}"; a
    
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
                | Found d -> loop t (d + 1L) (d :: acc)
                | TooLow -> loop t (n + 1L) acc
                | TooHigh -> List.rev acc
        loop s 0L []

    let findInvalid record = 
        let b, e = record.bounds
        let min, max = record.lower, record.upper
        match b, e with
        | IsOddLen, IsOddLen -> List.Empty
        | _ -> b |> findDups min max
        
    /// Part Two - find invalids with more repeating patterns

    type InvalidPattern =
        | OneDigitRepeating
        | TwoDigitRepeating
        | ThreeDigitRepeating
        | HalfRepeating

    let (|Even|Odd|) (s: string) =
        let len = s.Length
        if (len &&& 1) = 0 then Even len else Odd len

    let getInvalidSearchPatterns record =
        let b, e = record.bounds
        match b, e with
        | Even x, Even _ ->
            if x < 3
            then [ HalfRepeating ]
            else [ TwoDigitRepeating; HalfRepeating ]
        | Odd x, Odd y ->
            match Math.DivRem(x, 3), Math.DivRem(y, 3) with
            | (0, 1), _ -> List.empty                                   // length of 1 has no repeats
            | (1, _), _ -> [ OneDigitRepeating ]                        // length of 3 is limited to single digit repeats
            | _, (1, _) -> [ OneDigitRepeating ]                        // length of 3 is limited to single digit repeats
            | (2, 1), _ -> [ OneDigitRepeating ]                        // length of 7 is limited to single digit repeats
            | _, (2, 1) -> [ OneDigitRepeating ]                        // length of 7 is limited to single digit repeats
            | (_, 0), _ -> [ OneDigitRepeating; ThreeDigitRepeating ]   // length is multiple of 3
            | _, (_, 0) -> [ OneDigitRepeating; ThreeDigitRepeating ]   // length is multiple of 3
            | _ -> [ OneDigitRepeating ]                                // odd length not multiple of 3
        | Odd o, Even e
        | Even e, Odd o ->
            let even' = if e < 3 then [ HalfRepeating ] else [ TwoDigitRepeating; HalfRepeating ]
            if o % 3 = 0 && o > 3
            then [ OneDigitRepeating; ThreeDigitRepeating ] @ even'
            else [ OneDigitRepeating ] @ even'

    let getRepeatParams pattern (s: string) =
        match pattern with
        | OneDigitRepeating ->
            s.Length, s.Substring(0, 1)
        | TwoDigitRepeating ->
            s.Length >>> 1, s.Substring(0, 2)
        | ThreeDigitRepeating ->
            s.Length / 3, s.Substring(0, 3)
        | HalfRepeating ->
            let half = s.Length >>> 1
            2, s.Substring(0, half)
        
    let stringRepeater pattern s =
        let cnt, rpt = getRepeatParams pattern s
        rpt |> String.replicate cnt //|> dump $"Repeating {rpt} for pattern {pattern} yields"

    let (|EvenPattern|OddPattern|) pattern =
        match pattern with
        | TwoDigitRepeating
        | HalfRepeating -> EvenPattern
        | OneDigitRepeating
        | ThreeDigitRepeating -> OddPattern
        
    let findBadIds record pattern =
        let min, max = record.lower, record.upper
        let rec loop num set =
            if num > max then
                set
            else
                let str = (int64 num).ToString(CultureInfo.InvariantCulture)
                match pattern, str with
                | EvenPattern, Odd _ ->
                    let nearestEven = pown 10 str.Length |> int64
                    loop nearestEven set
                | OddPattern, Even _ ->
                    let nearestOdd = pown 10 str.Length |> int64
                    loop nearestOdd set
                | OneDigitRepeating, Odd l when l = 1 ->
                    set
                | ThreeDigitRepeating, Odd l when l = 3 ->
                    set
                | _ ->
                    let rpt = str |> stringRepeater pattern
                    if set |> Set.contains (int64 rpt) then 
                        loop (num + 1L) set
                    else
                        match rpt |> tryDup min max with
                        | Found d -> loop (d + 1L) (Set.add d set)
                        | TooLow -> loop (num + 1L) set
                        | TooHigh -> set
        loop min Set.empty |> Set.toList

    let findInvalid2 record =
        let patterns = getInvalidSearchPatterns record
        patterns
        |> List.collect (findBadIds record)
        |> List.distinct

// Example.given
// Example.given2
// Example.given3
File.ReadAllText "2025/input-02.txt"
|> Problem.parseInput
|> Array.map Problem.findInvalid2
|> List.concat
|> List.sum
