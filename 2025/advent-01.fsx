open System
open System.IO

module Example =
    let given = """
        L68
        L30
        R48
        L5
        R60
        L55
        L1
        L99
        R14
        L82
        """

    let expected = 3

module Problem =
    type Dir = L | R
    type Turn = Dir * int

    module Turn =
        let create (str: string) =
            let dir = if str[0] = 'L' then L else R
            let dist = str[1..] |> int
            dir, dist

    let startPos = 50

    let parseInput (input: string) =
        input
            .Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
            |> Array.map Turn.create

    let solve input =
        parseInput input
        |> Array.scan (fun pos turn ->
            let dist = if fst turn = L then -snd turn else snd turn
            ((pos + dist) % 100 + 100) % 100
            ) startPos

    let rotate pos dist =
        let turns = abs(dist / 100)
        let remainder = dist % 100
        match turns, pos + remainder with
        | turns, pos' when pos' < 0 ->
            100 + pos', turns + if pos <> 0 then 1 else 0
        | turns, pos' when pos' > 100 ->
            pos' % 100, turns + if pos <> 0 then 1 else 0
        | turns, pos' ->
            pos' % 100, turns

    let solve2 input =
        parseInput input
        |> Array.scan (fun (pos, _) turn ->
            let dist = if fst turn = L then -snd turn else snd turn
            rotate pos dist
            ) (startPos, 0)

let count (pos, xz) =
    match pos, xz with
    | 0, x -> 1 + x
    | _, x -> x

// Example.given
File.ReadAllText "2025/input-01.txt"
|> Problem.solve2
|> Array.map count
|> Array.sum
|> (fun cnt -> printfn $"Result: %d{cnt}"; cnt)

// Example.given
File.ReadAllText "2025/input-01.txt"
|> Problem.solve
|> Array.where (fun pos -> pos = 0)
|> Array.length
|> (fun cnt -> printfn $"Result: %d{cnt}"; cnt)
