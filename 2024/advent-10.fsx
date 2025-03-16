open System
open System.IO

module Example =
    let given = """
        89010123
        78121874
        87430965
        96549874
        45678903
        32019012
        01329801
        10456732
        """

    let expected = [5; 6; 5; 3; 1; 3; 5; 3; 5]

    let given1 = """
        8888808
        8843218
        8858828
        8865438
        2272242
        2287652
        2292222
        """
    let given2 = """
        2290229
        4441498
        2222227
        6543456
        7652987
        8762222
        9872222
        """
    let given3 = """
        012345
        123456
        234567
        345678
        496789
        567891
        """

module Problem =
    let given = File.ReadAllText "2024/input-10.txt"

let input =
    Problem.given
        .Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun ch -> (int ch) - (int '0')))
        |> Array.mapi (fun i xs -> xs |> Array.mapi (fun j x -> x, (i, j)))
        |> Array.concat
        |> Array.groupBy fst
        |> Array.map (fun (k, vs) -> k, vs |> Array.map snd |> Set.ofArray)
        |> Array.sortBy fst

let nextSteps (r, c) =
    [
        r - 1, c  // try up
        r + 1, c  // try down
        r, c - 1  // try left
        r, c + 1  // try right
    ]

let rec bfs (state: {| moves: Set<int * int>; next: int; rating: int |}) coord=
    let state = {| state with moves = state.moves |> Set.add coord |}
    if state.next > 9 then {| state with rating = state.rating + 1 |}
    else
        let nextPositions = input[state.next] |> snd
        let possibleSteps = nextSteps coord
        let matches =
            possibleSteps
            |> List.filter (fun step -> nextPositions |> Set.contains step)
            |> List.filter (fun step -> not (state.moves |> Set.contains step))
        if List.isEmpty matches then state
        else
            let state' =
                {| state with next = state.next + 1; moves = state.moves |> Set.union (matches |> Set.ofList) |}
            matches
            |> List.map (bfs state')
            |> List.reduce (fun s1 s2 ->
                {| state' with moves = s1.moves |> Set.union s2.moves; rating = s1.rating + s2.rating |})

let zeros = input[0] |> snd |> Set.toList
let trailheads =
    zeros
    |> List.map (fun coord -> bfs {| moves = Set.singleton coord; next = 1; rating = 0 |} coord)

trailheads
|> List.zip zeros
|> List.map (fun (k, st) -> k, st.moves |> Set.intersect (input[9] |> snd) |> Set.count)
|> List.sumBy snd

trailheads
|> List.map _.rating
// |> fun lst -> printfn $"%A{lst}"; lst
|> List.sum
