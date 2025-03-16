open System

let test = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

let given = """
.............4....O..........w....R...............
.................................f................
..............4...j......NW0......................
....................R..W..........................
...............R..................................
..................................................
v.......................f.......0W................
.....9L............l...N.........w................
....L....9.......ON........8......................
.1.........49L........f..0..N.....................
..........................V...l...................
..........4.......................................
.....................j...................3.....U..
....O.....U.......................................
........J......................l..................
.O....s.Q.......j.....l.....w..........F...q......
..................................................
.U.......................j..8.....................
................U...............................3.
2.............................J............3......
..............................F...................
.....s...R...........J..................F.........
.s......................x..........F.....q........
.......2.....Q........3........x..................
...........v......................u...............
..............v...........n......8............q...
.......f..................8........i..............
.5..................1n..............P.....i.......
............7............Q..................X.....
......5...p....................V..................
.................J..........nx............q.......
.......p............W...........................0.
......2.............p.5.....1....P................
......I.................7.X....i...P..............
............s.....r...w................V..........
...............or...6.................V...........
............................PS.7..................
..........o...........................S...........
...........5..............o..1.......n............
...........I.........r.......7.......6............
.................o.r...........X..................
................................x.........u.......
.........p..Q....2................................
.........v.................S.....................u
I...........................S.....6...............
..................................................
.......I..........................................
..................................................
.......................................6..........
.................................X................
"""

let grid =
    given.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map _.ToCharArray()
    |> List.map Array.toList

let maxRow = grid.Length
let maxCol = grid |> List.head |> List.length

let antennas =
    [
        for i, line in grid |> List.indexed do
            for j, ch in line |> List.indexed do
                if ch <> '.' then
                    yield ch, (i, j)
    ]
    |> List.groupBy fst
    |> List.map (fun (k, v) -> k, v |> List.map snd)

let allAntinodes =
    let all ((r1, c1), (r2, c2)) =
        let rec post cnt ((r1, c1), (r2, c2)) lst =
            let r, c = (2 + cnt) * r2 - (1 + cnt) * r1, (2 + cnt) * c2 - (1 + cnt) * c1
            if (0 <= r && r < maxRow) && (0 <= c && c < maxCol) then
                post (cnt + 1) ((r1, c1), (r2, c2)) ((r, c)::lst)
            else
                lst
        let rec pre cnt ((r1, c1), (r2, c2)) lst =
            let r, c = (2 + cnt) * r1 - (1 + cnt) * r2, (2 + cnt) * c1 - (1 + cnt) * c2
            if (0 <= r && r < maxRow) && (0 <= c && c < maxCol) then
                pre (cnt + 1) ((r1, c1), (r2, c2)) ((r, c)::lst)
            else
                post 0 ((r1, c1), (r2, c2)) lst
        pre 0 ((r1, c1), (r2, c2)) []

    antennas
    |> List.map (fun (k, v) ->
        k, v
           |> List.allPairs v
           |> List.filter (fun ((r1, c1), (r2, c2)) -> r1 <> r2 && c1 <> c2)
           |> List.distinctBy (fun ((r1, c1), (r2, c2)) ->
               if r1 < r2 then (r1, c1), (r2, c2) else (r2, c2), (r1, c1))
           |> List.collect all
           |> List.filter (fun (r,c) -> (0 <= r && r < maxRow) && (0 <= c && c < maxCol))
        )

let antinodes =
    antennas
    |> List.map (fun (k, v) ->
        k, v
           |> List.allPairs v
           |> List.filter (fun ((r1, c1), (r2, c2)) -> r1 <> r2 && c1 <> c2)
           |> List.distinctBy (fun ((r1, c1), (r2, c2)) ->
               if r1 < r2 then (r1, c1), (r2, c2) else (r2, c2), (r1, c1))
           |> List.collect (fun ((r1, c1), (r2, c2)) ->
               [(2 * r1 - r2, 2 * c1 - c2); (2 * r2 - r1, 2 * c2 - c1)])
           |> List.filter (fun (r,c) -> (0 <= r && r < maxRow) && (0 <= c && c < maxCol))
        )

let grid' = ResizeArray<ResizeArray<char>>()
for i in 0..maxRow - 1 do
    grid'.Add(ResizeArray<char>())
    for j in 0..maxCol - 1 do
        grid'[i].Add('.')

//antinodes
allAntinodes
|> List.iter (fun (_, v) ->
    v |> List.iter (fun (r, c) -> grid'[r][c] <- '#'))

antennas
|> List.iter (fun (k, v) ->
    v |> List.iter (fun (r, c) -> grid'[r][c] <- k))

grid'
|> Seq.iteri (fun i line -> printfn $"%d{i % 10}%s{String(line.ToArray())}")
[-1..maxCol - 1]
|> Seq.iter (fun i -> if i < 0 then printf " " else printf $"{i % 10}")
printfn ""

antinodes
|> List.collect snd
|> Set.ofList
|> Set.count
|> printfn "%d"

let antennaSet =
    antennas
    |> List.collect snd
    |> Set.ofList

allAntinodes
|> List.collect snd
|> Set.ofList
|> Set.union antennaSet
|> Set.count
|> printfn "%d"
