open System
open System.IO

module Example =
    let given1 = """
        AAAA
        BBCD
        BBCC
        EEEC
        """

    let expected1 =
        [
            ('A', [(4, 10)])
            ('B', [(4, 8)])
            ('C', [(4, 10)])
            ('D', [(1, 4)])
            ('E', [(3, 8)])
        ]

    let total1 = 140

    let given2 = """
        OOOOO
        OXOXO
        OOOOO
        OXOXO
        OOOOO
        """

    let expected2 =
        [
            ('O', [(21, 36)])
            ('X', [(1, 4); (1, 4); (1, 4); (1, 4)])
        ]

    let total2 = 772

    let given3 = """
        RRRRIICCFF
        RRRRIICCCF
        VVRRRCCFFF
        VVRCCCJFFF
        VVVVCJJCFE
        VVIVCCJJEE
        VVIIICJJEE
        MIIIIIJJEE
        MIIISIJEEE
        MMMISSJEEE
        """

    let expected3 =
        [
            ('R', [(12, 18)])
            ('I', [(4, 8)])
            ('C', [(14, 28)])
            ('F', [(10, 18)])
            ('V', [(13, 20)])
            ('J', [(11, 20)])
            ('E', [(13, 18)])
            ('M', [(5, 12)])
            ('S', [(3, 8)])
        ]

    let total3 = 1930

module Problem =
    let given = File.ReadAllText "2024/input-12.txt"

let input =
    // Problem.given.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    Example.given1.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (_.ToCharArray() >> Array.toList)

let nRows = input.Length
let nCols = input[0].Length

type Direction = U | D | L | R

let move (r, c) = function
    | U -> (r - 1, c)
    | D -> (r + 1, c)
    | L -> (r, c - 1)
    | R -> (r, c + 1)

let moves coord =
    [R; D; L; U]
    |> List.map (move coord)

let isValid (r, c) =
    (r >= 0 && r < nRows) && (c >= 0 && c < nCols)

let countEdge plot neighbor =
    let nr, nc = neighbor
    let sr, sc = plot
    if neighbor |> isValid |> not then 1
    elif input[nr][nc] <> input[sr][sc] then 1
    else 0

let perim coords =
    coords
    |> List.map (fun plot -> moves plot
                             |> List.sumBy (countEdge plot))
    |> List.sum

let findNodes coords =
    coords
    |> List.filter (fun plot ->
        moves plot
        |> List.sumBy (countEdge plot) > 0)

let findEdges crop coords =
    let dirs = Set [R; D; L; U]
    let turn = function | U -> R | D -> L | L -> U | R -> D
    let (|Invalid|SameCrop|Ignore|) (r, c) =
        if not (isValid (r, c)) then Invalid
        elif input[r][c] = crop then SameCrop
        else Ignore

    let addTo plot set = set |> Set.add plot
    let rec loop plot dir visit seen edges =
        let neighbor = move plot dir
        match neighbor with
        | _ when seen |> Set.contains neighbor -> edges, seen
        | SameCrop ->
            loop neighbor dir (addTo neighbor seen) edges
        | _ ->
            dirs
            |> Set.remove dir
            |> Set.fold (fun (edges, seen) d ->
                loop neighbor d (addTo neighbor seen) (addTo neighbor edges))
                (edges, seen)
            // loop plot (turn dir) (addTo neighbor seen) (addTo plot edges)

    let head = coords |> List.head
    let first = Set.singleton head
    loop head R first first |> fst

let distinctPlots plot coords =
    coords
    |> List.fold (fun acc p ->
        let neighbors =
            moves p
            |> Set.ofList
            |> Set.filter isValid
            |> Set.filter (fun (r, c) -> input[r][c] = plot)

        let xs =
            acc
            |> List.indexed
            |> List.filter (fun (_, s) -> Set.intersect neighbors s |> Set.isEmpty |> not)
            |> List.map fst

        if xs.IsEmpty then
            Set.singleton p :: acc
        else
            (xs
            |> List.fold (fun mp idx -> Set.union mp acc[idx]) (Set.singleton p))
            ::
            (acc
            |> List.indexed
            |> List.filter (fun (idx, _) -> xs |> List.contains idx |> not)
            |> List.map snd)
        ) []
    |> List.map Set.toList

let garden =
    [
        for i in [0..input.Length - 1] do
            for j in [0..input[i].Length - 1] do
                yield (input[i][j], (i, j))
    ]

let cropsets =
    garden
    |> List.groupBy fst
    |> List.map (fun (k, v) -> (k, v |> List.map snd))

// Part 1
let perimeterCost =
    cropsets
    |> List.map (fun (k, v) -> (k, v |> distinctPlots k))
    |> List.map (fun (k, v) -> (k, v |> List.map _.Length, v |> List.map perim))
    |> List.map (fun (k, v, p) -> (k, List.zip v p))
    |> List.map (fun (k, v) -> (k, v |> List.map (fun (a, b) -> a * b)))
    |> List.map (fun (k, v) -> (k, List.sum v))
    |> List.sumBy snd


module Drawing =
    type Point = { X: int; Y: int }
    type PointT = int * int

    let private crossProduct (o: Point) (a: Point) (b: Point) =
        (a.X - o.X) * (b.Y - o.Y) - (a.Y - o.Y) * (b.X - o.X)

    let private grahamScan (points: Point list) =
        let basePoint = points |> List.minBy (fun p -> (p.Y, p.X))
        let sortedPoints =
            points
            |> List.filter ((<>) basePoint)
            |> List.sortBy (fun p -> atan2 (float (p.Y - basePoint.Y)) (float (p.X - basePoint.X)))

        let rec buildHull hull remainingPoints =
            match remainingPoints with
            | [] -> hull
            | p :: ps ->
                let rec removeNonLeftTurns hull =
                    match hull with
                    | [] | [_] -> hull
                    | b :: a :: rest when crossProduct a b p <= 0 -> removeNonLeftTurns (a :: rest)
                    | _ -> hull
                buildHull (p :: removeNonLeftTurns hull) ps

        buildHull [basePoint] sortedPoints

    let makePolygon (vertices: Set<PointT>) =
        let points = vertices |> Set.toList |> List.map (fun (y, x) -> { X = x; Y = y })
        grahamScan points

    let isPointInPolygon (polygon: Point list) (point: PointT) =
        let pt = { X = snd point; Y = fst point }
        let rec loop (vertices: Point list) (intersections: int) =
            match vertices with
            | [] | [_] -> intersections % 2 <> 0
            | v1 :: v2 :: rest ->
                let intersects =
                    (v1.Y > pt.Y) <> (v2.Y > pt.Y) &&
                    (pt.X < (v2.X - v1.X) * (pt.Y - v1.Y) / (v2.Y - v1.Y) + v1.X)
                loop (v2 :: rest) (if intersects then intersections + 1 else intersections)
        let vs = (List.last polygon) :: polygon
        loop vs 0

// Example usage
//let points = Set [(0, 0); (0, 4); (4, 0); (4, 4)]
let points = Set [(1, 2); (2, 2); (2, 3); (3, 3)]
let polygon = Drawing.makePolygon points
let testPoints = [(1, 1); (1, 3); (3, 1); (3, 3)]

let results = testPoints |> List.map (Drawing.isPointInPolygon polygon)
results |> List.iter (printfn "%A")

// Part 2
let exteriorEdges =
    cropsets
    |> List.map (fun (k, v) -> (k, v |> distinctPlots k))
    |> List.map (fun (k, v) -> (k, v |> List.map _.Length, v |> List.map findNodes))
    |> List.map (fun (k, a, n) -> (k, a, n |> List.map (findEdges k)))
    |> List.map (fun (k, a, n) -> (k, n |> List.zip a))

exteriorEdges
|> List.iter (fun (k, v) ->
    v
    |> List.iter (fun (area, edges) ->
        printfn $"{k}:\tarea: {area},\tedges: %A{edges}"))

// let interiorEdges =
//     exteriorEdges
//     |> List.filter (fun (k, v) -> v |> List.exists (fun (a, _) -> a > 2))
//     |> List.map (fun (k, v) ->
//         (k, v |> List.
//     |> List.map (fun (k, v, w) -> k, v |> List.zip w)

// For comparison let's see HyperNeutrino's solution, translated from Python to F#, to get the answer
// https://www.youtube.com/watch?v=KXwKGWSQvS0
