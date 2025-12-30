open System
open System.Collections.Generic
open System.IO

module Example =
    let given = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."""

    let expected = 21

    let answer = """
012345678901234
.......S.......0
.......|.......1
......|^|......2
......|.|......3
.....|^|^|.....4
.....|.|.|.....5
....|^|^|^|....6
....|.|.|.|....7
...|^|^|||^|...8
...|.|.|||.|...9
..|^|^|||^|^|..0
..|.|.|||.|.|..1
.|^|||^||.||^|.2 
.|.|||.||.||.|.3
|^|^|^|^|^|||^|4
|.|.|.|.|.|||.|5"""


module Array =
    let get' i xs = Array.get xs i

module Problem =

    type Grid = (int * char) array array
    
    let parse (input: string) : Grid =
        input.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Seq.toArray
        |> Array.map Array.indexed

    let gridSize (grid: Grid) =
        grid.Length, grid[0].Length

    let startPos (grid: Grid) =
        0, grid[0] |> Array.find (fun (_, c) -> c = 'S') |> fst

    let findSplitters (grid: Grid) =
        grid
        |> Array.mapi (fun r cells ->
            cells
            |> Array.filter (fun (_, ch) -> ch = '^')
            |> Array.map (fun (c, _) -> (r, c)))
        |> Array.collect id
        |> Set.ofArray
    
    let solve1 (input: string) =
        // Tracks two things: (1) number of splits encountered and (2) visited positions
        // Ultimately determines how many unique splitters were hit
        let grid = parse input
        let rows, cols = gridSize grid
        let start = startPos grid
        let splits = findSplitters grid
        
        let outOfBounds (r, c) =
            r < 0 || r >= rows || c < 0 || c >= cols
        
        let rec loop pos (hits: Set<_>, visited: Set<_>) =
            if (outOfBounds pos) || (visited |> Set.contains pos) then
                hits, visited
            else
                let visited' = visited |> Set.add pos
                if splits |> Set.contains pos then
                    let hits' = hits |> Set.add pos
                    let left = (fst pos + 1, snd pos - 1)
                    let right = (fst pos + 1, snd pos + 1)
                    loop right (loop left (hits', visited'))
                else
                    let down = (fst pos + 1, snd pos)
                    loop down (hits, visited')
        
        loop start (Set.empty, Set.empty)
        
    /// Part 2: How many unique paths through the grid?
    
    let solve2 (input: string) =
        // Change traversal rules and counting logic to answer how many unique paths through the grid
        let grid = parse input
        let rows, cols = gridSize grid
        let start = startPos grid
        let splits = findSplitters grid 

        let moveLeft (r, c) = (r + 1, c - 1)
        let moveRight (r, c) = (r + 1, c + 1)
        let moveDown (r, c) = (r + 1, c)
        
        let isSplit pos =
            splits |> Set.contains pos

        let rec loop row cnt paths =
            if row >= rows then
                cnt
            else
                let cnt' = 
                    paths
                    |> List.sumBy snd
                
                // printfn $"Row %d{row}:\n\tCount so far: %d{cnt'}\n\tPaths: %A{paths}"
                // printfn $"Row %d{row}:\n\tCount so far: %d{cnt'}"
                let paths' =
                    paths
                    |> List.collect (fun (pos, cnt) ->
                        [
                            match moveDown pos with
                            | pos' when isSplit pos' ->
                                let left = moveLeft pos
                                let right = moveRight pos
                                yield! [ (left, cnt); (right, cnt) ]
                            | next ->
                                yield! [ next, cnt ]
                        ])
                    |> List.groupBy fst
                    |> List.map (fun (pos, grp) ->
                        let total = grp |> List.sumBy snd
                        (pos, total))

                // printfn $"\t%A{paths'}"
                loop (row + 1) cnt' paths'
        
        loop 0 0L [ (start, 1L) ]


// File.ReadAllText "2025/input-07.txt"
Example.given
|> Problem.solve1
|> fst
|> Set.count


File.ReadAllText "2025/input-07.txt"
// Example.given
|> Problem.solve2

