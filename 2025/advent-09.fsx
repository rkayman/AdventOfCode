open System

module Problem =
    let example = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

    let file = System.IO.File.ReadAllText "2025/input-09.txt"

    let parse (input: string) =
        input.Trim().Split("\n")
        |> Array.map _.Split(',')
        |> Array.map (Array.map int64)
        |> Array.map (fun arr -> (arr.[0], arr.[1]))
        |> Array.toList

    let distinctPairs xs =
        let xs' = List.indexed xs
        [
            for p in xs' do
                let ys = Seq.skip (p |> fst |> (+) 1) xs
                for q in ys do
                    yield (snd p, q)
        ]
    
    let rectArea (p, q) =
        let x1, y1 = p
        let x2, y2 = q
        let width = abs (x2 - x1) + 1L
        let height = abs (y2 - y1) + 1L
        width * height, (p, q)
    
    let solve1 (input: string) =
        let points = parse input
        
        points
        |> distinctPairs
        |> List.map rectArea
        // |> List.sortByDescending fst
        |> List.maxBy fst
        |> fst
        
    // Part 2

    [<Struct>]
    type Point = { X: float; Y: float }
    module Point =
        let fromTuple (x: int64, y: int64) = { X = float x; Y = float y }
    
    [<Struct>]
    type Segment = { Start: Point; End: Point }
    module Segment =
        let fromTuple (p, q) = { Start = p; End = q }
    
    [<Struct>]
    type Rectangle = { First: Point; Second: Point }
    module Rectangle =
        let create (p: int64 * int64) (q: int64 * int64) =
            {
                First = Point.fromTuple p
                Second = Point.fromTuple q
            }
            
        let area (rect: Rectangle) =
            let width = abs (rect.Second.X - rect.First.X) + 1.0
            let height = abs (rect.Second.Y - rect.First.Y) + 1.0
            width * height, rect

        let vertices (rect: Rectangle) =
            let p1 = rect.First
            let p2 = rect.Second
            [
                p1
                { X = p1.X; Y = p2.Y }
                p2
                { X = p2.X; Y = p1.Y }
            ]
            
        let segments (rect: Rectangle) =
            let pts = vertices rect
            pts @ [List.head pts]
            |> List.pairwise
            |> List.map Segment.fromTuple
    
    type Polygon = Polygon of Point list
    module Polygon =
        let create (points: Point list) =
            if points.Length < 3 then
                failwith "Polygon must have at least 3 points"
            Polygon points
        
        let points (Polygon pts) = pts
        
        let segments (Polygon pts) =
            pts @ [List.head pts]
            |> List.pairwise
            |> List.map Segment.fromTuple
        
        let private rayCrossesSegment point segment =
            let { Start = p1; End = p2 } = segment
            (p1.Y > point.Y) <> (p2.Y > point.Y) &&
            (point.X < (p2.X - p1.X) * (point.Y - p1.Y) / (p2.Y - p1.Y) + p1.X)
                    
        let private isOnSegment point segment =
            let { Start = p1; End = p2 } = segment
            let crossProduct = (point.Y - p1.Y) * (p2.X - p1.X) - (point.X - p1.X) * (p2.Y - p1.Y)
            
            if crossProduct <> 0.0 then false
            else
                point.X >= min p1.X p2.X && point.X <= max p1.X p2.X &&
                point.Y >= min p1.Y p2.Y && point.Y <= max p1.Y p2.Y
            
        let private segmentsIntersect rs ps =
            let { Start = rectStart; End = rectEnd } = rs
            let { Start = polyStart; End = polyEnd } = ps

            let dir a b c =
                (c.Y - a.Y) * (b.X - a.X) - (b.Y - a.Y) * (c.X - a.X)
                
            let d1 = dir polyStart polyEnd rectStart
            let d2 = dir polyStart polyEnd rectEnd
            let d3 = dir rectStart rectEnd polyStart
            let d4 = dir rectStart rectEnd polyEnd

            ((d1 > 0.0 && d2 < 0.0) || (d1 < 0.0 && d2 > 0.0)) && 
            ((d3 > 0.0 && d4 < 0.0) || (d3 < 0.0 && d4 > 0.0))
            
        let private rectangleCrossSegment polygon rectangle =
            let rectSegments = Rectangle.segments rectangle            
            let polySegments = segments polygon

            rectSegments
            |> List.exists (fun rectSeg ->
                polySegments
                |> List.exists (segmentsIntersect rectSeg)
            )

        let containsPoint polygon point =
            let segments = segments polygon
            let onEdge = segments |> List.exists (isOnSegment point)

            if onEdge then true
            else
                segments
                |> List.filter (rayCrossesSegment point)
                |> List.length
                |> Int32.IsOddInteger
                
        let isRectangleContained polygon rectangle =
            rectangle
            |> Rectangle.vertices
            |> List.forall (containsPoint polygon)
            && not (rectangleCrossSegment polygon rectangle)

    let solve2 (input: string) =
        let points = parse input
        let polygon = 
            points 
            |> List.map Point.fromTuple 
            |> Polygon.create
        
        points
        |> distinctPairs
        |> List.map (fun (p, q) -> Rectangle.create p q)
        |> List.map (fun rect -> Polygon.isRectangleContained polygon rect, rect)
        |> List.filter fst
        |> List.map (snd >> Rectangle.area)
        |> List.sortByDescending fst
        // |> List.maxBy fst
        // 4605538168L, too high
        // 1292238828, too low
        

#time
Problem.file
// Problem.example
// |> Problem.solve1
|> Problem.solve2
|> List.take 5
|> List.iter (printfn "%A")
