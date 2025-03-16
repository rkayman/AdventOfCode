open System

module Example =
    let given1 = "0 1 10 99 999"
    let given2 = "125 17"

module Problem =
    let given = "6563348 67 395 0 6 4425 89567 739318"

let applyRules stone=
    if stone = 0L then [1L]
    else
        match stone |> string with
        | s when s.Length % 2 = 0 ->
            let mid = s.Length / 2
            [s.Substring(0, mid); s.Substring(mid)]
            |> List.map int64
        | _ -> [stone * 2024L]

let rec blink numBlinks stones =
    if numBlinks < 1 then stones
    else
        let stones' = stones |> List.collect applyRules
        blink (numBlinks - 1) stones'

let input =
    // Example.given2.Split(' ', StringSplitOptions.TrimEntries) |> List.ofArray
    Problem.given.Split(' ', StringSplitOptions.TrimEntries)
    |> List.ofArray
    |> List.map int64

input
|> blink 25
|> List.length

// Part 2
// wants us to run this for 75 times, which blows up the brute force solution above
// @HyperNeutrino's solution is much more efficient (https://www.youtube.com/watch?v=pVfsmQSlVOQ)
// The key part is to use memoization to avoid recalculating the same values over and over
// The memoization is done by using a dictionary to store the results of the previous calculations
// The dictionary is passed as an argument to the recursive function, and the function checks if the value is already in the dictionary before calculating it
// If the value is already in the dictionary, the function returns the value from the dictionary instead of recalculating it
// This way, the function only calculates each value once, and the performance is greatly improved
open System.Collections.Generic

module Math =
    let isPrime n =
        let upperBound = n |> float |> sqrt |> int
        let rec loop i =
            if i > upperBound then true
            elif n % i = 0 then false
            else loop (i + 1)
        loop 2

    let isPrime2 n=
        if n <= 1 then false
        elif n <= 3 then true
        elif n % 2 = 0 || n % 3 = 0 then false
        else
            let rec loop i v =
                match i with
                | i when i * i > v -> true
                | i when v % i = 0 || v % (i + 2) = 0 -> false
                | _ -> loop (i + 6) v
            loop 5 n

    let nextPrime n =
        if n <= 1 then 2
        else
            let rec findNext i =
                if isPrime2 i then i
                else findNext (i + 2)
            let start = if n % 2 = 0 then n + 1 else n + 2
            findNext start

let prime = Math.nextPrime 1_000_000_000
let cache = Dictionary<int * int64, int64>(prime)

let memoize f =
    let rec memo param =
        match cache.TryGetValue(param) with
        | true, res -> res
        | _ ->
            let res = f memo param
            cache.Add(param, res)
            res
    memo

let quickBlink f (blinks, stone) =
    if blinks = 0 then 1L
    elif stone = 0L then
        f ((blinks - 1), 1L)
    else
        match string stone with
        | s when s.Length % 2 <> 0 ->
            f ((blinks - 1), (stone * 2024L))
        | s ->
            let mid = s.Length / 2
            f ((blinks - 1), (int64 s[0..mid-1]))
            + f ((blinks - 1), (int64 s[mid..]))

let qb = memoize(quickBlink)

input
|> List.map (fun stone -> qb (75, stone))
|> List.sum


