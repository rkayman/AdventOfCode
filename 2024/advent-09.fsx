open System.IO

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

let test = "2333133121414131402"

let given = File.ReadAllText "2024/input-09.txt"

let input = given.ToCharArray() |> Array.map (fun ch -> ch - '0' |> int) |> Array.toList

let initDiskMap blockCount=
    let capacity = blockCount |> Math.nextPrime
    ResizeArray<int option>(capacity)

let (|File|Free|) x = if x % 2 = 0 then File else Free

let layoutDisk denseInput=
    let disk = denseInput |> List.sum |> initDiskMap

    let rec loop state cnt xs =
        let fileId, diskIndex, fileMap = state
        match cnt, xs with
        | _, [] -> fileMap, disk

        | File, x::xs' ->
            let blocks = List.init x (fun _ -> Some fileId)
            disk.AddRange blocks
            loop (fileId + 1, diskIndex + x, (disk.Count - x, x)::fileMap) (cnt + 1) xs'

        | Free, x::xs' ->
            let blocks = List.init x (fun _ -> None)
            disk.AddRange blocks
            loop (fileId, diskIndex + x, fileMap) (cnt + 1) xs'

    loop (0, 0, []) 0 denseInput

let fileMap, diskMap = input |> layoutDisk

let displayDiskMap (disk: ResizeArray<int option>)=
    disk.ForEach (fun x -> if x.IsSome then printf $"{x.Value}" else printf ".")
    printfn ""

let rec compact first last =
    let freeIdx = diskMap.FindIndex(first, _.IsNone)
    let fileIdx = diskMap.FindLastIndex(last, _.IsSome)
    if freeIdx = -1 || fileIdx = -1 then ()
    elif fileIdx < freeIdx then ()
    else
        diskMap[freeIdx] <- diskMap[fileIdx]
        diskMap[fileIdx] <- None
        compact freeIdx fileIdx

compact 0 (diskMap.Count - 1)

let checksum () =
    let rec loop idx acc =
        if idx >= diskMap.Count then acc
        else
            match diskMap[idx] with
            | None ->
                loop (idx + 1) acc

            | Some fid ->
                let acc' = acc + ((int64 idx) * (int64 fid))
                loop (idx + 1) acc'

    loop 0 0

checksum ()

// Part 2
type Node = { index: int; length: int }
type Block = Free of Node | File of Node

let findFreeBlock fileStart size=
    let rec freeSize idx cnt =
        if idx + cnt >= diskMap.Count then Some cnt
        else
            match diskMap[idx + cnt] with
            | None -> freeSize idx (cnt + 1)
            | _ -> Some cnt

    let rec nextFree idx =
        if idx >= diskMap.Count then None
        elif idx >= fileStart then None
        else
            match diskMap[idx] with
            | None ->
                match freeSize idx 1 with
                | None -> None // no free block
                | Some len when len < size -> nextFree (idx + len)
                | Some len -> Free { index = idx; length = len } |> Some
            | _ -> nextFree (idx + 1)
    nextFree 0

let rec defrag files=
    match files with
    | [] -> ()

    | (fileIdx, fileLen)::xs ->
        // printf $"File (index = {fileIdx}, length = {fileLen}): "
        match findFreeBlock fileIdx fileLen with
        | None -> // unable to find a free block large enough or no more free blocks
            // printfn "No free block found"
            defrag xs

        | Some (Free { index = freeIdx }) ->
            for i in 0..(fileLen - 1) do
                diskMap[freeIdx + i] <- diskMap[fileIdx + i]
                diskMap[fileIdx + i] <- None

            // displayDiskMap diskMap
            defrag xs

        | Some _ -> failwith "Invalid state: defrag cannot move free blocks, only file blocks"

fileMap |> defrag

