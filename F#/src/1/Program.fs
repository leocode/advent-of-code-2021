let countIncreasing list =
  list
  |> List.pairwise
  |> List.filter (fun (prev, curr) -> curr > prev)
  |> List.length

[<EntryPoint>]
let main argv =
  let measurements =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map int

  let part1Result =
    measurements
    |> countIncreasing
  
  let part2Result =
    measurements
    |> List.windowed 3
    |> List.map List.sum
    |> countIncreasing

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result
  
  0