open System
open Common.ActivePatterns

[<EntryPoint>]
let main argv =
  let bins =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun line -> line.ToCharArray() |> Array.toList |> List.map string)
  
  let totalLines = bins |> List.length
  let firstLineLength = bins |> List.head |> List.length
  
  let part1Result =
    bins
    |> List.fold (fun counts line ->
      (counts, line)
      ||>List.map2 (fun nthBitCount nthBit -> if nthBit = "1" then nthBitCount + 1 else nthBitCount)
    ) (Array.create firstLineLength 0 |> Array.toList)
    |> (fun counts ->
      counts
      |> List.map (fun count ->
        let diff = totalLines - count
        
        if count > diff then
          ("1", "0")
        else
          ("0", "1")
      )
      |> List.unzip
      |> (fun lists -> [fst lists; snd lists])
      |> List.map (fun s -> Convert.ToInt32(String.concat "" s, 2))
      |> List.fold (*) 1
    )
  
  let findBinaryNumber whatValueToLeave (binaryNumbers: string list list) =
    let rec inner (bins: string list list) pos =
      if List.length bins = 1 then
        bins[0]
      else
        let totalLines = List.length bins
        
        let ones =
          bins
          |> List.sumBy (fun bin -> if bin[pos] = "1" then 1 else 0)
        
        let zeroes = totalLines - ones
        let leave = whatValueToLeave ones zeroes

        let filtered =
          bins
          |> List.filter (fun bin -> bin[pos] = leave)
        
        inner filtered (pos + 1)
      
    String.concat "" (inner binaryNumbers 0)

  let oxygenGeneratorRating = bins |> findBinaryNumber (fun ones zeroes -> if ones >= zeroes then "1" else "0")
  let co2ScrubberRating = bins |> findBinaryNumber (fun ones zeroes -> if ones < zeroes then "1" else "0")
  
  let part2Result =
    [oxygenGeneratorRating; co2ScrubberRating]
    |> List.map (fun s -> Convert.ToInt32(s, 2))
    |> List.fold (*) 1

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result
  
  0