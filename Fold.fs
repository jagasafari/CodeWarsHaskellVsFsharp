let foldArray (array: int array) runs =
    let rec foldOnce = function
        | (l, r) when l > r -> r
        | (l, r) when l = r -> l
        | (l, r) -> array.[l] <- (array.[l] + array.[r]); foldOnce (l+1, r-1)
    let rec foldNTimes = function
        | (endIdx, 0) -> array.[0..endIdx]
        | (endIdx, c) -> foldNTimes (foldOnce (0, endIdx), (c-1))
    ((Array.length array) - 1, runs) |> foldNTimes 
printfn "%A" (foldArray [|1;2;3;4;5|] 1)
