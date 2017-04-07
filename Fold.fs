type IndexPosition = | Middle | SecondHalf of int | FirstHalf

let getIndexPosition len halfLen = function 
    | idx when idx < halfLen -> FirstHalf
    | idx when idx = halfLen && len%2=1 -> Middle
    | idx -> SecondHalf idx

let foldOnce getIdxPosition array lastIdx =
    let rec foldOnce (x:xs) acc = function
        | FirstHalf -> acc
        | Middle -> acc::[x]
        | SecondHalf idx -> 
            foldOnce 
                xs 
                (acc::[x+array.[idx]) 
                (getIdxPosition (idx-1))
    foldOnce array (Array.empty) lastIdx

let rec foldArray foldOnce array = function
    | 0 -> array | c -> foldArray (foldOnce array) (c-1)
    
let foldArray array runs =
    let len = Array.lenght array
    let foldOnce = foldOnce (getIndexPosition len (len/2))
    foldArray foldOnce array runs
