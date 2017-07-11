module ExploringPrelude where

succ8 = succ 8
min71 = min 7 1
--walk left list => not efficient
arraypp = [1,2,3] ++ [4,5,6]
headAppend = 'a' : "bc"
getByIdx = [1,2,3] !! 2
listIsNull = null []
listDrop = drop 3 [1,2,3,4,5]
listMinimum = minimum [1,6,8]
l = [1,2,3]
listProduct = product l
elemInfix = 4 `elem` [8,4]
alfpabet = ['A'..'z']
rangeBy = [5,7..25]
rnageByInfinite = take 12 [89,120..]
infiniteCycle = take 2 $ cycle [1,2,3]
repeat4 = take 4 $ repeat 9
replicate30 = replicate 4 30
listComprehension = [2*x|x<-[6,8..90]]
listComprehensionCondition = [2*x|x<-[6,8..90],x `mod` 7 == 3 ]
notEqual = x /= 13 where x = 6
multipleListsCombination = [x+y|x<-[1,2,3],y<-6 `replicate` 1]
combinationLength = length multipleListsCombination == 3*6

