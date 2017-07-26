module ExploringPrelude where
--sleep
import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map

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
myLength = sum [1|_<-[1,2,3]]
dlistComprehenstion = [[x | x <- xs, even x ]| xs <- [[1,2],[12,23,45]]]
zipped = zip [1..] ['g','f']
tringles = [(a,b,c)|c<-[1..10],b<-[1..c],a<-[1..b],c^2==a^2+b^2,a+b+c==24]
readEx = (read "[1,2,3]" ++ [1]) == [1,2,3,1]
readInt = read "(5,9)" :: (Int,Double)
enumList = [LT .. GT]
predesessorEnum = pred 'F'
allChars = [minBound::Char .. maxBound::Char]
boundOfTupleIfMembersBounded = maxBound::(Int, Char, Bool)
floatsAndIntTogether = fromIntegral (length [1,2,3]) + 3.9
multipleClassConstrant::(Num b, Integral a) => a -> b
multipleClassConstrant = fromIntegral
intMatching 7 = 67
intMatching x = x + 7
comprehenstionListMatching = [a+4| (a,8) <- [(1,8),(3,3)]]
bindWholeThing [] = "empty list"
bindWholeThing xs@(y:ys) = show xs ++ "with head: " ++ show y 
guardEx1 a b 
    | a < 18 = "low"
    | a/b < 20 = "ok"
    | a + b < 23 = "high"
    | otherwise = "bad"
insteadOfIfElse a b | a < b = b | otherwise = a
a `mycompare` b
    | a < b = LT
    | a == b = EQ
    | otherwise = GT
guardEx2 a b 
    | comp < 18 = "low"
    | comp < 20 = "ok"
    | comp < 23 = "high"
    | otherwise = "bad"
    where comp = a/b^2
          (skinny, normal, fat) = ("low", "normal", "fat")
initals::[Char]->[Char]->[Char]
initals first second = "first: " ++ [f] ++ " second: " ++ [s]
    where ((f:_),(s:_)) = (first, second)
initals2 first second = 
    let ((f:_),(s:_)) = (first, second)
    in "first: " ++ [f] ++ " second: " ++ [s]
cramLet = 6 + ( let b = 8 in 2*b)
multipleLetInLocalScope = [let a=7;f b=b^2 in (a, f a)]
letInListComprehension = [bmi | (x, y)<-[(7,9)],let bmi = x+y, bmi >9]
caseOfInsteadOfGuard li =
    case li of 
        [] -> error "error empty list"
        (x:_) -> "head: " ++ show x
caseOfLocal = "list " ++ case [] of [] -> "empty"
                                    [x] -> "singlee"
myMax [] = error "empty list"
myMax [x] = x
myMax (x:xs)
    | x > maxTail = x
    | otherwise = maxTail  
    where maxTail = myMax xs
myMaxBetter [] = error "empty list"
myMaxBetter [x] = x
myMaxBetter (x:xs) = max x $ myMaxBetter xs
myReplicate n x 
    | n <= 0 = []
    | otherwise = x:myReplicate (n-1) x
myTake n _ | n <= 0 = []
myTake _ [] = []
myTake n (x:xs) = x:myTake (n-1) xs
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
infiniteRec x = x:infiniteRec x
myElem _ [] = False
myElem el (x:xs)
    | el == x = True
    | otherwise = myElem el xs
myQuickSort [] = []
myQuickSort (x:xs) =
    smallSort ++ [x] ++ bigSort
    where
        smallSort = myQuickSort [a|a<-xs, a<=x]
        bigSort = myQuickSort [a|a<-xs, a>x]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y:myZipWith f xs ys
myFlip f = 
    let g x y = f y x
    in g
myFlipSimply f y x = f x y
myFilter _ [] = []
myFilter p (x:xs)
    | p x = x:myFilter p xs
    | otherwise = myFilter p xs
myQuickSort2 []  = []
myQuickSort2 (x:xs) =
    small ++ [x] ++ big
    where 
        small = myQuickSort2 $ filter (<=x) xs
        big = myQuickSort2 $ filter (>x) xs
lagestMod = head $ filter (\x -> x `mod` 666 == 0) [1000000,999999..]
foldlEx = foldl (\acc x -> acc + x) 0 [1,2,3,4]
mySumReadable = foldl (+) 0 [1,2,3]
myElemWithFoldl e = foldl (\acc x -> if x == e then True else acc) False
myMaximum1 :: (Ord a) => [a] -> a
myMaximum1 = foldl1 (\acc x -> if x > acc then x else acc)
myReverse2 :: [a] -> [a]
myReverse2 = foldl (\acc x -> x:acc) []
myFilterFold :: (a -> Bool) -> [a] -> [a] 
myFilterFold p = foldr (\x acc -> if p x then x:acc else acc) []
myIncreasingList = foldl1 (\acc x -> if x > acc then x else acc) [7,4,5,89]
myFlipListScan = scanl (flip (:)) [] [1,2,3]
--add polinomials 3x^2+5x+9, 10x^3+9, 8x^3+2x^2 -x -1
addingPolinomial = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,2,-1,-1]]
eachElemCount = map (\l@(x:xs)->(x, length l)) . group . sort $ [1,2,2,3,1,2]
baby = "I am a nig baby" \\ "baby"
sortByLength = sortBy (compare `on` length) [[1,2,3],[1],[],[2,4,3]]
charToInt = (map (ord . chr) $ [1,2,7,67]) == [1,2,7,67]
cipher5 = map chr $ map (+5) $ map ord "myMsg"
cipher5Composition = map (chr . (+5) . ord) "myMsg"
duplcatedValuesMap = 
    Map.fromListWith (++) $ map (\(k, v) -> (k,[v])) [(1,2),(1,3),(3,6)]
