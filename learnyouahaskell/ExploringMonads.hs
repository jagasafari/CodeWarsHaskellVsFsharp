module ExploringMonad where
import qualified Control.Monad as C
import Data.Monoid
import qualified Control.Monad.Writer as W

--(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x
bothJust = (Just 6) `applyMaybe` (\x -> Just (+8))
withNothing = Nothing `applyMaybe` (\x -> Just (++ ":)"))
class MyMonad m where
    myReturn :: a -> m a
    (.>>=) :: m a -> (a -> m b) -> m b
    (.>>) :: m a -> m b -> m b
    x .>> y = x .>>= (\_ -> y)
    fail :: String -> m a
    fail msg = error msg
instance MyMonad Maybe where
    myReturn x = Just x
    Nothing .>>= f = Nothing
    Just x .>>= f = f x
    fail _ = Nothing
returnMonadExample = return 9 :: Maybe Int
monadMaybe = (Just 5) >>= (\x -> return (x * 9))
monadMaybeNothing = Nothing >>= (\x -> return (x * 0))
x |> f = f x
pipeSum = 5 |> (5+)
pipeBool = False |> not
monadIgnoreBothJust = Just 5 >> Just 3
monadIgnoreLeftNothing = Nothing >> Just 3
monadIgnoreRightNothing = Just 6 >> Nothing

landLeft n (l, r)
    | abs (l+n-r) < 4 = Just (l+n,r)
    | otherwise = Nothing
landRight n (l,r)
    | abs (l-(n+r)) < 4 = Just (l,n+r)
    | otherwise = Nothing
obvousFailure = landLeft 1 (0,0) >>= landRight 1 >> Nothing >>= landLeft 5
obviousFailureDoNotation = do
    first <- landLeft 1 (0, 0)
    Nothing
    -- same as above
    _ <- Nothing 
    second <- landRight 1 first
    landLeft 5 second
alinedNestedMonads :: Maybe String 
alinedNestedMonads = Just 3 >>= (\x -> 
                     Just "!" >>= (\y -> 
                     Just $ show x ++ y))
doNotation = do
    x <- Just 3
    y <- Just "!"
    return $ show x ++ y
patternMatchingDoNotation = do
    (x:xs) <- Just [8,9,0]
    return x
--instance Monad [] where
--    return x = [x]
--    xs >>= f = concat $ map f xs
--    fail _ = []

listComprehenstion = [(n,c) | c <- ['a','b'], n <- [3,6]]
monadOperator = [3,6] >>= \n ->
                ['a','b'] >>= \c ->
                return (n,c)
monadDoNotation = do
    n <- [3,6]
    c <- ['a','b']
    return (n,c)
allowComputationToContinue = C.guard (79 > 8) >> return "continue"::[String]
resultOfGuardIsFalse = C.guard (7 > 9) >> return "this not happen"::[String]

guardDoNotation = do
    x <- [1..99]
    C.guard ('7' `elem` show x)
    return x
guardInMonadOperator =
    [1..99] >>= (\x -> C.guard ('7' `elem` show x) >> return x)
guardInListComprehension = 
    [ x | x <- [1..99], '7' `elem` show x]
myGuard::(C.MonadPlus m) => Bool -> m ()
myGuard False = C.mzero
myGuard True = return ()
class Monad m => MyMonadPlus m where
    mzero::m a
    mplus::m a -> m a -> m a
instance MyMonadPlus [] where
    mzero = []
    mplus = (++)
identityLaw f x = (return x >>= f) == (f x)
rightIdentityMonadLaw m = (m >>= return) == m
rightIdentityExample1 = ( Just "hallo" >>= return ) == Just "hallo"
rightIdentityExample2 = ( [1,2.34] >>= return) == [1,2.34]
associetivityLaw f g m = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
--(.) :: (a -> b) -> (c -> a) -> (c -> b)
--f . g = \x -> f (g x)
(.<=>)::(Monad m) => (b -> m c)->(a -> m b)->(a -> m c)
f .<=> g = (\x -> g x >>= f)
f x = [x,-x]
g x = [3*x,4*x]
monadicCompositionExample = f C.<=< g $ 3
monadicCompositionAssociativity = 
    left 3 == right 3
    where
        left = (f C.<=< g) C.<=< f
        right = f C.<=< (g C.<=< f)
compositionAssociativity f g h =    
    ((f . g) . h $ 7) == (f . (g . h) $ 7)
compositionIdentity f =
    (a == b) && (b == c)
    where
        a = (f . id $ 6)
        b = id . f $ 6
        c = f 6
monoidicCompositionIdentity f = do
    a <- f C.<=< return $ 7
    b <- return C.<=< f $ 7
    c <- f 7
    return ((a == b) && (b == c))
myWriter :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
myWriter (x, oldLog) f =
    let (z, newLog) = f x in (z, oldLog `mappend` newLog)
type Drink = String
type Price = Sum Int
addDrink::Drink->(Drink, Price)
addDrink "water" = ("cold", Sum 45)
addDrink _ = ("herb", Sum 15)
accompaningMonoidValue = ("water", Sum 89) `myWriter` addDrink
monoidInWriterType =
    W.runWriter ((return 3 :: W.Writer (Product Int) Int))
tellMod a b =
    [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
gcd'::Int -> Int -> W.Writer [String] Int
gcd' a b
    | b == 0 = do
        W.tell ["ending with: " ++ show a]
        return a
    | otherwise =  do
        W.tell $ tellMod a b
        gcd' b (a `mod` b)
efficientVsNotEfficientList a b c d =
    a++(b++(c++d)) == ((a++b)++c)++d
gcdNotEfficientListBuilding a b =
    gcd a b
    where
        gcd :: Int -> Int -> W.Writer [String] Int
        gcd a b 
            | b == 0 = do
                W.tell ["gcd is " ++ show a]
                return a
            | otherwise = do
                result <- gcd b $ a `mod` b
                W.tell $ tellMod a b
                return result
diffrenceList = \xs -> [1,2,3] ++ xs
diffrenceListForEmpty = \xs -> [] ++ xs
f `myappendOf2DiffLists` g = \xs -> f $ g xs
newtype MyDiffList a = MyDiffList { getDiffList :: [a] -> [a] }
toMyDiffList :: [a] -> MyDiffList a
toMyDiffList xs = MyDiffList (xs++)
fromMyDiffList:: MyDiffList a -> [a]
fromMyDiffList (MyDiffList a) = a []
instance Monoid (MyDiffList a) where
    mempty = MyDiffList (\xs -> [] ++ xs)
    (MyDiffList f) `mappend` (MyDiffList g) =
        MyDiffList (\xs -> f $ g xs)
gcdWithEfficientListBuilding :: Int -> Int -> W.Writer (MyDiffList String) Int
gcdWithEfficientListBuilding a b =
    gcd a b
    where
        gcd :: Int -> Int -> W.Writer (MyDiffList String) Int
        gcd a b         
            | b == 0 = do
                W.tell $ toMyDiffList ["result is " ++ show a]
                return a
            | otherwise = do
                result <- gcd b $ a `mod` b
                W.tell $ toMyDiffList $ tellMod a b 
                return result
gcdEfficientWithDiffList =
    mapM_ putStrLn $ fromMyDiffList $ snd $ W.runWriter $ gcdWithEfficientListBuilding 110 34
gcdEfficient =
    mapM_ putStrLn $ snd $ W.runWriter $ gcd' 110 34
gcdNotEfficident =
    mapM_ putStrLn $ snd $ W.runWriter $ gcdNotEfficientListBuilding 110 34
performanceOfWriterWithDiffList::Int -> W.Writer (MyDiffList String) ()
performanceOfWriterWithDiffList 0 = do
    W.tell $ toMyDiffList ["finished"]
performanceOfWriterWithDiffList count = do
    performanceOfWriterWithDiffList (count - 1)
    W.tell $ toMyDiffList [show count]
--instance Monad ((->)r) where
--    return x = \_ -> x
--    f >>= g = \w -> g (f w) w
exampleOfFunctionMonad :: Int -> Int
exampleOfFunctionMonad = do 
    a <- (*6)
    b <- (+8)
    return $ a + b
equivalentOfFunctionMonad = (+) <$> (*6) <*> (+8)
