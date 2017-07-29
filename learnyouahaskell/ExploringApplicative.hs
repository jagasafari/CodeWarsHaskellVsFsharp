module ExploringApplicative where
import Data.Monoid
import Control.Applicative

class (Functor f) => MyApplicative f where
    myPure :: a -> f a
    (<.*>) :: f (a -> b) -> f a -> f b

maybeApplicative = Just (*3) <*> Just 10
purifyFunction = pure (*3) <*> Just 10
purifyValue = Just (*3) <*> pure 10
overNothingValue = pure (++"ha") <*> Nothing
applyNothingFunction = Nothing <*> Just "str"

twoArgumentsStartingFunction = pure (*) <*> Just 9 <*> Just 9
twoArgumentsStartingFunctionNothingAsLastArgument = 
    pure (*) <*> pure 9 <*> Nothing
twoArgumentsStartingFunctionNothingAsFirstArgument = 
    pure (*) <*> Nothing <*> pure 7
(<.$>) :: (Functor f) => (a -> b) -> f a -> f b
f <.$> x = fmap f x
chainedApplicativeExample = (++) <$> Just "hallo " <*> Just "world"
instesdOfListComprehension = filter (>50) $ (*) <$> [6,9,7] <*> [9,7,8]
insteadOfApplicative = [ x*y | x <- [7,9,0], y <- [9,7,6]]
nonDeterministicAllCombinations = [(*),(+)] <*> [8,9,0,0] <*> [9,8,7]
pureTypeString = pure "cghj" :: [String]
pureTypeMaybe = pure "ghj" :: Maybe String
--instance Aplicative [] where
--    pure x = [x]
--    fs <*> xs = [ f x | f <- fs, x <- xs]
--instance Applicative IO where
--    pure = return
--    fIO <*> xIO = do
--        f <- fIO
--        x <- xIO
--        return $ f x
--instance Functor ((->) r) where
--    fmap f g = \x -> f $ g x
--instance Functor ((->) r) where
--    fmap f g = (.)
--instance Applicative ((->) r) where
--    pure x = \_ -> x
--    f <*> g = \x -> f x $ g x

functionAsFunctorAndApplicativeExample =
    (+) <$> (+3) <*> (*100) $ 5
functionAsFunctorAndApplicativeExampleList =
    (\x y z -> [x, y, z]) <$> (+3) <*> (*5) <*> (/2) $ 5

--instance Applicative ZipList where
--    pure x = ZipList $ repeat x
--    ZipList fs <*> ZipList xs = ZipList $ zipWith (\f x -> f x) fs xs
zipWithApplicativeExample = 
    getZipList $ max <$> ZipList [9,8,0] <*> ZipList [9,9..]
zipListApplicativeExampleTupleConstructor = 
    getZipList 
        $ (,,) 
        <$> ZipList [8,9..] <*> ZipList [8,9,0] <*> ZipList [9,0,8,9]

myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b
liftA2Maybe :: Maybe [Int]
liftA2Maybe = liftA2 (:) (Just 4) (Just [6])
sameAsLiftA2 :: Maybe [Int]
sameAsLiftA2 = (:) <$> Just 6 <*> Just [8]

mySequenceA :: (Applicative f) => [f a] -> f [a]
mySequenceA [] = pure []
mySequenceA (x:xs) = (:) <$> x <*> mySequenceA xs
sequenceAMaybe = sequenceA [Just 6, Just 7, Just 0]
sequenceAMaybeNothingCase = sequenceA [Just 7, Nothing]
sequenceANestedArrays = sequenceA [[9,0,8],[9,0,5]]

newtype MyZipListNewtype a = MyZipListNewtype { getMyZipList :: [a] }
newtype MyCharList = MyCharList { getCharList :: [Char]} deriving (Eq, Show)
newtype Pair b a = Pair { getTuple :: (a, b) }
instance Functor (Pair c) where
    fmap f (Pair (a,b)) = Pair (f a, b)
mapOverFstExample = getTuple $ fmap (*100) $ Pair (2, 100)
mapOverFstExample2 = fmap reverse $ Pair ("ghko", 9)

class MyMonoid m where
    myMempty :: m
    myMappend :: m -> m -> m
    myMconcat :: [m] -> m
    myMconcat = foldr myMappend myMempty

instance MyMonoid [a] where
    myMempty = []
    myMappend = (++)
    
newtype MyProduct a = MyProduct { getNum :: a }
    deriving (Show, Read, Eq, Ord, Bounded)

instance Num a => MyMonoid (MyProduct a) where
    myMempty = MyProduct 1
    MyProduct a `myMappend` MyProduct b = MyProduct (a * b)

multTwoNum = getProduct $ Product 3 `mappend` Product 7 
multIdentity = getProduct $ Product 3 `mappend` mempty
multThreeNums = getProduct $ Product 5 `mappend` Product 8 `mappend` Product 2
multListOfNums = getProduct . mconcat . map  Product $ [8,9,7,6]
    

