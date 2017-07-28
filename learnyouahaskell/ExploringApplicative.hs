module ExploringApplicative where

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
