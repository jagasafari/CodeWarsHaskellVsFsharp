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
