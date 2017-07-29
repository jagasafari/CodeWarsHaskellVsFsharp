module ExploringMonad where

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
