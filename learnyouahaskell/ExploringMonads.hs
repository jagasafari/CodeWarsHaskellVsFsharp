module ExploringMonad where

--(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x
bothJust = (Just 6) `applyMaybe` (\x -> Just (+8))
withNothing = Nothing `applyMaybe` (\x -> Just (++ ":)"))
