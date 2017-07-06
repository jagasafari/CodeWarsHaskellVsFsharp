module CustomMonads where
import Data.Either

data EitherIO l r = EitherIO { runEitherIO :: IO (Either l r) }

a = Left "l"
b :: IO (Either [Char] b)
b = return a
c = EitherIO b
d = EitherIO $ return $ Left "l"
e = EitherIO . return . Left

instance Functor (EitherIO a) where
    fmap f ex = wrapped 
        where
        unwrapped = runEitherIO ex
        fmapped = fmap (fmap f) unwrapped
        wrapped = EitherIO fmapped
