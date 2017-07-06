module CustomMonads where
import Data.Either

data EitherIO l r = EitherIO { runEitherIO :: IO (Either l r) }

a = Left "l"
b :: IO (Either [Char] b)
b = return a
c = EitherIO b
d = EitherIO $ return $ Left "l"
eitherIOLeft = EitherIO . return . Left
eitherIORight = EitherIO . return . Right

instance Functor (EitherIO a) where
    fmap f = EitherIO . fmap (fmap f) . runEitherIO 

myMap = fmap (const 5) 
leftEitherIO = myMap . eitherIOLeft $ "gfhji"     

