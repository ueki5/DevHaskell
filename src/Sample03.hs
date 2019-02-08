module Sample03 where

data Maybe' a = Just' a | Nothing' deriving (Show)
class Functor' m where
    fmap' :: (a -> b) -> m a -> m b
class Functor' m => Applicative' m where
    pure' :: a -> m a
    ap' :: m (a -> b) -> m a -> m b
class Applicative' m => Monad' m where
    bind' :: m a -> (a -> m b) -> m b
instance Functor' Maybe' where
    fmap' f (Just' a) = Just' (f a)
    fmap' f Nothing' = Nothing'
instance Applicative' Maybe' where
    pure' a = Just' a
    ap' (Just' f) (Just' a) = Just' (f a)
    ap' _ _ = Nothing'
plus::(Num a) => a -> a -> a
plus  x y = x + y
--b= plus <$> Just' 1 <*> Just' 2