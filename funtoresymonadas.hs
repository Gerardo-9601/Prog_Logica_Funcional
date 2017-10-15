
class Functor f where
    fmap :: (a -> b) -> f a -> f b


instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing


class Functor f => Applicative f where 
pure :: a -> f a (<*>) :: f (a -> b) -> f a -> f b


class Monad m where 
(>>=) :: m a -> (a -> m b) -> m b 
return :: a -> m a   


instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
