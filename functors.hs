data Either' e a = Left' e | Right' a deriving Show

instance Functor (Either' e) where
    fmap g (Right' a) = Right' (g a)
    fmap _ (Left' e)  = Left' e

-- returns function that expects one argument, e.g., if it compiled:
-- (.) (\_ -> ((->) String))) ((->) Integer)
-- commented below bc already defined in GHC.Base
-- instance Functor ((->) a) where
--     fmap = (.)
