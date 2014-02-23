data Either' e a = Left' e | Right' a deriving Show

instance Functor (Either' e) where
    fmap g (Right' a) = Right' (g a)
    fmap _ (Left' e)  = Left' e

-- returns function that expects one argument, e.g., if it compiled:
-- (.) (\_ -> ((->) String))) ((->) Integer)
-- commented below bc already defined in GHC.Base
-- instance Functor ((->) a) where
--     fmap = (.)

-----------------------------------------------------------------
-- Both Pair and (,) take 2 arguments to fulfill the value
-- constructor. The difference between Pair and (,) is that Pair
-- enforces its arguments to have the same type. The arguments to
-- (,) can have different types. This means that (,)'s data
-- constructor must have 2 parameters. This in turn means its
-- functor instance declaration would be `instance Functor ((,) e)'.
-- In its implementation of fmap,  `e' doesn't get mapped, but the
-- second argument to (,) does. Contrast this with Pair where its
-- fmap implementation maps both of its arguments.
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap g (Pair a b) = Pair (g a) (g b)

-- == `data (,) e a = (,) e a (already defined in GHC.Base)
data Comma e a = Comma e a deriving Show

instance Functor (Comma e) where
    fmap g (Comma e a) = Comma e (g a)

-----------------------------------------------------------------
