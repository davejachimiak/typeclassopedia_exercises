-- 1. Implement Functor instances for Either e and ((->) e).

data Either' e a = Left' e | Right' a deriving Show

instance Functor (Either' e) where
    fmap g (Right' a) = Right' (g a)
    fmap _ (Left' e)  = Left' e

-- (->) is the type constructor for functions. The first argument
-- to the constructor represents the function itself. The
-- application of (->) to some value represents the function
-- itself, while the application of the next variable in the
-- type constructor represents the application of that function
-- to an argument.
--
-- The implementation is commented below because it's already
-- defined in GHC.Base
--
-- instance Functor ((->) a) where
--     fmap = (.)

-----------------------------------------------------------------
-- 2. Implement Functor instances for ((,) e) and for Pair, defined as
--     data Pair a = Pair a a
--     Explain their similarities and differences.
--
-- Both Pair and (,) take 2 arguments to fulfill the value
-- constructor. The difference between Pair and (,) is that Pair
-- has a kind of * -> * and (,) has a kind of * -> * -> *.
-- This means that (,)'s type  constructor has 2 parameters.
-- The first of (,)'s parameters does not get mapped, but its
-- second argument does. Contrast this with Pair where its
-- fmap implementation maps both of its arguments.
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap g (Pair a b) = Pair (g a) (g b)

-- == `data (,) e a = (,) e a' (already defined in GHC.Base)
data Comma e a = Comma e a deriving Show

instance Functor (Comma e) where
    fmap g (Comma e a) = Comma e (g a)

-----------------------------------------------------------------
-- 3. Implement a Functor instance for the type ITree, defined as
--    data ITree a = Leaf (Int -> a) 
--                 | Node [ITree a]

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Show (ITree a) where
    show (Node xs) = "Node " ++ show xs
    show (Leaf _) = "Leaf (Int -> a)"

instance Functor ITree where
    fmap g (Node xs) = Node $ map (fmap g) xs
    fmap g (Leaf h) = Leaf $ g . h
--
-----------------------------------------------------------------
-- 4. Give an example of a type of kind * -> * which cannot be
--    made an instance of Functor (without using undefined).
--
--    TODO
--
-----------------------------------------------------------------
-- 5. Is this statement true or false?
--
-- The composition of two Functors is also a Functor.
--
-- If false, give a counterexample; if true, prove it by
-- exhibiting some appropriate Haskell code. 

-- The composition of two functors is a functor. Composition
-- applies the second function to a parameter,
-- to which the first function applies itself. Type
-- constructors that are functors take one argument, which is the
-- argument that is mapped in fmap.

