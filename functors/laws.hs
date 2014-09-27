-- 2. 
-- Functor laws:
-- fmap id = id
-- fmap (g . h) = (fmap g) . (fmap h)

-- The following is an evil functor instance. It breaks the first
-- law. Mapping `id' over the List is not equal to calling the
-- List with `id'.

data List a = EmptyList | List [a] deriving (Show, Eq)

instance Functor List where
    fmap g (List [])   = List []
    fmap g (List (x:xs)) = List (g x : g x : fmap g xs)

-- fmap id (List ["functor", "foul"]) == id (List ["functor", "foul"])
-- False
--
-- fmap id (List ["functor", "foul"])
-- > List ["functor","functor","foul"]
-- id (List ["functor", "foul"])
-- > List ["functor","foul"]
