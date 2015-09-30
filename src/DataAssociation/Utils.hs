module DataAssociation.Utils (

  preservingArg

) where

-- I guess in Haskell should already exist a way to do it,
-- but I don't know it

preservingArg :: (a -> b) -> a -> (a, b)
preservingArg f a = (a, f a)

