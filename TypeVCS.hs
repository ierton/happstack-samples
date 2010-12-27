{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

class Storable a where
    type Patch a :: *
    type History a :: *
    revert :: a -> History a -> Int -> (a, History a)
    same :: Patch a -> Patch a -> Bool
    update :: a -> History a -> Patch a -> (a, History a)

{-
-- Example
data Tree a = Node (Tree a) (Tree a) | Leaf a

data TreeStep = Left | Right
type family Location a :: *
type instance Location (Tree a) = [TreeStep]
 
instance (Storable a) => Storable (Tree a) where
    type Patch (Tree a) = (Location (Tree a), Patch a)
    type History (Tree a) = [Patch a]
    update = undefined
    revert = undefined
-}


-- Example2
data Board a = Board {
    area :: [[a]]
    } deriving(Show)

data BoardPatch = BoardPatcha {
    } deriving(Show)

updateBoard 
type Id = Int
    
instance (Storable a) => Storable (Board a) where
    type Patch (Board a) = (Id, (Int, Int), Patch a)
    type History (Board a) = [Patch a]
    (id1,_,_) `same` (id2,_,_) = id1==id2
    revert = undefined
    update b hist p = undefined



