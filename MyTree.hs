module MyTree where
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node a left right
  | x > a  = Node a left (treeInsert x right)
  | x < a  = Node a (treeInsert x left) right

inTree :: Ord a => a -> Tree a -> Bool
inTree _ EmptyTree = False
inTree x (Node a left right)
  | x == a = True
  | x > a  = inTree x right
  | x < a  = inTree x left

listToTree :: Ord a => [a] -> Tree a
--listToTree []   = EmptyTree
--listToTree x:[] = Node x EmptyTree EmptyTree
--listToTree (x:xs) = treeInsert x (listToTree xs)
listToTree xs = foldr treeInsert EmptyTree xs