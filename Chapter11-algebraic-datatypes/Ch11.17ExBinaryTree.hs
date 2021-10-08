module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
-- insert' b Leaf = Node Leaf b Leaf
-- insert' b (Node left a right)
--         | b == a = Node left a right
--         | b < a  = Node (insert' b left) a right
--         | b > a  = Node left a (insert' b right)

-- Write map for BinaryTree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = --see #learn-haskell
  Node (mapTree f left) (f a) (mapTree f right) -- pattern match with BinaryTree; f needs to be applied to a and not a tree

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


-- >>> mapOkay 
-- "yup okay!"
--

-- >>> mapTree (+1) testTree'
-- Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
--
-- >>> mapTree (+1) Leaf
-- Leaf
--


-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

-- >>> testPreorder
-- Preorder fine!
--

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."
-- >>> testInorder
-- Inorder fine!
--

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- >>> testPostorder
-- Postorder fine!
--


main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Write foldr for BinaryTree   see https://en.wikibooks.org/wiki/Haskell/Lists_III
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree = foldr f b (inorder tree)  -- see earlier that a list was developed from a inorder  (Node left a right)

-- notes--
-- foldr :: (a -> b -> b) -> b -> [a] -> b -- compare foldr with foldTree & q2 a list was developed from a inorder (Node left a right)
-- foldr f acc []     = acc
-- foldr f acc (x:xs) = f x (foldr f acc xs)