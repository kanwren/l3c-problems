-- Insertion into a Binary Tree

module Problem1 where

-- Data type for a unique ordered binary trees
data Bin a = EmptyTree
           | TreeNode a (Bin a) (Bin a)
           deriving (Show, Eq)

leaf :: a -> Bin a
leaf x = TreeNode x EmptyTree EmptyTree

-- Recursively insert element into ordered binary tree
insert :: Ord a => a -> Bin a -> Bin a
insert x EmptyTree = leaf x
insert x (TreeNode n lower higher) =
  case compare x n of
    -- Update data in case of non-standard Ord/Eq implementation over a
    EQ -> TreeNode x lower higher
    LT -> TreeNode n (insert x lower) higher
    GT -> TreeNode n lower (insert x higher)

