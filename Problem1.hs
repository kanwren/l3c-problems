-- Insertion into a Binary Tree

module Problem1 where

-- Data type for an ordered binary trees
data Bin a = EmptyTree | TreeNode a (Bin a) (Bin a)

singleton :: a -> Bin a
singleton x = TreeNode x EmptyTree EmptyTree

insert :: Ord a => a -> Bin a -> Bin a
insert x EmptyTree = singleton x
insert x (TreeNode n lower higher) =
  case compare x n of
    -- Update data in case of non-standard Ord/Eq implementation for a
    EQ -> TreeNode x lower higher
    -- Assuming the trees aren't unique:
    -- EQ -> TreeNode n (insert x lower) higher
    LT -> TreeNode n (insert x lower) higher
    GT -> TreeNode n lower (insert x higher)

