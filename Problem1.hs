-- Insertion into a Binary Tree

module Problem1 where

-- Data type for an ordered binary trees
data Bin a = EmptyTree | TreeNode a (Bin a) (Bin a)

singleton :: a -> Bin a
singleton x = TreeNode x EmptyTree EmptyTree

insert :: Ord a => a -> Bin a -> Bin a
insert x EmptyTree = singleton x
insert x tree@(TreeNode n lower higher) =
  case compare x n of
    EQ -> tree
    -- Assuming the trees aren't unique:
    -- EQ -> TreeNode n (insert x lower) higher
    LT -> TreeNode n (insert x lower) higher
    GT -> TreeNode n lower (insert x higher)

