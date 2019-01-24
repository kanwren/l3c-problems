-- Insertion into a Binary Tree

module Problem1 where

-- Data type for an ordered binary trees
data Bin a = EmptyTree | TreeNode a (Bin a) (Bin a)

insert :: Ord a => a -> Bin a -> Bin a
insert = undefined
