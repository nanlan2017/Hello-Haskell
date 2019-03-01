module BinTree where

-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇因为没有 null，所以 为了匹配类型的任意值，需要定义一个 Empty 构造子
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
    deriving (Show)

{-▇▇▇▇▇▇▇▇ 递归结构的数据结构，你就只定义它的一个节点就好了。比如list,tree
template <A>
class Tree 
{

}
-}

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- 向tree 上插入一个元素：小就往左，大的往右
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x tree = case tree of
    EmptyTree                 -> singleton x
    Node y leftTree rightTree -> if x <= y
        then Node y (treeInsert x leftTree) rightTree
        else Node y leftTree (treeInsert x rightTree)

-- 检查一个节点是否在tree 上
treeElem :: (Ord a) => a -> Tree a -> Bool
-- 不能在case 语句里再嵌套guards, 得把case 语句的匹配情况分开写
treeElem x EmptyTree = False
treeElem x (Node y leftTree rightTree) | x == y    = True
                                       | x < y     = treeElem x leftTree
                                       | otherwise = treeElem x rightTree

-- 根据 List 构造 Tree                                       
generateTree :: (Ord a) => [a] -> Tree a
generateTree = foldr treeInsert EmptyTree
