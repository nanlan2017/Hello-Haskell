module TypeclassOpedia where
{-
instance Functor  where
    // fmap :: (a->b) -> f a -> f b  //【首先替换f、得到类型签名】

    =====================>  //【实现该类型签名的函数】
-}
-- 3.2 练习 -----------------------------------------------------------------------------------------
-- "type constructor"，即template，是Functor (泛型容器)。
    -- 如 [], Tree , Maybe


-- 1. Implement Functor instances for Either e and ((->) e).
{-
 instance Functor (Either e) where
    【首先替换f、得到类型签名】
    // fmap :: (a->b) -> f a -> f b   
    // fmap :: (a->b) -> (Either e a) -> (Either e b)   // 不改变 Left 中的类型，而改变 Right 中的类型
    =====================>
    【实现该类型签名的函数】
    // a , b, e :: 代表一个类型，如 Int ,Char
    // g        :: 一个函数，其类型签名为 a->b

    // err 为e 类型的值，x 为a 类型的值
    // Left err :: 一个值，类型为 Either e a
    // Right x  :: 一个值，类型为 Either e a

    // 

    fmap g (Left err) = Left err    // Left err 的类型为 Either e $err     // $err 代表err 的类型
    fmap g (Right x) = Right g x 
-}
-------------------------------------------
--  ((->) r) 注册Functor
{-
instance Functor (r->) where
    【首先替换类型、得到类型签名】
    // fmap :: (a->b) -> f a -> f b
    // fmap :: (a->b) -> (r->a) -> (r-> b)    // 这里当然写a 了，这样才符合fmap 的类型定义
    =====================>
    【实现该类型签名的函数】
    fmap g f = g.f
    // fmap = (.)
-}
-------------------------------------------

-- Implement Functor instances for ((,) e) 
-- and for Pair, defined as data Pair a = Pair a a   // 这是递归定义的数据结构？？
-- Explain their similarities and differences.
{-
instance Functor (e,) where
    【首先替换类型、得到类型签名】
    // fmap :: (a->b) -> f a -> f b
    fmap :: (a->b) -> (e,a) -> (e,b)
    =====================>
    【实现该类型签名的函数】
    fmap g (x,y) = (x, g y)

-------------------------------------------
instance Functor Pair where
    // fmap :: (a->b) -> f a -> f b  //【首先替换类型、得到类型签名】
    fmap :: (a->b) -> (Pair a) -> (Pair b)
    =====================>  //【实现该类型签名的函数】
    fmap g (Pair )
-}

{-
Implement a Functor instance for the type ITree, defined as
data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Functor ITree where
     // fmap :: (a->b) -> f a -> f b  //【首先替换类型、得到类型签名】
     fmap:: (a->b) -> ITree a -> ITtree b
     ==========================>
     fmap g Leaf (Int->x)               

-}