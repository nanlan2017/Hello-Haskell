module MonadMaybe where

-- ************************************************************************ --
type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

x -: f = f x  --解构函数调用
    -- ghci>  （0，0）-: landLeft 1 -: landRight 1

    {- 加上调用时/后的值校验逻辑 -}
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
    | abs ((right + n) - left) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

    {- 这样，一次land 后结果是 Maybe Pole，不能作为下一次land 的参数！  没法串起来写了
    而显然，不能把 land 的类型声明改为  Birds -> Maybe Pole -> Maybe Pole
    --- 这样你总不能以 Maybe (0,0) 开始吧！ 也不符合 自然的语义 
    
    自然而然的， 需要 对 Monadic value 进行 接受普通参数->返回 Monadic value 的函数： 就是 >>=  -}
proc1 =
    return (0, 0) >>= landLeft' 3 >>= landRight' 4 >> Just (1, 1) >>= landLeft'
        2

routine :: Maybe Pole
routine = case landLeft' 1 (0, 0) of
    Nothing    -> Nothing
    Just pole1 -> case landRight' 4 pole1 of
        Nothing    -> Nothing
        Just pole2 -> case landLeft' 2 pole2 of
            Nothing    -> Nothing
            Just pole3 -> landLeft' 1 pole3
    {- Maybe 对>>=的实现： 碰到 Nothing 就传 Nothing,碰到 正确值就继续传递值 -}
