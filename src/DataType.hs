module DataType
    ( Point(..)
    , Shape'(Circle)
    , baseCircle
    , baseRectangle
    )
where

import           Data.Map

{-
class data {
    // 构造函数 Circle()
    Circle :: Float -> Float-> Float-> Shape
    Circle (Float Float Float) ;

    Rectangle (Float Float Float Float);
}
-}


{- 
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)


-- 求 Shape 的面积

▇▇▇▇▇▇▇▇▇ 通过函数调用时参数不同的模式匹配，来实现 函数“多态”
-- 但没有这种: f1 有f1得类型签名，f2有f2的类型签名。



surface :: Shape -> Float
surface (Circle _ _ r         ) = pi * r ^ 2  -- 要拿到一个类型（对象）里的值时，▇▇▇使用模式匹配
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
-}
--------------------------------------------------

data Point = Point Float Float
    deriving (Show)

    -- Circle 的类型签名已经确定、binding 。这地方不能再换一个签名
data Shape' = Circle Point Float | Rectangle Point Point
    deriving (Show)

baseCircle :: Float -> Shape'
baseCircle = Circle (Point 0 0)

baseRectangle :: Point -> Shape'        -- ▇▇▇▇▇▇ 显式的类型签名 是 Expected Type
baseRectangle = Rectangle (Point 0 0)   -- ▇▇▇▇▇▇▇▇▇ 推导出来的类型 是 Actual Type

{-
[C++]

baseShape (a,b,c) {return Circle(a,b,c); }
baseShape (a,b,c,d) {return Rectangle(a,b,c); }

▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇  Haskell 中每个符号都有明确的类型签名！ 
                不存在 同一个函数名，却可以接受不同类型的参数 （OO 语言中的函数重载）

                ▇▇▇▇▇ Shape 类型也更加灵活，可能是 Circle 形式、也可能是 Rectangle 形式
                 （ 这放在 Java 要表达这种逻辑 ，就得是 抽象类型-> 具体子类 了？ ）

        -- Haskell: 每个符号都有明确的参数类型和返回类型（若其为函数。即kind 不是 *）
                            或 恒定的值       （若其为varible，即kind 为 * ）
-}
------------------------ Record Syntax -------------------------------
data Person = Person {firstName ::String
                      ,lastName ::String
                      , age ::Int
                     } deriving (Show)


person1 = Person "wang" "jiaheng" 20
person2 = Person {firstName = "wang", lastName = "jiaheng", age = 24}

-- ▇▇▇▇▇▇▇ 这样，就自动生成了 firstName :: Person -> String 等函数。 可以直接从类型中读取其命名的“域”


----------------------  template -----------------------------------------
--------------------- Type Parameter ------------------------------------------
 -- 书中说的“type constructor" 相当于我们说的 template type-parameter
{-
▇▇▇▇没有 Maybe 3 这种东西，Maybe Int 是 Maybe<Int> ，是一个类型
                     Just 3 是一个类型为 Maybe Int 的值/对象
-}

-- ▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇ ▇▇▇▇▇▇▇ Haskell 里一个instance varible 的写法，总是 【构造函数名 params】
-------------  所以： Just 4 是一个instance，其类型为 Maybe<Int>

{-
data Maybe a = Nothing | Just a

★ Just a 这是函数调用啊（两个符号用空格在一起，则都处于函数调用）！ 
    说明 ▇▇▇▇▇▇▇ Just 是一个函数
★ 而 Nothing 是一个

template<a>
class Maybe {

    // 构造函数 Just :: a -> Maybe a
    Just(a var){ v1 = var; }

        [数据域]
    a v1;
}

Nothing :: Maybe a

-}

data Maybe a = Nothing | Just a


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a

    -- 实现内部对a 类型使用了 + 运算，则a 应有类型约束 Num
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

------------------ type synonyms :类型别名 -----------------------------
type MyStr = [Char]

-- 电话簿：姓名+号码
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pno pbook = (name, pno) `elem` pbook


type MyMap k v = [(k,v)]

type IntMap v = Map Int v

{-
如果参数是一个template，如何对其类型进行匹配？？

Shape 型参数，我可以用 Circle / Rectangle 来区分匹配 Shape 实例
———— 这和 C++里的 模板特化匹配是一个道理：
        非类型参数：可以根据特定值特化匹配；
        类型参数：可以根据具体类型进行特化匹配


-}

----------------- 递归地定义数据结构 --------------------------------
data List a = Empty
            | Cons {head::a, tail:: List a }
    deriving (Show)

list1 = 5 `Cons` Empty


-- 用一个运算符代替上面的 Cons 函数 (右结合、优先级为5)
infixr 5 :-:
data List' a = Empty'
             | a :-: List' a
    deriving (Show)

{-▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇
运算符函数与fixity 声明：
    infixr 6 :-:      // :-:这个运算符是右结合的，优先级为6
    infixl 7 *        // * 这个运算符是左结合的，优先级为7

一般的函数默认是 前缀调用的
foo :: Int -> Int ->Int
foo a b  = 定义
        调用时 foo a b
        可用中缀调用  a `foo` b
        
运算符函数默认是 中缀调用的
infixr 6 :-:
(:-:) :: Int -> Int -> Int
(:-:) a b = 定义
       调用时 a :-: b
       可用前缀调用 (:-:) a b
-}
