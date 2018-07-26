module Route where



data Section   = Section {getA ::Int,getB::Int, getC::Int}    deriving (Show)
type RoadSystem = [Section]

-- 实例化一条道路系统
_hToLondon :: RoadSystem
_hToLondon =
    [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A |B |C deriving (Show)
type Path = [(Label ,Int)]
{-
举例来说，最开始我们的最佳路径是 [] 跟 []。
我们看过 Section 50 10 30 后就得到新的到 A1 的最佳路径为 [(B,10),(C,30)]，
而到 B1 的最佳路径是 [(B,10)]。
------- 抽象化 -----------
如果你们把这个步骤看作是一个函数，他接受一对路径跟一个 section，并产生出新的一对路径。
所以类型是 (Path, Path) -> Section -> (Path, Path)

A ------------a-----
      C  |          |
         |          c
         |          |        
B ------------b-----
-}
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c)
    = let
          priceA   = sum $ map snd pathA
          priceB   = sum $ map snd pathB
          priceAA  = priceA + a
          priceBA  = priceB + b + c
          priceBB  = priceB + b
          priceAB  = priceA + a + c
          newPathA = if priceAA < priceBA
              then (A, a) : pathA
              else (B, b) : (C, c) : pathB
          newPathB = if priceAB < priceBB
              then (A, a) : (C, c) : pathA
              else (B, b) : pathB
      in
          (newPathA, newPathB)


bestPath :: RoadSystem -> Path
bestPath roadSystem =
    let (bestPathA, bestPathB) = foldl roadStep ([], []) roadSystem
    in  if sum (map snd bestPathA) <= sum (map snd bestPathB)
            then bestPathA
            else bestPathB

-- show == toString(),  read = T (str)          
