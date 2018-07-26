module Locker where

-- 包名、module 路径倒是像 Java，但最后的一个 Module 像是 C++里的一个.h 里的资源，而不是 Java 里的一个类
import           Data.List                     as List
import           Data.Map                      as Map

-- Enum类型：两种状态
data LockerState = Taken | Free deriving (Show,Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState,Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup no lockermap = case Map.lookup no lockermap of
    Nothing -> Left "doesn't exist!"
    Just (state, code) ->
        if state /= Taken then Right code else Left "has been taken!"
