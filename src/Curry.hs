module Curry where

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _        []       = []
zipWith f []       _        = []
zipWith f (x : xs) (y : ys) = f x y : Curry.zipWith f xs ys

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x




-------------------------------------------------------------








