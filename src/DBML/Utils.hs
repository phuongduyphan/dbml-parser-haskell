{-# LANGUAGE OverloadedStrings #-}

module DBML.Utils
  ( allDifferent
  , subList
  )
where

import qualified Data.Set as Set

allDifferent :: (Eq a) => [a] -> Either a ()
allDifferent [] = Right ()
allDifferent (x : xs) = 
  if x `elem` xs then
    Left x
  else allDifferent xs

-- check whether list a is a subset of list b 
subList :: (Ord a) => [a] -> [a] -> Either a ()
subList a b = 
  if null listDiff then
    Right ()
  else 
    Left (head listDiff)
  where listDiff = Set.toList $ Set.difference (Set.fromList a) (Set.fromList b)