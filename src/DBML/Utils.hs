{-# LANGUAGE OverloadedStrings #-}

module DBML.Utils
  ( allDifferent
  )
where

allDifferent :: (Eq a) => [a] -> Either a ()
allDifferent [] = Right ()
allDifferent (x : xs) = 
  if x `elem` xs then
    Left x
  else allDifferent xs