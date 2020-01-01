module Data.Sluice

where

data Sluice a = Sluice [a]

empty :: Sluice a
empty = Sluice []
