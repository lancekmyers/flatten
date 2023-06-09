{-# LANGUAGE BangPatterns #-}

import Control.DeepSeq
import Data.Monoid
import Data.Semigroup
import Flat
import Test.Tasty.Bench

example :: Expr
example = (go 15 0)

go :: Int -> Integer -> Expr
go i k
  | i <= 0 = fromInteger k
  | even i = (go (i - 1) (k + 1)) + (go (i - 2) (-2 * k))
  | odd i = (go (i - 1) (k + 1)) * (go (i - 2) (-2 * k))

main :: IO ()
main = do
  let example_flat = force flatten example
  print example
  defaultMain
    [ bench "tree walking" $ nf interp example,
      bench "flatten then eval " $ nf flatten example,
      bench
        "flat fold"
        $ nf flatInterp example_flat
    ]
