{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Flat where

import Control.Monad.State
import Data.Fix (Fix (..))
import Data.Functor.Foldable
import qualified Data.Vector as V

data ExprF a = AddF a a | SubF a a | MulF a a | Lit Int
  deriving (Functor, Foldable, Traversable)

instance Show (ExprF String) where
  show (Lit x) = show x
  show (AddF x y) = "(+ " ++ x ++ " " ++ y ++ ")"
  show (MulF x y) = "(* " ++ x ++ " " ++ y ++ ")"
  show (SubF x y) = "(- " ++ x ++ " " ++ y ++ ")"

instance Show (ExprF Ind) where
  show (Lit x) = "lit " ++ show x
  show (AddF x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
  show (MulF x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
  show (SubF x y) = "(- " ++ show x ++ " " ++ show y ++ ")"

newtype Expr = Expr {getExpr :: Fix ExprF}

instance Num Expr where
  fromInteger x = Expr (Fix (Lit $ fromInteger x))
  (Expr x) + (Expr y) = Expr . Fix $ AddF x y
  (Expr x) * (Expr y) = Expr . Fix $ MulF x y
  (Expr x) - (Expr y) = Expr . Fix $ SubF x y

instance Show Expr where
  show (Expr e) = cata alg e
    where
      alg = show

newtype Ind = Ind Int

instance Show Ind where
  show (Ind i) = '%' : show i

type FlatExpr = Flat ExprF

showFlat :: Show (f Ind) => Flat f -> String
showFlat (Flat xs) = unlines . map show . V.toList $ xs

len :: Flat f -> Int
len (Flat v) = V.length v

snoc :: Flat f -> f Ind -> Flat f
snoc (Flat v) x = Flat (V.snoc v x)

newtype Flat f = Flat (V.Vector (f Ind))

flat :: forall t f. (Base t ~ f, Traversable f, Recursive t) => t -> Flat f
flat x = execState (go x) (Flat [])
  where
    go :: t -> State (Flat f) Ind
    go x = do
      x' <- traverse go (project x)
      i <- gets len
      modify (flip snoc x')
      return (Ind i)

flatten :: Expr -> FlatExpr
flatten (Expr e) = execState (flatten' e) (Flat [])

flatten' :: (Fix ExprF) -> State FlatExpr (Ind)
flatten' (Fix (Lit x)) = do
  i <- gets len
  modify (flip snoc (Lit x))
  return $ Ind i
flatten' (Fix (AddF x y)) = do
  xi <- flatten' x
  yi <- flatten' y
  i <- gets len
  modify (flip snoc (AddF xi yi))
  return $ Ind i
flatten' (Fix (SubF x y)) = do
  xi <- flatten' x
  yi <- flatten' y
  i <- gets len
  modify (flip snoc (SubF xi yi))
  return $ Ind i
flatten' (Fix (MulF x y)) = do
  xi <- flatten' x
  yi <- flatten' y
  i <- gets len
  modify (flip snoc (MulF xi yi))
  return $ Ind i

interp :: Expr -> Int
interp = cata alg . getExpr
  where
    alg (AddF x y) = x + y
    alg (SubF x y) = x - y
    alg (MulF x y) = x * y
    alg (Lit x) = x

flattenInterp' :: FlatExpr -> Ind -> Int
flattenInterp' e@(Flat xs) (Ind i) = case xs V.! i of
  (AddF x y) -> flattenInterp' e x + flattenInterp' e y
  (SubF x y) -> flattenInterp' e x - flattenInterp' e y
  (MulF x y) -> flattenInterp' e x * flattenInterp' e y
  (Lit x) -> x

flatInterp :: FlatExpr -> Int
flatInterp expr = case len expr of
  0 -> 0
  n -> flattenInterp' expr (Ind $ n - 1)

e :: Expr
e = 1 + 2 - 4 + 0
