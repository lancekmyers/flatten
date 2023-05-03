{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flat (Flat (..), Expr, flatInterp, flatInterp', interp, flatten) where

import Control.DeepSeq
import Control.Monad.State
import Data.Fix (Fix (..), Mu (..))
import Data.Functor.Foldable
import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Generic (GStorable)
import GHC.Generics

data ExprF a = AddF a a | SubF a a | MulF a a | Lit Int
  deriving (Functor, Foldable, Traversable, Generic, Generic1, NFData1)

instance (Storable a) => GStorable (ExprF a)

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
  deriving (Generic)

-- instance NFData Expr

type instance Base Expr = ExprF

instance Recursive Expr where
  project (Expr x) = Expr <$> project x

instance Num Expr where
  fromInteger x = Expr (embed (Lit $ fromInteger x))
  (Expr x) + (Expr y) = Expr . embed $ AddF x y
  (Expr x) * (Expr y) = Expr . embed $ MulF x y
  (Expr x) - (Expr y) = Expr . embed $ SubF x y

instance Show Expr where
  show (Expr e) = cata alg e
    where
      alg = show

newtype Ind = Ind Int
  deriving (Generic)

instance GStorable Ind

instance NFData Ind

instance NFData (ExprF Ind)

instance Show Ind where
  show (Ind i) = '%' : show i

type FlatExpr = Flat ExprF

showFlat :: (Storable (f Ind), Show (f Ind)) => Flat f -> String
showFlat (Flat xs) = unlines . map show . V.toList $ xs

len :: Storable (f Ind) => Flat f -> Int
len (Flat v) = V.length v

snoc :: Storable (f Ind) => Flat f -> f Ind -> Flat f
snoc (Flat v) x = Flat (V.snoc v x)

newtype Flat f = Flat (VS.Vector (f Ind))
  deriving (Generic)

deriving instance (NFData (f Ind)) => NFData (Flat f)

flatten ::
  forall t f.
  (Storable (f Ind), Base t ~ f, Traversable f, Recursive t) =>
  t ->
  Flat f
flatten x = execState (go x) (Flat [])
  where
    go :: t -> State (Flat f) Ind
    go x = do
      x' <- traverse go (project x)
      i <- gets len
      modify (flip snoc x')
      return (Ind i)

interp :: Expr -> Int
interp = cata alg . getExpr
  where
    alg (AddF x y) = x + y
    alg (SubF x y) = x - y
    alg (MulF x y) = x * y
    alg (Lit x) = x

flatFold :: forall f a. (Storable (f Ind), Functor f) => (f a -> a) -> Flat f -> a
flatFold func (Flat xs) = go (Ind $ V.length xs - 1)
  where
    go :: Ind -> a
    go (Ind i) = func (go <$> xs V.! i)

flatInterp = flatFold alg
  where
    alg (AddF x y) = x + y
    alg (MulF x y) = x * y
    alg (SubF x y) = x - y
    alg (Lit x) = x

flatInterp' :: FlatExpr -> Int
flatInterp' expr = case len expr of
  0 -> 0
  n -> flattenInterp' (Ind $ n - 1)
  where
    Flat xs = expr
    flattenInterp' :: Ind -> Int
    flattenInterp' (Ind i) = case xs V.! i of
      (AddF x y) -> flattenInterp' x + flattenInterp' y
      (SubF x y) -> flattenInterp' x - flattenInterp' y
      (MulF x y) -> flattenInterp' x * flattenInterp' y
      (Lit x) -> x
