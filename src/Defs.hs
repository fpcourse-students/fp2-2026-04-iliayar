-- | Выданные формы данных и интерфейсы. Решения задач — в Level1–Level3.
{-# LANGUAGE UndecidableInstances #-}
module Defs where

newtype Fix f = In { out :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

type Algebra f a = f a -> a
type Coalgebra f s = s -> f s

data NatF r = Z | S r deriving (Eq, Show, Functor)
type Nat = Fix NatF

data ListF a r = Nil | Cons a r deriving (Eq, Show, Functor)
type List a = Fix (ListF a)

to :: [a] -> List a
to [] = In Nil
to (x : xs) = In (Cons x (to xs))

from :: List a -> [a]
from (In Nil) = []
from (In (Cons x xs)) = x : from xs

data TreeF a r = Leaf | Branch r a r deriving (Eq, Show, Functor)
type Tree a = Fix (TreeF a)

newtype ChurchList a = ChurchList
  { runChurchList :: forall r. (a -> r -> r) -> r -> r }

-- Тип состояния s известен только коалгебре внутри коробки.
data Box f where
  Box :: s -> (s -> f s) -> Box f

stepBox :: Functor f => Box f -> f (Box f)
stepBox (Box s next) = fmap (\s' -> Box s' next) (next s)

boxFromList :: [a] -> Box (ListF a)
boxFromList xs = Box xs \case
  [] -> Nil
  x : rest -> Cons x rest

boxToList :: Box (ListF a) -> [a]
boxToList box = case stepBox box of
  Nil -> []
  Cons x rest -> x : boxToList rest

data Step a s = Done | Skip s | Yield a s deriving (Eq, Show, Functor)

data Stream a where
  Stream :: (s -> Step a s) -> s -> Stream a

stream :: [a] -> Stream a
stream xs = Stream next xs
  where
    next [] = Done
    next (x : rest) = Yield x rest

unstream :: Stream a -> [a]
unstream (Stream next seed) = go seed
  where
    go s = case next s of
      Done -> []
      Skip s' -> go s'
      Yield x s' -> x : go s'

data Instruction = PushConstInstr Int | AddInstr | MultInstr
  deriving Eq

instance Show Instruction where
  show (PushConstInstr n) = show n
  show AddInstr = "+"
  show MultInstr = "*"

type ByteCode = [Instruction]
type Stack = [Int]

showByteCode :: ByteCode -> String
showByteCode = unwords . map show
