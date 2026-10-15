-- Независимая модель выражений: тестам не нужны имена конструкторов студента.
module Test.Expr where

import Defs
import Level1 qualified as L
import Test.QuickCheck

data Model = Number Int | Plus Model Model | Times Model Model
  deriving (Eq, Show)

genExpr :: Gen Model
genExpr = sized (gen . min 8)
  where
    gen 0 = Number <$> chooseInt (-20, 20)
    gen n = frequency
      [ (2, gen 0)
      , (2, Plus <$> gen (n `div` 2) <*> gen (n `div` 2))
      , (2, Times <$> gen (n `div` 2) <*> gen (n `div` 2)) ]

buildExpr :: Model -> L.Expr
buildExpr (Number n) = In (L.constCtor n)
buildExpr (Plus l r) = In (L.addCtor (buildExpr l) (buildExpr r))
buildExpr (Times l r) = In (L.multCtor (buildExpr l) (buildExpr r))

evalModel :: Model -> Int
evalModel (Number n) = n
evalModel (Plus l r) = evalModel l + evalModel r
evalModel (Times l r) = evalModel l * evalModel r

showModel :: Model -> String
showModel (Number n) = show n
showModel (Plus l r) = "(" ++ showModel l ++ "+" ++ showModel r ++ ")"
showModel (Times l r) = "(" ++ showModel l ++ "*" ++ showModel r ++ ")"

codeModel :: Model -> ByteCode
codeModel (Number n) = [PushConstInstr n]
codeModel (Plus l r) = codeModel l ++ codeModel r ++ [AddInstr]
codeModel (Times l r) = codeModel l ++ codeModel r ++ [MultInstr]
