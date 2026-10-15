module SpecLevel1 where

import Data.Functor.Contravariant (Contravariant (..))
import Defs
import Examples qualified as E
import Level1
import Test.Expr
import Test.Prelude

tests :: NamedTests
tests = nameTests 1 [testRead, testArrows, testCata, testAna, testExpr]

testRead :: Test
testRead = TestList
  [ TestCase $ assertEqual "меняется рекурсивный слот" (Cons 7 3) predictedLayer
  , TestCase $ assertEqual "слои с зёрнами, включая остановку"
      [Cons 3 2, Cons 2 1, Cons 1 0, Nil] predictedUnfold
  , TestCase $ assertEqual "результаты хвостов, от пустого к полному"
      [0, 3, -1, 2] predictedFold ]

testArrows :: Test
testArrows = TestList
  [ TestCase $ assertEqual "Functor меняет тип результата" "42" $
      appArrow1 (fmap show (Arrow1 (+ 40))) 2
  , TestCase $ assertEqual "Contravariant меняет тип входа" 42 $
      appArrow2 (contramap length (Arrow2 (+ 40))) "ab"
  , propertyToTest "Arrow1 identity" $ \(Fun _ f :: Fun Int Int) x ->
      appArrow1 (fmap id (Arrow1 f)) x === f x
  , propertyToTest "Arrow1 composition" $
      \(Fun _ h :: Fun Int Int) (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) x ->
        appArrow1 (fmap (g . f) (Arrow1 h)) x ===
        appArrow1 (fmap g (fmap f (Arrow1 h))) x
  , propertyToTest "Arrow2 identity" $ \(Fun _ f :: Fun Int Int) x ->
      appArrow2 (contramap id (Arrow2 f)) x === f x
  , propertyToTest "Arrow2 composition" $
      \(Fun _ h :: Fun Int Int) (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) x ->
        appArrow2 (contramap (f . g) (Arrow2 h)) x ===
        appArrow2 (contramap g (contramap f (Arrow2 h))) x ]

testCata :: Test
testCata = TestList
  [ TestCase $ assertEqual "пустой слой" 0 (cata sumAlg (to [] :: List Integer))
  , TestCase $ assertEqual "натуральное число" 3 (cata natAlg E.natExample)
  , TestCase $ assertEqual "оба рекурсивных слота и порядок"
      [1, 2, 3] (cata inorder threeNodes)
  , TestCase $ assertEqual "некоммутативная алгебра" 2 $
      cata E.minusAlg (to [1, 2, 3])
  , TestCase $ assertEqual "алгебра может не требовать хвост" 7 $
      cata (\case Nil -> 0; Cons x _ -> x) (In (Cons (7 :: Integer) infinite))
  , TestCase $ assertEqual "пустая сумма" 0 (sumList (to [] :: List Integer))
  , propertyToTest "сумма" $ \(xs :: [Integer]) -> sumList (to xs) === sum xs ]
  where
    sumAlg Nil = 0
    sumAlg (Cons x r) = x + r
    natAlg Z = 0 :: Integer
    natAlg (S r) = 1 + r
    inorder Leaf = []
    inorder (Branch l x r) = l ++ [x] ++ r
    leaf = In Leaf
    threeNodes = In (Branch (In (Branch leaf (1 :: Int) leaf)) 2
      (In (Branch leaf 3 leaf)))

testAna :: Test
testAna = TestList
  [ TestCase $ assertEqual "остановка сразу" (In Z) (ana natStep (0 :: Int))
  , TestCase $ assertEqual "NatF" E.natExample (ana natStep (3 :: Int))
  , TestCase $ assertEqual "ListF" [3, 2, 1] (from (ana E.countDown 3))
  , TestCase $ assertEqual "ветвящаяся форма" expectedTree (ana treeStep (2 :: Int))
  , TestCase $ assertEqual "ленивый бесконечный результат" [0..19] $
      take 20 (from (ana (\n -> Cons n (n + 1)) (0 :: Integer)))
  , TestCase $ assertEqual "непотребованный хвост" [7] $
      take 1 (from (ana (\() -> Cons (7 :: Int) infinite) ()))
  , TestCase $ assertEqual "отрицательная граница" [] (from (iota (-3)))
  , TestCase $ assertEqual "нулевая граница" [] (from (iota 0))
  , TestCase $ assertEqual "единичная граница" [1] (from (iota 1))
  , propertyToTest "диапазон включительно" $ forAll (chooseInteger (-20, 100)) $ \n ->
      from (iota n) === [1..n] ]
  where
    natStep n | n <= 0 = Z
              | otherwise = S (n - 1)
    treeStep n | n <= 0 = Leaf
               | otherwise = Branch (n - 1) n 0
    leaf = In Leaf
    expectedTree = In (Branch (In (Branch leaf 1 leaf)) 2 leaf)

testExpr :: Test
testExpr = TestList
  [ TestCase $ assertEqual "число" "3" (showExpr E.e3)
  , TestCase $ assertEqual "скобки" "((3+5)*7)" (showExpr E.emp357)
  , TestCase $ assertEqual "отрицательная константа" "(3+-5)" (showExpr E.emi35)
  , TestCase $ assertEqual "порядок операндов" "(7*(3+5))" (showExpr E.em7p35)
  , TestCase $ assertEqual "вычисление" 56 (evalExpr E.emp357)
  , TestCase $ assertEqual "отрицательное число" (-2) (evalExpr E.emi35)
  , TestCase $ assertEqual "Functor не меняет константу"
      (constCtor 7 :: ExprF Bool) (fmap even (constCtor 7 :: ExprF Int))
  , TestCase $ assertEqual "Functor меняет оба слота Add"
      (addCtor "1" "2") (fmap show (addCtor (1 :: Int) 2))
  , TestCase $ assertEqual "Functor меняет оба слота Mult"
      (multCtor "1" "2") (fmap show (multCtor (1 :: Int) 2))
  , propertyToTest "случайные выражения: вычисление" $ forAll genExpr $ \model ->
      evalExpr (buildExpr model) === evalModel model
  , propertyToTest "случайные выражения: печать" $ forAll genExpr $ \model ->
      showExpr (buildExpr model) === showModel model ]
