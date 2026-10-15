module SpecLevel2 where

import Data.Functor.Contravariant (Contravariant (..), Op (..))
import Data.List qualified as List
import Defs
import Examples qualified as E
import Level2
import Test.Prelude hiding (Done)

tests :: NamedTests
tests = nameTests 2
  [testCont, testCompose, testMaps, testSort, testChurch, testZip, testSkip]

testCont :: Test
testCont = TestList
  [ TestCase $ assertEqual "результат другого типа и два вызова" "4242" $
      runCont (fmap (+ 2) (Cont (\k -> k (40 :: Int) ++ k 40))) show
  , TestCase $ assertEqual "порядок вызовов" ["3", "1", "4"] $
      runCont (fmap show (Cont (\k -> concatMap k [3, 1, 4 :: Int]))) (: [])
  , propertyToTest "Cont identity" $ \(xs :: [Int]) (Fun _ k :: Fun Int [Int]) ->
      runCont (fmap id (Cont (\h -> concatMap h xs))) k === concatMap k xs
  , propertyToTest "Cont composition" $
      \(xs :: [Int]) (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int)
        (Fun _ k :: Fun Int [Int]) ->
          let c = Cont (\h -> concatMap h xs)
          in runCont (fmap (g . f) c) k === runCont (fmap g (fmap f c)) k ]

testCompose :: Test
testCompose = TestList
  [ TestCase $ assertEqual "оба положительных слоя" (Just ["2", "3"]) $
      getCompose (fmap show (Compose (Just [2, 3 :: Int])))
  , TestCase $ assertEqual "внешний минус" [3, 2] $
      getOp (getCompose (contramap length (Compose (Op (map (+ 1)))))) ["ab", "x"]
  , propertyToTest "Compose Functor identity" $ \(xs :: Maybe [Int]) ->
      getCompose (fmap id (Compose xs)) === xs
  , propertyToTest "Compose Functor composition" $
      \(xs :: Maybe [Int]) (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) ->
        getCompose (fmap (g . f) (Compose xs)) ===
        getCompose (fmap g (fmap f (Compose xs)))
  , propertyToTest "Compose Contravariant identity" $ \(xs :: [Int]) ->
      getOp (getCompose (contramap id (Compose (Op reverse)))) xs === reverse xs
  , propertyToTest "Compose Contravariant composition" $
      \(xs :: [Int]) (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) ->
        let c = Compose (Op reverse)
        in getOp (getCompose (contramap (f . g) c)) xs ===
           getOp (getCompose (contramap g (contramap f c))) xs ]

testMaps :: Test
testMaps = TestList $ concatMap check [cataMap, anaMap]
  where
    check mapper =
      [ TestCase $ assertEqual "пустой map" [] (from (mapper show (to [] :: List Int)))
      , propertyToTest "смена типа, порядок, повторы" $ \(xs :: [Int]) ->
          from (mapper show (to xs)) === map show xs ]

testSort :: Test
testSort = TestList
  [ TestCase $ assertEqual "симметричный обход несортированного слоя"
      [3, 1, 2] (phiTreeInorder (Branch [3] (1 :: Int) [2]))
  , TestCase $ assertEqual "база обхода" ([] :: [Int]) (phiTreeInorder Leaf)
  , TestCase $ assertEqual "база коалгебры" (Leaf :: TreeF Int [Int]) (psiTreeBST [])
  , TestCase $ assertEqual "повторы идут вправо" (Branch [1] 2 [2, 3 :: Int]) $
      psiTreeBST [2, 1, 2, 3]
  , TestCase $ assertEqual "пустая сортировка" ([] :: [Int]) (hyloSort [])
  , TestCase $ assertEqual "все элементы одинаковы" [2, 2, 2 :: Int] (hyloSort [2, 2, 2])
  , propertyToTest "сортировка с отрицательными и повторяющимися элементами" $
      \(xs :: [Int]) -> hyloSort xs === List.sort xs ]

referenceChurch :: [a] -> ChurchList a
referenceChurch xs = ChurchList (\step z -> foldr step z xs)

testChurch :: Test
testChurch = TestList
  [ TestCase $ assertEqual "fromChurch от данного значения" [3, 1, 4 :: Int] $
      fromChurch (ChurchList (\step z -> step 3 (step 1 (step 4 z))))
  , propertyToTest "toChurch: некоммутативный потребитель" $ \(xs :: [Int]) ->
      runChurchList (toChurch xs) (:) [] === xs
  , propertyToTest "toChurch: другой тип результата" $ \(xs :: [Int]) ->
      runChurchList (toChurch xs) (\x r -> show x ++ "/" ++ r) "end" ===
      foldr (\x r -> show x ++ "/" ++ r) "end" xs
  , propertyToTest "обратимость по наблюдениям" $ \(xs :: [Int]) ->
      let restored = toChurch (fromChurch (referenceChurch xs))
      in runChurchList restored (:) [] === xs
  , propertyToTest "конкатенация и порядок" $ \(xs :: [Int]) ys ->
      runChurchList (appendChurch (referenceChurch xs) (referenceChurch ys)) (:) [] === xs ++ ys
  , TestCase $ assertEqual "левый пустой" [1, 2 :: Int] $
      runChurchList (appendChurch (referenceChurch []) (referenceChurch [1, 2])) (:) []
  , TestCase $ assertEqual "правый пустой" [1, 2 :: Int] $
      runChurchList (appendChurch (referenceChurch [1, 2]) (referenceChurch [])) (:) [] ]

testZip :: Test
testZip = TestList
  [ propertyToTest "конечные источники разных длин" $ \(xs :: [Int]) (ys :: [Bool]) ->
      boxToList (zipBox (boxFromList xs) (boxFromList ys)) === zip xs ys
  , TestCase $ assertEqual "бесконечные источники с разными состояниями" [(n, n) | n <- [0..9]] $
      take 10 (boxToList (zipBox E.nats E.natsFromInterval))
  , TestCase $ assertEqual "конечный источник ограничивает бесконечный" [(0, 'a'), (1, 'b')] $
      boxToList (zipBox E.nats (boxFromList ['a', 'b']))
  , TestCase $ assertBool "создание не вызывает коалгебры" $
      case zipBox (Box () (\() -> infinite) :: Box (ListF Int))
                  (Box () (\() -> infinite) :: Box (ListF Bool)) of Box _ _ -> True
  , TestCase $ assertEqual "пустой левый не запрашивает правый" ([] :: [(Int, Bool)]) $
      boxToList (zipBox (boxFromList []) (Box () (\() -> infinite)))
  , TestCase $ assertEqual "пустой правый" ([] :: [(Integer, Int)]) $
      boxToList (zipBox E.nats (boxFromList []))
  , TestCase $ assertEqual "получение головы не запрашивает следующий слой" [(7, 0)] $
      take 1 (boxToList (zipBox oneThenBottom E.nats)) ]
  where
    oneThenBottom = Box False (\seen -> if seen then infinite else Cons (7 :: Int) True)

-- Наблюдаем ровно n шагов, различая Skip и Yield, но не раскрывая тип состояния.
events :: Int -> Stream a -> [Maybe a]
events n _ | n <= 0 = []
events n (Stream next s) = case next s of
  Done -> []
  Skip s' -> Nothing : events (n - 1) (Stream next s')
  Yield x s' -> Just x : events (n - 1) (Stream next s')

testSkip :: Test
testSkip = TestList
  [ TestCase $ assertBool "контрпример действительно различает программы" $
      unstream (E.brokenFilterS even (stream skipCounterexample)) /= filter even skipCounterexample
  , propertyToTest "последовательность подходящих элементов" $ \(xs :: [Int]) ->
      unstream (filterS even (stream xs)) === filter even xs
  , TestCase $ assertEqual "неподходящий Yield даёт Skip" [Nothing, Just 2, Nothing, Just 4] $
      events 10 (filterS even (stream [1, 2, 3, 4 :: Int]))
  , TestCase $ assertEqual "исходные Skip сохраняются" [Nothing, Nothing, Nothing, Just 2] $
      events 10 (filterS even withSkips)
  , TestCase $ assertEqual "один Skip не запрашивает следующий шаг" [Nothing :: Maybe Int] $
      events 1 (filterS even (Stream (\() -> Skip infinite) ()))
  , TestCase $ assertEqual "отклонённый Yield не запрашивает следующий шаг" [Nothing :: Maybe Int] $
      events 1 (filterS even (Stream (\() -> Yield 1 infinite) ()))
  , TestCase $ assertEqual "продуктивный бесконечный поток" [0, 2..18] $
      take 10 (unstream (filterS even (stream [0 :: Integer ..]))) ]
  where
    withSkips = Stream next (0 :: Int)
    next 0 = Skip 1
    next 1 = Yield (1 :: Int) 2
    next 2 = Skip 3
    next 3 = Yield 2 4
    next _ = Done
