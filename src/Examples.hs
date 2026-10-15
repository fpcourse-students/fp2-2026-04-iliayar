-- | Примеры для чтения и запуска в конкретной домашке.
module Examples where

import Defs
import Level1 qualified as L

natExample :: Nat
natExample = In (S (In (S (In (S (In Z))))))

listExample :: List Int
listExample = In (Cons 3 (In (Cons 4 (In (Cons 5 (In Nil))))))

countDown :: Coalgebra (ListF Integer) Integer
countDown n
  | n <= 0 = Nil
  | otherwise = Cons n (n - 1)

minusAlg :: Algebra (ListF Integer) Integer
minusAlg Nil = 0
minusAlg (Cons x r) = x - r

-- Эти запросы можно проверить после выполнения 1.3 и 1.4.
foldExample :: Integer
foldExample = L.cata minusAlg (to [1, 2, 3])

unfoldExample :: List Integer
unfoldExample = L.ana countDown 3

en :: Int -> L.Expr
en = In . L.constCtor

e3 :: L.Expr
e3 = en 3

ep35 :: L.Expr
ep35 = In (L.addCtor e3 (en 5))

emi35 :: L.Expr
emi35 = In (L.addCtor e3 (en (-5)))

emp357 :: L.Expr
emp357 = In (L.multCtor ep35 (en 7))

em7p35 :: L.Expr
em7p35 = In (L.multCtor (en 7) ep35)

nats :: Box (ListF Integer)
nats = Box 0 (\n -> Cons n (n + 1))

-- Та же последовательность, но другое скрытое состояние.
natsFromInterval :: Box (ListF Integer)
natsFromInterval = Box (100, 100) (\(lo, hi) -> Cons (hi - lo) (lo, hi + 1))

-- Ошибка для задачи 2.7: неподходящий элемент не означает конец потока.
brokenFilterS :: (a -> Bool) -> Stream a -> Stream a
brokenFilterS p (Stream next s) = Stream next' s
  where
    next' st = case next st of
      Done -> Done
      Skip st' -> Skip st'
      Yield x st' -> if p x then Yield x st' else Done
