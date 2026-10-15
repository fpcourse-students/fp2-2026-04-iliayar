-- | Домашка 4. Типы данных: обязательные задачи.
-- В заготовке 1.2 параметры пока не используются.
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Level1 where

import Data.Functor.Contravariant (Contravariant (..))
import Defs
import MetaUtils (todo, todoImpl)


-- 1.1. Один слой и рекурсия
--
-- Прочитайте Examples до запуска. Запишите ответы литералами и конструкторами,
-- не вызывая функции из условий. Все числа в этом задании имеют тип Integer,
-- кроме результата length, у которого тип Int.
--
-- a) fmap length (Cons 7 [10, 20, 30]). Какой параметр ListF меняет fmap?
-- b) Начав с зерна 3, выпишите результаты countDown до Nil включительно.
--    В Cons остаётся следующее зерно, а не раскрытый хвост.
-- c) Сворачиваем список [1, 2, 3] алгеброй minusAlg. Запишите результаты
--    для пустого хвоста, [3], [2, 3], [1, 2, 3] именно в этом порядке.

predictedLayer :: ListF Integer Int
predictedLayer = todo "1.1 слой"

predictedUnfold :: [ListF Integer Integer]
predictedUnfold = todo "1.1 развёртка"

predictedFold :: [Integer]
predictedFold = todo "1.1 свёртка"


-- 1.2. Направления стрелки
--
-- В Int -> b параметр b положителен: меняем результат. Завершите Functor Arrow1.
-- В a -> Int параметр a отрицателен: меняем вход. Самостоятельно напишите полный
-- инстанс Contravariant Arrow2. Оба отображения должны удовлетворять законам
-- тождества и композиции из главы 4 конспекта.

newtype Arrow1 b = Arrow1 { appArrow1 :: Int -> b }
newtype Arrow2 a = Arrow2 { appArrow2 :: a -> Int }

instance Functor Arrow1 where
  fmap f (Arrow1 g) = Arrow1 (\x -> todo "1.2 результат")

todoImpl ''Contravariant ''Arrow2


-- 1.3. Слой становится результатом
--
-- Дополните cata: в каждом рекурсивном слоте должен оказаться результат свёртки
-- подструктуры. Затем самостоятельно напишите sumList через cata.
-- Сумма пустого списка — 0. cata должен работать с любым Functor, не только ListF.

cata :: Functor f => Algebra f a -> Fix f -> a
cata phi (In layer) = phi (fmap (todo "1.3 рекурсивный вызов") layer)

sumList :: Num a => List a -> a
sumList = todo "1.3 сумма"


-- 1.4. Зерно становится слоем
--
-- Реализуйте ana для любого Functor. Затем через ana постройте iota n:
-- список от 1 до n включительно, при n <= 0 — пустой список.
-- Тип Integer позволяет увеличивать зерно без переполнения.
-- Определения ana и iota напишите самостоятельно.

ana :: Functor f => Coalgebra f s -> s -> Fix f
ana = todo "1.4 ana"

iota :: Integer -> List Integer
iota = todo "1.4 iota"


-- 1.5. Выражение — неподвижная точка
--
-- Выделите форму исходного рекурсивного типа:
--   data Expr = Const Int | Add Expr Expr | Mult Expr Expr
-- Замените временную декларацию ExprF своей, сохранив Eq, Show, Ord и Functor.
-- Адаптеры constCtor, addCtor, multCtor строят соответствующие слои для тестов
-- и примеров независимо от выбранных имён конструкторов.
--
-- Через cata реализуйте evalExpr и showExpr. Число печатается через show;
-- каждая операция — в скобках, без пробелов: ((3+5)*7), (3+-5).
-- Вложенность и порядок операндов должны сохраняться, упрощать выражения не нужно.

data ExprF r = ReplaceMeWithSomething
  deriving (Eq, Show, Ord, Functor)

type Expr = Fix ExprF

constCtor :: Int -> ExprF r
constCtor = todo "1.5 Const"

addCtor :: r -> r -> ExprF r
addCtor = todo "1.5 Add"

multCtor :: r -> r -> ExprF r
multCtor = todo "1.5 Mult"

evalExpr :: Expr -> Int
evalExpr = todo "1.5 вычисление"

showExpr :: Expr -> String
showExpr = todo "1.5 печать"
