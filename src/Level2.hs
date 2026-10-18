-- | Домашка 4. Типы данных: уровень 2.
module Level2 where

import Data.Functor.Contravariant (Contravariant (..))
import Data.List (partition)
import Defs
import Level1 (ana, cata)
import MetaUtils (todo, todoImpl)


-- 2.1. Два изменения знака
--
-- Самостоятельно объявите полный Functor (Cont r). Параметр a дважды находится
-- слева от стрелки. Отображение должно менять передаваемый обработчик,
-- сохраняя число и порядок его вызовов. Проверьте законы функтора.

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    fmap f (Cont cont) = Cont $ \g -> cont $ \x -> g $ f x

-- 2.2. Композиция направлений
--
-- Для Compose f g самостоятельно напишите два полных инстанса:
-- Functor, если f и g ковариантны; Contravariant, если f контравариантен,
-- а g ковариантен. В каждом случае сами укажите необходимые ограничения.
-- Оба инстанса должны удовлетворять законам тождества и композиции.

newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose c) = Compose $ fmap (fmap f) c

instance (Contravariant f, Functor g) => Contravariant (Compose f g) where
    contramap f (Compose c) = Compose $ contramap (fmap f) c


-- 2.3. Одинаковый map, разные схемы
--
-- Реализуйте map над List двумя способами: свёрткой входной структуры
-- и развёрткой результата. Сохраните порядок и число элементов.
-- В каждой функции используйте указанную схему рекурсии; функция отображения
-- может менять тип элемента. cata и ana берутся из уровня 1.

cataMap :: (a -> b) -> List a -> List b
cataMap f =
  cata
    ( \case
        Nil -> In Nil
        Cons x xs -> In $ Cons (f x) xs
    )

anaMap :: (a -> b) -> List a -> List b
anaMap f =
  ana
    ( \case
        In Nil -> Nil
        In (Cons x xs) -> Cons (f x) xs
    )


-- 2.4. Сортировка через дерево
--
-- Определите phiTreeInorder и psiTreeBST. Алгебра выполняет симметричный обход
-- дерева; коалгебра по пустому списку даёт Leaf, иначе выбирает первый элемент
-- опорным, меньшие отправляет влево, остальные вправо. Повторы не теряются.
-- Форма TreeF выдана в Defs. partition может пригодиться.
-- hylo сначала принимает коалгебру, затем алгебру — как в главе 4.

hylo :: (Functor f) => Coalgebra f s -> Algebra f a -> s -> a
hylo psi phi = cata phi . ana psi

phiTreeInorder :: Algebra (TreeF a) [a]
phiTreeInorder = \case
  Leaf -> []
  Branch l x r -> l ++ [x] ++ r

psiTreeBST :: (Ord a) => Coalgebra (TreeF a) [a]
psiTreeBST = \case
  [] -> Leaf
  (pivot : xs) ->
    let (l, r) = partition (< pivot) xs
     in Branch l pivot r

hyloSort :: Ord a => [a] -> [a]
hyloSort = hylo psiTreeBST phiTreeInorder


-- 2.5. Доставляем обработчики к данным
--
-- ChurchList из Defs хранит полиморфную свёртку: сначала шаг Cons, затем базу Nil,
-- как у foldr. Реализуйте преобразования обычного списка в ChurchList и обратно.
-- Напишите appendChurch непосредственно через обработчики, без преобразования
-- аргументов в обычные списки. Элементы левого списка идут перед правым.

toChurch :: [a] -> ChurchList a
toChurch [] = ChurchList $ \_ nil -> nil
toChurch (x : xs) = ChurchList $ \cons nil -> cons x (runChurchList (toChurch xs) cons nil)

fromChurch :: ChurchList a -> [a]
fromChurch (ChurchList l) = l (:) []

appendChurch :: ChurchList a -> ChurchList a -> ChurchList a
appendChurch (ChurchList l) (ChurchList r) = ChurchList $ \cons nil -> l cons $ r cons nil


-- 2.6. Два скрытых состояния
--
-- Реализуйте zipBox: по запросу очередного слоя выдавайте пару элементов,
-- пока оба источника непусты. Сначала проверяется левый источник: если он
-- закончился, правый не запрашивается. При создании коробки шаги не выполняются.
-- Работайте пошагово, без промежуточных обычных списков или Fix.
-- Box, stepBox, boxFromList, boxToList и примеры источников выданы.

zipBox :: Box (ListF a) -> Box (ListF b) -> Box (ListF (a, b))
zipBox l r = Box (l, r) $ \case
  (l, r) -> case (stepBox l, stepBox r) of
    (Nil, _) -> Nil
    (_, Nil) -> Nil
    ((Cons x xs), (Cons y ys)) -> Cons (x, y) (xs, ys)


-- 2.7. Пропуск не означает конец
--
-- Прочитайте brokenFilterS в Examples. Подберите конечный список, на котором
-- unstream (brokenFilterS even (stream xs)) отличается от filter even xs.
-- Затем реализуйте правильный filterS: неподходящий Yield становится Skip,
-- исходный Skip сохраняется. Один шаг результата делает ровно один шаг источника;
-- искать следующий подходящий элемент рекурсией внутри шага не нужно.

skipCounterexample :: [Integer]
skipCounterexample = [2, 1, 2]

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next s) = Stream next' s
  where
    next' st = case next st of
      Done -> Done
      Skip st' -> Skip st'
      Yield x st' -> if p x then Yield x st' else Skip st'
