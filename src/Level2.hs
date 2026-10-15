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

todoImpl ''Functor ''Cont


-- 2.2. Композиция направлений
--
-- Для Compose f g самостоятельно напишите два полных инстанса:
-- Functor, если f и g ковариантны; Contravariant, если f контравариантен,
-- а g ковариантен. В каждом случае сами укажите необходимые ограничения.
-- Оба инстанса должны удовлетворять законам тождества и композиции.

newtype Compose f g a = Compose { getCompose :: f (g a) }

todoImpl ''Functor ''Compose
todoImpl ''Contravariant ''Compose


-- 2.3. Одинаковый map, разные схемы
--
-- Реализуйте map над List двумя способами: свёрткой входной структуры
-- и развёрткой результата. Сохраните порядок и число элементов.
-- В каждой функции используйте указанную схему рекурсии; функция отображения
-- может менять тип элемента. cata и ana берутся из уровня 1.

cataMap :: (a -> b) -> List a -> List b
cataMap = todo "2.3 cataMap"

anaMap :: (a -> b) -> List a -> List b
anaMap = todo "2.3 anaMap"


-- 2.4. Сортировка через дерево
--
-- Определите phiTreeInorder и psiTreeBST. Алгебра выполняет симметричный обход
-- дерева; коалгебра по пустому списку даёт Leaf, иначе выбирает первый элемент
-- опорным, меньшие отправляет влево, остальные вправо. Повторы не теряются.
-- Форма TreeF выдана в Defs. partition может пригодиться.
-- hylo сначала принимает коалгебру, затем алгебру — как в главе 4.

hylo :: Functor f => Coalgebra f s -> Algebra f a -> s -> a
hylo psi phi = cata phi . ana psi

phiTreeInorder :: Algebra (TreeF a) [a]
phiTreeInorder = todo "2.4 обход"

psiTreeBST :: Ord a => Coalgebra (TreeF a) [a]
psiTreeBST = todo "2.4 разбиение"

hyloSort :: Ord a => [a] -> [a]
hyloSort = hylo psiTreeBST phiTreeInorder


-- 2.5. Доставляем обработчики к данным
--
-- ChurchList из Defs хранит полиморфную свёртку: сначала шаг Cons, затем базу Nil,
-- как у foldr. Реализуйте преобразования обычного списка в ChurchList и обратно.
-- Напишите appendChurch непосредственно через обработчики, без преобразования
-- аргументов в обычные списки. Элементы левого списка идут перед правым.

toChurch :: [a] -> ChurchList a
toChurch = todo "2.5 toChurch"

fromChurch :: ChurchList a -> [a]
fromChurch = todo "2.5 fromChurch"

appendChurch :: ChurchList a -> ChurchList a -> ChurchList a
appendChurch = todo "2.5 appendChurch"


-- 2.6. Два скрытых состояния
--
-- Реализуйте zipBox: по запросу очередного слоя выдавайте пару элементов,
-- пока оба источника непусты. Сначала проверяется левый источник: если он
-- закончился, правый не запрашивается. При создании коробки шаги не выполняются.
-- Работайте пошагово, без промежуточных обычных списков или Fix.
-- Box, stepBox, boxFromList, boxToList и примеры источников выданы.

zipBox :: Box (ListF a) -> Box (ListF b) -> Box (ListF (a, b))
zipBox = todo "2.6 zipBox"


-- 2.7. Пропуск не означает конец
--
-- Прочитайте brokenFilterS в Examples. Подберите конечный список, на котором
-- unstream (brokenFilterS even (stream xs)) отличается от filter even xs.
-- Затем реализуйте правильный filterS: неподходящий Yield становится Skip,
-- исходный Skip сохраняется. Один шаг результата делает ровно один шаг источника;
-- искать следующий подходящий элемент рекурсией внутри шага не нужно.

skipCounterexample :: [Integer]
skipCounterexample = todo "2.7 контрпример"

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS = todo "2.7 filterS"
