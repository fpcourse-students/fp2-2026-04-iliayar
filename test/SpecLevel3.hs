module SpecLevel3 where

import Defs
import Examples qualified as E
import Level1 qualified as L
import Level3
import Test.Expr
import Test.Prelude hiding (Done)

tests :: NamedTests
tests = nameTests 3 [testMachine]

testMachine :: Test
testMachine = TestList
  [ TestCase $ assertEqual "байткод" "3 5 + 7 *" (showByteCode (compile E.emp357))
  , TestCase $ assertEqual "порядок ветвей" "7 3 5 + *" (showByteCode (compile E.em7p35))
  , TestCase $ assertEqual "точная трасса" [[3], [5, 3], [8], [7, 8], [56]] $
      from (evalStackMachine (compile E.emp357, []))
  , TestCase $ assertEqual "отрицательные константы" [[3], [-5, 3], [-2]] $
      from (evalStackMachine (compile E.emi35, []))
  , TestCase $ assertEqual "пустой код не включает начальное состояние" [] $
      from (evalStackMachine ([], [10]))
  , TestCase $ assertEqual "непустой начальный стек" [[3, 10], [13], [2, 13], [26]] $
      from (evalStackMachine ([PushConstInstr 3, AddInstr, PushConstInstr 2, MultInstr], [10]))
  , TestCase $ assertEqual "недостаточно операндов: инструкция и остаток не исполняются" [] $
      from (evalStackMachine ([AddInstr, PushConstInstr 1], [10]))
  , TestCase $ assertEqual "трасса заканчивается на первой ошибке" [[2]] $
      from (evalStackMachine ([PushConstInstr 2, MultInstr, PushConstInstr 3], []))
  , TestCase $ assertEqual "ошибка в умножении при пустом стеке" [] $
      from (evalStackMachine ([MultInstr], []))
  , propertyToTest "точная компиляция случайных выражений" $ forAll genExpr $ \model ->
      compile (buildExpr model) === codeModel model
  , propertyToTest "байткод сохраняет значение и исходный стек" $
      forAll genExpr $ \model -> forAll (resize 8 arbitrary) $ \(stack :: [Int]) ->
        let trace = from (evalStackMachine (compile (buildExpr model), stack))
        in last trace === evalModel model : stack
  , propertyToTest "компилятор согласован с интерпретатором" $ forAll genExpr $ \model ->
      last (from (evalStackMachine (compile (buildExpr model), []))) === [L.evalExpr (buildExpr model)]
  , propertyToTest "произвольный код: точная трасса и остановка" $
      forAll genCode $ \code -> forAll (resize 8 arbitrary) $ \(stack :: [Int]) ->
        from (evalStackMachine (code, stack)) === referenceTrace code stack ]

genCode :: Gen ByteCode
genCode = resize 20 $ listOf $ oneof
  [PushConstInstr <$> chooseInt (-20, 20), pure AddInstr, pure MultInstr]

referenceTrace :: ByteCode -> Stack -> [Stack]
referenceTrace [] _ = []
referenceTrace (instr : rest) stack = case (instr, stack) of
  (PushConstInstr n, _) -> continue (n : stack)
  (AddInstr, r : l : xs) -> continue ((l + r) : xs)
  (MultInstr, r : l : xs) -> continue ((l * r) : xs)
  _ -> []
  where
    continue next = next : referenceTrace rest next
