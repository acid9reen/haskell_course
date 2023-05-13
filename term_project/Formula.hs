module Formula where

-- Этот модуль содержит 16 заданий. Внимательно читайте комментарии
-- перед определениями функций. Избегайте строк длиннее 80 символов.
-- Написание тестов приветствуется.

import Data.List
import Data.Semigroup
import BooleanSyntax
--  (Op, AssocType(FA, LA, RA, NA), Domain, arity, prec, noOp, opText,
--  assoc, evalOp)

-- Ограничения на импорт описаны в лекции 2. Данный модуль
-- предназначен для работы с функциями любой сигнатуры, то есть с
-- любым набором связок, а не только булевыми формулами. Это
-- достигается тем, что из модуля BooleanSyntax импортируется сам тип
-- Op, но не его конструкторы T, F, Neg, And и т.д. Чтобы
-- импортировались также конструкторы Op, нужно добавить их в скобках
-- после типа, как в случае AssocType, или вообще убрать ограничение
-- на импорт.

-- Пока ограничения временно закомментированны, но поскольку этот
-- модуль предназначен для формул любой сигнатуры, его функции должны
-- работать с ограничениями на импорт из BooleanSyntax. Также очень
-- важно то, чтобы все функции, кроме fullParen и displayFormula ниже,
-- работали со связками произвольной арности, а не только 0, 1 и 2.

-- C означает "compound", т.е. "составная".

data Formula a = V a | C Op [Formula a]

-- Тип a в Formula a есть тип имен переменных. Это может быть,
-- например, Char, String или Int.

-------------------------------------------------
-- 1. Примеры формул
-------------------------------------------------

-- Примеры формул ниже принимаются интерпретатором, если не
-- ограничивать импорт из модуля BooleanSyntax.

-- form1 = x \/ y -> ~z

form1 :: Formula Char
form1 = C If [C Or [V 'x', V 'y'], C Neg [V 'z']]

-- form2 = x \/ ~(y z)

form2 :: Formula Char
form2 = C Or [V 'x', C Neg [C And [V 'y', V 'z']]]

-- form3 = x y + ~z <-> x \/ z

form3 :: Formula Char
form3 = C Iff [C Xor [C And [V 'x', V 'y'], C Neg [V 'z']],
               C Or [V 'x', V 'z']]

form4 :: Formula Char
form4 = C Iff [V 'x']

-- form4 = <-> x (incorrect arity)

-- Задание 1. Напишите функцию correctArity, которая проверяет, что
-- арность каждого оператора, объявленная в модуле BooleanSyntax,
-- совпадает с действительным количеством его аргументов в формуле.

-- Важно: функцию correctArity не следует включать в другие функции,
-- описанные ниже, тем более в рекурсивные. Эти другие функции
-- работают в предположении, что формулы составлена правильно, то есть
-- correctArity вернула True.

correctArity :: Formula a -> Bool
correctArity (V _) = True
correctArity (C op args) = length args == arity op && all correctArity args

-------------------------------------------------
-- 2. Текстовое представление формул
-------------------------------------------------

-- Для преобразования формул в строки можно было использовать класс
-- Show. Однако в Haskell действует соглашение, что функция show
-- должна возвращать строку, содержащую правильное выражение Haskell,
-- которое можно ввести в командной строке интерпретатора или дать на
-- вход функции read. На самом деле, для многих типов функции show и
-- read являются взаимно обратными, то есть read . show является
-- тождественной функцией. Так, show 'c' возвращает строку "'c'" из
-- трех символов. Напротив, в этом проекте задание состоит в написании
-- функций, возвращающих такое текстовое представление формул, которое
-- подходит для чтения человеком (pretty printing). Поэтому мы
-- определим новый класс типов Display и объявим тип Char его
-- экземпляром другим способом, чем тот, который использовался при
-- объявлении Char экземпляром класса Show.

type DisplayS = ShowS

class Display a where
  display :: a -> String
  displays :: a -> DisplayS

-- Для объявления экземпляра достаточно переопределить одну из этих функций.

  display x = displays x ""
  displays x s = display x ++ s

instance Display Char where
  displays = (:)

instance Display Int where
  displays = shows

-- Функцию arityError следует вызывать в функциях fullParen и
-- displayFormula, определенных ниже, если в печатаемой формуле
-- встречаются операции с арностью, отличной от 0, 1 или 2. Эти
-- функции являются единственными функциями в данном модуле, которые
-- работаю со связками именно такой арности. В других функциях
-- использовать arityError не следует, так как они работают с
-- формулами общего вида, которые могут использовать связки
-- произвольной арности.

arityError = error "Arity other than 0, 1 or 2"

-- Задание 2. Напишите функцию fullParen, которая возвращает текстовое
-- представление формулы, где каждая состовная подформула с
-- положительным числом аргументов окружена скобками. Переменные и
-- константы (то есть нульарные функции) окружать скобками не нужно.

-- Для заключения формулы в скобки здесь и в следующем задании следует
-- использовать функцию showParen.

fullParen :: Display a => Formula a -> DisplayS
fullParen (V v) = displays v
fullParen (C op []) = opText op
fullParen (C op [arg]) = showParen True $ opText op . fullParen arg
fullParen (C op [arg1, arg2]) = showParen True
  $ fullParen arg1 . opText op . fullParen arg2
fullParen _ = arityError

-- Вариант, учитывающий приоритет и ассоциативность операций

-- Скобки вокруг констант (операций арности 0) и переменных не
-- ставятся. Операции арности 1 являются префиксными: например,
-- C Neg [f] отображается как ~f. Скобки вокруг таких формул ставятся
-- в том случае, если приоритет непосредственно окружающей операции
-- строго больше приоритета данной операции арности 1.

-- Инфиксные операции

-- Пусть данная формула (второй аргумент функции ниже) является левым
-- аргументом связки opExt, а главная связка формулы есть opInt.
-- Скобки вокруг формулы ставятся тогда и только тогда, когда
-- 1) приоритет opExt строго больше приоритета opInt, или
-- 2) приоритет opExt равен приоритету opInt и
-- 2а) opExt <> opInt, или
-- 2б) opExt = opInt имеет ассоциативность RA или NA.

-- Если данная формула является правым аргументом opExt, то в пункте 2б)
-- нужно заменить RA на LA.

-- Замечание: никогда не следует писать

-- f x
--   | p x = ...
--   | not (p x) = ...

-- Поскольку ограничения проверяются в указанном порядке, Haskell
-- рассматривает третью строчку определения только тогда, когда p x
-- ложно. Поэтому следует писать (напоминание: otherwise == True)

-- f x
--   | p x = ...
--   | otherwise = ...

data ArgPos = LeftArg | RightArg deriving (Show, Eq)

-- Задание 3. Напишите функцию displayFormula, которая возвращает
-- текстовое представление формулы, где вставляются только необходимые
-- скобки согласно описанию выше.
-- Первый аргумент: оператор, находящийся непосредственно снаружи формулы
--   (внешний оператор).
-- Второй аргумент: является ли формула левым (LeftArg) или правым (RightArg)
--   аргументом внешнего оператора.
-- Третий аргумент: формула, которую нужно напечатать.

-- Вроде работает, но некрасиво))0

displayFormula :: Display a => Op -> ArgPos -> Formula a -> DisplayS
displayFormula _ _ (V v) = displays v
displayFormula _ _ (C op []) = opText op
displayFormula extOp _ (C intOp [arg]) = showParen (prec extOp > prec intOp) -- Вроде была функция что можно улучшить в что-то типа (func > prec extOp intOp)
  $ opText intOp . displayFormula intOp LeftArg arg
displayFormula extOp formPos (C intOp [arg1, arg2]) = showParen needBraces
  $ displayFormula intOp LeftArg arg1
  . opText intOp
  . displayFormula intOp RightArg arg2 where
    needBraces =
      prec extOp > prec intOp
      || (prec extOp == prec intOp
         && (extOp /= intOp || assoc intOp == NA || assoc intOp == neededAssoc)) where
           neededAssoc = if formPos == LeftArg then RA else LA
displayFormula _ _ _ = arityError


-- После написания fullParen или displayFormula раскоментируйте
-- соответствующий вариант объявления членства типа Formula в классе
-- Display.

instance Display a => Display (Formula a) where
  -- displays f = fullParen f
 displays = displayFormula noOp LeftArg

-- Примеры формул form1, form2 и form3 выше должны печататься так, как
-- они записаны в комментариях перед определением.

-------------------------------------------------
-- 3. Значение формулы типа Formula Int
-------------------------------------------------

-- Важно: следующие функции должны принимать формулы со связками
-- любой арности: 0, 1, 2, 3 и т.д.

-- Значения переменных берутся из окружения (environment). Окружение
-- можно рассматривать как набор значений аргументов в одной строке
-- табличного определения функции (таблицы истинности в случае
-- двузначной логики).

-- Если f :: Formula Int, то переменные кодируются целыми числами. В
-- этом случае значения переменной с номером i (i >= 0) есть i-й
-- элемент окружения.

type Environment = [Domain]

-- Задание 4. Напишите функцию lookupVar, возвращающую значение переменной
-- с данным номером в окружении.

lookupVar :: Environment -> Int -> Domain
lookupVar env index = env !! index

-- Задание 5. Напишите функцию eval, возвращающую значение формулы
-- типа Formula Int в окружении. Значения операций определяются функцией
-- evalOp, определенной в модуле BooleanSyntax.
testEnv :: [Domain] = [True, False]
intForm1 :: Formula Int = C Or [V 0, V 1] -- -> True
intForm2 :: Formula Int = C Or [V 0, V 0] -- -> False

eval :: Environment -> Formula Int -> Domain
eval env (V v) = lookupVar env v
eval env (C op args) = evalOp op $ map (eval env) args

-------------------------------------------------
-- 4. Компиляция Formula a в Formula Int
-------------------------------------------------

-- Чтобы получить значения формулы на всех возможных окружениях,
-- поступим следующим образом.
-- 1. Найдем список всех переменных, встречающихся в формуле. Каждая
-- переменная должна входить в список по одному разу. Обозначим длину
-- этого списка через n.
-- 2. Преобразуем формулу в Formula Int, заменив каждую переменную на
-- ее индекс в списке из п. 1.
-- 3. Составим список всех окружений длины n.
-- 4. Вычислим значение формулы в каждом окружении с помощью функции
-- eval.

-- Задание 6. Напишите функцию collectVars1, которая возвращает список
-- переменных, входящих в формулу. Каждая переменная входит в список
-- не более одного раза. Можно использовать функцию nub из Data.List.

collectVars1 :: Eq a => Formula a -> [a]
collectVars1 (V v) = [v]
collectVars1 (C _ args) = nub $ concatMap collectVars1 args

-- Задание 7. Напишите функцию varsToInt, которая принимает список
-- переменных и формулу и возвращает формулу типа Formula Int, где
-- каждая переменная заменена на ее индекс в списке. Если переменной
-- из формулы нет в списке, нужно вызывать функцию error с сообщением
-- "varsToInt: Variable occurs in the formula but not in the list".
-- Можно использовать функции из Data.List для поиска в списке.

varsToInt :: Eq a => [a] -> Formula a -> Formula Int
varsToInt vars (V v) = case elemIndex v vars of
  (Just index) -> V index
  _ -> error "varsToInt: Variable occurs in the formula but not in the list"
varsToInt vars (C op args) = C op $ map (varsToInt vars) args

-- Задание 8. Напишите функцию compileFormula с аргументом f :: Formula a.
-- Пусть vars есть список всех переменных f без повторений. Тогда
-- compileFormula возвращает пару (length vars, varsToInt vars f).

compileFormula :: Eq a => Formula a -> (Int, Formula Int)
compileFormula form = (length vars, varsToInt vars form) where
  vars = collectVars1 form

-------------------------------------------------
-- 5. Значения формулы на всевозможных окружениях
-------------------------------------------------

-- Задание 9. В предположении, что тип Domain является членом классов
-- Enum и Bounded, определите список domain всех элементов типа
-- Domain. Следует использовать синтаксис [x..y] для определения
-- последовательностей. Может оказаться полезным описание классов
-- Enum и Bounded в Prelude.

domain :: [Domain]
domain = [minBound..maxBound]

-- Задание 10. Напишите функцию allEnvs, которая принимает число n
-- и возвращает список всех окружений длины n в лексикографическом
-- порядке. Порядок на компонентах окружения определяется списком
-- domain.

allEnvs :: Int -> [Environment]
allEnvs 0 = [[]]
allEnvs n = concatMap (addSymb $ allEnvs (n - 1)) domain where
  addSymb tails_ symb = [symb:tail_ | tail_ <- tails_] 

-- Задание 11. Напишите функцию formulaValues, которая возвращает
-- значения формулы на всех наборах аргументов. В случае двузначной
-- логики это последний столбец таблицы истинности. Формула должна
-- компилироваться один раз.

form5 :: Formula Char
form5 = C Or [V 'x', V 'y']

form6 :: Formula Char
form6 = C And [V 'x', V 'y']

formulaValues :: Eq a => Formula a -> [Domain]
formulaValues form = map (`eval` compiledForm) (allEnvs n) where
  (n, compiledForm) = compileFormula form

-- Задание 12. Напишите функцию isConstant c f, которая определяет, является
-- ли формула f константой, принимающей значение c на всех окружениях

constForm1 :: Formula Char
constForm1 = C Or [V 'x', C Neg [V 'x']]

constForm2 :: Formula Char
constForm2 = C And [V 'x', C Neg [V 'x']]

isConstant :: Eq a => Domain -> Formula a -> Bool
isConstant c form = all (== c) $ formulaValues form 

-------------------------------------------------
-- 6. Варианты collectVars
-------------------------------------------------

-- Задание 13. Напишите функцию collectVars2, аналогичную
-- collectVars1, но использующую списочную монаду. Список уже
-- определен в Prelude как член класса Monad, поэтому можно
-- использовать функции return и >>=.

collectVars2 :: Eq a => Formula a -> [a]
collectVars2 = undefined

-- Задание 14. Разностные списки описаны в лекции 8. Напишите функцию
-- collectVars3, аналогичную collectVars1, но использующую разностные
-- списки.

collectVars3 :: Eq a => Formula a -> [a]
collectVars3 = undefined

-- Задание 15. Сделайте конструктор типов Formula членом класса Foldable,
-- определив функцию foldMap.

instance Foldable Formula where
  foldMap = undefined

-- Задание 16. Напишите функцию collectVars4, аналогичную
-- collectVars1, которая явно не использует рекурсию, а вместо этого
-- использует foldMap с моноидом эндоморфизмов на типе списков
-- переменных.

-- Важно: В Haskell моноид эндоморфизмов отличается от моноида
-- функций, то есть значений типа (a -> a). В этих моноидах
-- операция (<>) определена по-разному.

collectVars4 :: Eq a => Formula a -> [a]
collectVars4 = undefined
