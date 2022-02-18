{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--Ethan Cline, Curtis Olels
--CS 381, 001, W2022
--02/17/22

--Base definitions of the data we are manipulating.
type Prog = [Cmd]

data Cmd
  = LDI Int
  | LDB Bool
  | LEQ
  | ADD
  | MULT
  | DUP
  | IFELSE Prog Prog
  | DEC
  | SWAP
  | POP Int
 deriving Show

data Stack = A [Val] | Int | Bool | TypeError | RankError
          deriving(Eq, Show)

type Stack2 = [Val]

data Val = I Int | B Bool
          deriving(Eq, Show)

--data Type = Int | Bool | TypeError | RankError
  --        deriving(Eq, Show)

type Rank = Int
type CmdRank = (Int, Int)

run:: Prog -> Stack2 -> Stack
run p s = semStatTC p (A s)

--run is is a recursive function that semCmds all commands in a program
semCmd :: Prog -> Stack -> Stack
--Adds an integer to a stack
semCmd [LDI n] (A s) = A (I n:s)
--Adds a boolean to a stack
semCmd [LDB n] (A s) = A (B n:s)
--If the top integer is bigger than the second top
--we return True, otherwise return false

semCmd [LEQ] (A (I x:I y:s)) | x <= y = semCmd [LDB True] (A s)
            | otherwise = semCmd [LDB False] (A s)
semCmd [ADD] (A (I x:I y:xs)) = semCmd [LDI (x + y)] (A (xs))
semCmd [MULT] (A (I x:I y:xs)) = semCmd [LDI (x * y)] (A (xs))
--Duplicates the top item on the stack, works for bools and ints
semCmd [DUP] (A (I x:xs)) = semCmd [LDI x, LDI x] (A xs)
semCmd [DUP] (A (B x:xs)) = semCmd [LDB x, LDB x] (A xs)
--If the top element of the stack is true, semCmd the first program,
--else semCmd the second program
semCmd [IFELSE p1 p2] (A (B x:s)) | x  = semCmd p1 (A s) |otherwise = semCmd p2 (A s)

semCmd [DEC] (A (I x:xs)) = semCmd[LDI (x-1)] (A (xs))
semCmd [SWAP] (A (I x:I y:xs)) = semCmd [LDI x, LDI y] (A (xs))
semCmd [SWAP] (A (I x:B y:xs)) = semCmd [LDI x, LDB y] (A (xs))
semCmd [SWAP] (A (B x:I y:xs)) = semCmd [LDB x, LDI y] (A (xs))
semCmd [SWAP] (A (B x:B y:xs)) = semCmd [LDB x, LDB y] (A xs)

semCmd[POP k] (A (x:xs)) | k > 1 = semCmd[POP (k-1)] (A xs) | otherwise = A xs

--This is the definition of the recursion for this function with base case on top
semCmd [] (A s) = A s
semCmd (x:xs) (A []) | typeCorrect x (A []) = semCmd xs (semCmd [x] (A[]))
                 | otherwise = TypeError
semCmd (x:xs) (A(y:ys))| typeCorrect x (A ys) = semCmd xs (semCmd [x] (A(y:ys)))
                       | otherwise = TypeError

{-
--This converts a maybe stack to stack for recursion
maybetostack :: Maybe Stack -> Stack
maybetostack Nothing = []
maybetostack (Just xs) = xs
-}

--helper function for rankC (IFELSE)
--returns true is n1 is greater than n2
rankIfelse :: CmdRank -> CmdRank -> Bool
rankIfelse (n1, m1) (n2, m2) = if n1 > n2 then True else False

--function for mapping rank values to stack functions
rankC :: Cmd -> CmdRank
rankC (ADD) = (2, 1)
rankC (DEC) = (1, 1)
rankC (LDI n) = (0, 1)
rankC (LDB b) = (0, 1)
rankC (LEQ) = (2, 1)
rankC (MULT) = (2, 1)
rankC (DUP) = (1, 2)
rankC (SWAP) = (2, 2)
rankC (POP k) = (k, 0)
--returns rank value with greatest n value
rankC (IFELSE (p1:ps1) (p2:ps2)) = if (rankIfelse (rankC p1) (rankC p2)) then rankC p1 else rankC p2

--returns n value of a CmdRank
rankN :: CmdRank -> Int
rankN (n, m) = n

--returns m value of a CmdRank
rankM :: CmdRank -> Int
rankM (n, m) = m

--returns rank of a program when semCmd with stack rank r
rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (p:ps) r = if (r - rankN (rankC p)) >= 0 then rankP ps (r - rankN (rankC p) + rankM (rankC p)) else Nothing

maybeToRank :: Maybe Rank -> Rank
maybeToRank Nothing = -1
maybeToRank (Just xs) = xs

stackLength :: Stack -> Int
stackLength (A []) = 0
stackLength (A (x:xs)) = 1 + stackLength (A xs)

semStatTC :: Prog -> Stack -> Stack
semStatTC x (A y) = if maybeToRank(rankP x (stackLength (A y))) >= 0 then semCmd x (A y) else RankError

sc :: Val -> Stack
sc (I x) = Int
sc (B x) = Bool

tc :: Cmd -> Stack -> Stack
tc (LDI n) _ = Int
tc (LDB n) _ = Bool
tc LEQ (A(x:y:xs)) | sc x == Int  && sc y == Int = Int
tc ADD (A(x:y:xs)) | sc x == Int  && sc y == Int = Int
tc MULT (A(x:y:xs)) | sc x == Int  && sc y == Int = Int
tc DUP _ = Int
tc (IFELSE _ _) (A(x:xs)) | sc x == Bool = Bool
tc DEC (A(x:xs)) | sc x == Int = Int
tc SWAP _ = Int
tc (POP n) _ = Int
tc _ _  = TypeError

typeCorrect :: Cmd -> Stack -> Bool
typeCorrect e e' = tc e e'/= TypeError

stack1 :: Stack
stack1 = A [I 1, I 3, I 5, I 7, I 9]
--stack2 :: Stack
--stack2 = [Left  True, Right 3]
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]
