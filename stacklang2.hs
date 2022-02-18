--Ethan Cline, Curtis Olels
--CS 381, 001, W2022
--02/17/22

--Base definitions of the data we are manipulting.
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

type Stack = [Val]
data Val = I Int | B Bool
          deriving(Eq, Show)

data Type = Int | Bool | TypeError | RankError
          deriving(Eq, Show)

type Rank = Int
type CmdRank = (Int, Int)

run:: Prog -> Stack -> Maybe Stack
run = semStatTC

--run is is a recursive function that semCmds all commands in a program
semCmd :: Prog -> Stack -> Maybe Stack
--Adds an integer to a stack
semCmd [LDI n] s = Just (I n:s)
--Adds a boolean to a stack
semCmd [LDB n] s = Just (B n:s)
--If the top integer is bigger than the second top
--we return True, otherwise return false
semCmd [LEQ] (x:y:s) | x <= y = semCmd [LDB True] s
            | otherwise = semCmd [LDB False] s
semCmd [ADD] (x:y:xs) = semCmd [LDI (x + y)] xs
semCmd [ADD] (x:y:xs) = Nothing
semCmd [ADD] (x:y:xs) = Nothing
semCmd [MULT] (x:y:xs) = semCmd [LDI (x * y)] xs
semCmd [MULT] (x:y:xs) = Nothing
semCmd [MULT] (x:y:xs) = Nothing
--Duplicates the top item on the stack, works for bools and ints
semCmd [DUP] [] = Nothing
semCmd [DUP] (x:xs) = semCmd [LDI x, LDI x] xs
semCmd [DUP] (x:xs) = semCmd [LDB x, LDB x] xs
--If the top element of the stack is true, semCmd the first program,
--else semCmd the second program
semCmd [IFELSE p1 p2] (x:s) | x  = semCmd p1 s |otherwise = semCmd p2 s
--If ADD is called with an emptystack we return nothing
semCmd [ADD] [] = Nothing
--If MULT is called with an emptystack we return nothing
semCmd [MULT] [] = Nothing

semCmd [DEC] (x:xs) = semCmd[LDI (x-1)] xs
semCmd [SWAP] (x:y:xs) = semCmd [LDI x, LDI y] xs
semCmd [SWAP] (x:y:xs) = semCmd [LDI x, LDB y] xs
semCmd [SWAP] (x:y:xs) = semCmd [LDB x, LDI y] xs
semCmd [SWAP] (x:y:xs) = semCmd [LDB x, LDB y] xs

semCmd[POP k] (x:xs) | k > 1 = semCmd[POP (k-1)] xs | otherwise = Just xs

--This is the definition of the recursion for this function with base case on top
semCmd [] s = Just s
semCmd (x:xs) [] = semCmd xs (maybetostack (semCmd [x] []))
semCmd (x:xs) (y:ys) = semCmd xs (maybetostack(semCmd [x] (y:ys)))

--This converts a maybe stack to stack for recursion
maybetostack :: Maybe Stack -> Stack
maybetostack Nothing = []
maybetostack (Just xs) = xs

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


maybetorank :: Maybe Rank -> Rank
maybetorank Nothing = -1
maybetorank (Just xs) = xs

semStatTC :: Prog -> Stack -> Maybe Stack
semStatTC x y | maybetorank(rankP x (length y)) >= 0 = semCmd x y
             | otherwise     = Nothing


--Testing tools
{-
stack1 :: Stack
stack1 = [Right  1, Right 3, Right 5, Right 7, Right 9]
stack2 :: Stack
stack2 = [Left  True, Right 3]
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]
-}
