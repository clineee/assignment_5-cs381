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

type Stack = [Either Bool Int]
data Val = I Int | B Bool

type Rank = Int
type CmdRank = (Int, Int)

--run is is a recursive function that runs all commands in a program
run :: Prog -> Stack -> Maybe Stack
--Adds an integer to a stack
run [LDI n] s = Just(Right n:s)
--Adds a boolean to a stack
run [LDB n] s = Just(Left n:s)
--If the top integer is bigger than the second top
--we return True, otherwise return false
run [LEQ] (x:y:s) | x <= y = run [LDB True] s
            | otherwise = run [LDB False] s
--Adds the top 2 intergers, I couldn't get the error protection to work.
--Protects against Bools though
run [ADD] (Right x:Right y:xs) = run [LDI (x + y)] xs
run [ADD] (Left x: y:xs) = Nothing
run [ADD] (x: Left y:xs) = Nothing
--multiplies the top 2 intergers, I couldn't get the error protection to work.
--Protects against Bools though
run [MULT] (Right x:Right y:xs) = run [LDI (x * y)] xs
run [MULT] (Left x: y:xs) = Nothing
run [MULT] (x: Left y:xs) = Nothing
--Duplicates the top item on the stack, works for bools and ints
run [DUP] [] = Nothing
run [DUP] (Right x:xs) = run [LDI x, LDI x] xs
run [DUP] (Left x:xs) = run [LDB x, LDB x] xs
--If the top element of the stack is true, run the first program,
--else run the second program
run [IFELSE p1 p2] (Left x:s) | x  = run p1 s |otherwise = run p2 s
--If ADD is called with an emptystack we return nothing
run [ADD] [] = Nothing
--If MULT is called with an emptystack we return nothing
run [MULT] [] = Nothing

run [DEC] (Right x:xs) = run[LDI (x-1)] xs
run [SWAP] (Right x: Right y:xs) = run [LDI x, LDI y] xs
run [SWAP] (Right x: Left y:xs) = run [LDI x, LDB y] xs
run [SWAP] (Left x: Right y:xs) = run [LDB x, LDI y] xs
run [SWAP] (Left x: Left y:xs) = run [LDB x, LDB y] xs

run[POP k] (x:xs) | k > 1 = run[POP (k-1)] xs | otherwise = Just xs

--This is the definition of the recursion for this function with base case on top
run [] s = Just s
run (x:xs) [] = run xs (maybetostack (run [x] []))
run (x:xs) (y:ys) = run xs (maybetostack(run [x] (y:ys)))

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

--returns rank of a program when run with stack rank r
rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (p:ps) r = if (r - rankN (rankC p)) >= 0 then rankP ps (r - rankN (rankC p) + rankM (rankC p)) else Nothing

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
