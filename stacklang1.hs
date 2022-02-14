--Ethan Cline
--CS 381, 001, W2022
--02/08/22

--Base definitions of the data we are manipulting.
type Prog = [Cmd]

data Cmd
  = LD Int
  | ADD
  | MULT
  | DUP
 deriving Show

type Stack = [Int]


--run is is a recursive function that runs all commands in a program
run :: Prog -> Stack -> Maybe Stack
--Adds an integer to a stack
run [LD n] s = Just(n:s)
--Adds the top 2 intergers, I couldn't get the error protection to work. :(
run [ADD] (x:y:xs) = run [LD (x + y)] xs
--Multplies the top 2 intergers, I couldn't get the error protection to work. :(
run [MULT] (x:xs) = run [LD (x * head xs)] (drop 1 xs)
--If DUP is called with an emptystack we return nothing
run [DUP] [] = Nothing
--Duplicates the top item on the stack
run [DUP] xs = run [LD (head xs)] xs
--If ADD is called with an emptystack we return nothing
run [ADD] [] = Nothing
--If MULT is called with an emptystack we return nothing
run [MULT] [] = Nothing

--This is the definition of the recursion for this function with base case on top
run [] s = Just s
run (x:xs) [] = run xs (maybetostack (run [x] []))
run (x:xs) (y:ys) = run xs (maybetostack(run [x] (y:ys)))

--This converts a maybe stack to stack for recursion
maybetostack :: Maybe Stack -> Stack
maybetostack Nothing = []
maybetostack (Just xs) = xs

--Testing tools
stack1 :: Stack
stack1 = [1, 2, 3, 4, 5]
test1 = [LD 3,DUP,ADD,DUP,MULT]
test2 = [LD 3,ADD]
test3 = []
test4 = [ADD, ADD, ADD, ADD]
