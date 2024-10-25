data TurnstileState = Locked | Unlocked deriving (Show)
data TurnstileOutput = Thank | Tut | Open deriving (Show)

coin :: TurnstileState -> (TurnstileOutput,TurnstileState)
coin _ = (Thank,Unlocked)

newtype State s a = State { runState :: s -> (a,s) }
state :: (s -> (a,s)) -> State s a 
state = State

(>>=) :: State s a -> (a -> State s b) -> State s b
p >>= k = state $ \ s0 ->
  let (x,t) = runState p s0
  in runState (k x) t
