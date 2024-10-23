{-
In this section of the code we will understand what is state through the 
example of a Turnstile
-}

data TurnstileState = Locked | Unlocked deriving (Eq, Show)
data TurnstileOutput = Tut | Thank | Open deriving (Eq, Show)

coinSimple :: TurnstileState -> TurnstileOutput
coinSimple _ = Thank

pushSimple :: TurnstileState -> TurnstileOutput
pushSimple Locked = Tut
pushSimple Unlocked = Open


-- To track state we need to include the state
coin, push :: TurnstileState -> (TurnstileState,TurnstileOutput)

coin _ = (Unlocked, Thank)

push Locked = (Locked,Tut)
push Unlocked = (Locked,Open)

monday :: TurnstileState -> (TurnstileState,[TurnstileOutput])
monday s0 = 
    let (s1,m1) = coin s0
        (s2,m2) = push s1
        (s3,m3) = push s2
        (s4,m4) = push s3
        (s5,m5) = coin s4
        (s6,m6) = push s5
    in (s6,[m1,m2,m3,m4,m5,m6])

regularPerson,hastyPerson,distractedPerson :: TurnstileState -> (TurnstileState,[TurnstileOutput])

regularPerson _ = (Locked,[Thank, Open])

hastyPerson Unlocked = (Locked,[Open])
hastyPerson Locked = (Locked, [Tut,Thank,Open])

distractedPerson _ = (Unlocked, [Thank])

tuesday :: TurnstileState -> (TurnstileState,[TurnstileOutput])
tuesday s0 = 
    let (s1,m1) = regularPerson s0
        (s2,m2) = hastyPerson s1
        (s3,m3) = distractedPerson s2
        (s4,m4) = hastyPerson s3
    in (s4,m1++m2++m3++m4)

luckyPair :: Bool -> TurnstileState -> (Bool,TurnstileState)
luckyPair True _ = (False, Locked)
luckyPair False _ = (True, Locked)


