module Lib
    ( eval
    , Band(..)
    , State(..)
    , evalToken
    , moveRight
    , moveLeft
    , increase
    , decrease
    )
where

eval :: String -> IO ()
eval (c : code) =
    let init = (Band [] c code, Band [] 0 [])
    in  sequence_ $ iterate (>>= evalToken) $ pure init

data Band a = Band [a] a [a]
    deriving (Show, Eq)

type State = (Band Char, Band Int)

evalToken :: State -> IO State
evalToken (code@(Band _ token _), band@(Band prefix cell suffix)) =
    case token of
        '>' -> pure (nextInstruction code, moveRight band)
        '<' -> pure (nextInstruction code, moveLeft band)
        '+' -> pure (nextInstruction code, increase band)
        '-' -> pure (nextInstruction code, decrease band)
        '.' -> do
            putChar $ toEnum cell
            --print code
            pure (nextInstruction code, band)
        ',' -> do
            c <- getChar
            pure (nextInstruction code, Band prefix (fromEnum c) suffix)
        '[' -> undefined
        ']' -> undefined

nextInstruction :: Band Char -> Band Char
nextInstruction (Band prev token (next : following)) =
    Band (prev ++ [token]) next following

moveRight :: Enum a => Band a -> Band a
moveRight (Band p c []      ) = Band (p ++ [c]) (toEnum 0) []
moveRight (Band p c (x : xs)) = Band (p ++ [c]) x xs

moveLeft :: Enum a => Band a -> Band a
moveLeft (Band [] c s) = Band [] (toEnum 0) [c]
moveLeft (Band p  c s) = Band (init p) (last p) (c : s)

increase :: Enum a => Band a -> Band a
increase (Band p c s) = Band p (succ c) s

decrease :: Enum a => Band a -> Band a
decrease (Band p c s) = Band p (pred c) s
