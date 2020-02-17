module Lib
    ( run
    , Band(..)
    , State(..)
    , parse
    , Op(..)
    , step
    , moveRight
    , moveLeft
    , nextInstruction
    , increase
    , decrease
    , splitOnMatchingBracket
    )
where

data Op = MoveRight
        | MoveLeft
        | Increase
        | Decrease
        | Output
        | Input
        | Loop [Op]
    deriving (Show, Eq)

data Band a = Band
    { prefix :: ![a]
    , suffix :: ![a]
    }
    deriving (Show, Eq)

data State = State
    { ops :: !(Band Op)
    , band :: !(Band Int)
    , input :: String
    , output :: !String
    }
    deriving(Show, Eq)

run :: String -> String -> String
run code inp =
    let init = State (Band [] (parse code)) (Band [] [0]) inp ""
    in  output $ execute init

execute :: State -> State
execute = until (null . suffix . ops) step

splitOnMatchingBracket :: String -> (String, String)
splitOnMatchingBracket l = go ([], l) 0
  where
    go (prefix, x : xs) level
        | x == '['               = go (prefix ++ "[", xs) $ level + 1
        | x == ']' && level == 0 = (prefix, xs)
        | x == ']' && level /= 0 = go (prefix ++ "]", xs) $ level - 1
        | otherwise              = go (prefix ++ [x], xs) level

parse :: String -> [Op]
parse []       = []
parse (c : cs) = case token of
    Just t  -> t : parse rest
    Nothing -> parse rest
  where
    (token, rest) = case c of
        '>' -> (Just MoveRight, cs)
        '<' -> (Just MoveLeft, cs)
        '+' -> (Just Increase, cs)
        '-' -> (Just Decrease, cs)
        '.' -> (Just Output, cs)
        ',' -> (Just Input, cs)
        '[' ->
            let (inLoop, outLoop) = splitOnMatchingBracket cs
            in  (Just $ Loop $ parse inLoop, outLoop)
        _ -> (Nothing, cs)

step :: State -> State
step state@(State code@(Band _ (token : _)) band'@(Band prefix (cell : suffix)) inp out)
    = case token of
        MoveRight ->
            state { ops = nextInstruction code, band = moveRight band' }
        MoveLeft -> state { ops = nextInstruction code, band = moveLeft band' }
        Increase -> state { ops = nextInstruction code, band = increase band' }
        Decrease -> state { ops = nextInstruction code, band = decrease band' }
        Output ->
            state { ops = nextInstruction code, output = out ++ [toEnum cell] }
        Input ->
            let (i : is) = inp
            in  State (nextInstruction code)
                      (Band prefix $ fromEnum i : suffix)
                      is
                      out
        Loop ops' ->
            let s = loop state { ops = Band [] ops' }
            in  s { ops = nextInstruction code }

loop :: State -> State
loop state = until ((== 0) . head . suffix . band)
                   (\s -> execute s { ops = ops state })
                   state

nextInstruction :: Band a -> Band a
nextInstruction (Band prev (current : following)) =
    Band (prev ++ [current]) following

moveRight :: Enum a => Band a -> Band a
moveRight (Band p [c         ]) = Band (p ++ [c]) [toEnum 0]
moveRight (Band p (c : x : xs)) = Band (p ++ [c]) $ x : xs

moveLeft :: Enum a => Band a -> Band a
moveLeft (Band [] (c : s)) = Band [] [toEnum 0, c]
moveLeft (Band p  (c : s)) = Band (init p) (last p : c : s)

increase :: Enum a => Band a -> Band a
increase (Band p (c : s)) = Band p (succ c : s)

decrease :: Enum a => Band a -> Band a
decrease (Band p (c : s)) = Band p (pred c : s)
