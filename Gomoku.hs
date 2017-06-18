module Main where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Control.Monad

import System.CPUTime
import System.IO
import Text.Printf

type Field = (Position, Color)
type BoardMap = Map.Map Position Color
type GameTree = Tree.Tree Game
type Point = (Int, Int)
type Region = (Point, Point)


-- Color

data Color = B | C deriving (Read, Eq, Ord)

instance Show Color where
    show B = "\9920"
    show C = "\9922"

-- Position

newtype Position = Pos {getCoords :: (Int, Int)} deriving (Eq, Ord)

instance Show Position where
    show (Pos pair) = show pair

instance Read Position where
    readsPrec _ = readPosition

readPosition :: ReadS Position
readPosition = readParen True (\s -> [(Pos (read x :: Int, read y :: Int), t) |
                                            (x, t'') <- lex s,
                                            (",", t') <- lex t'',
                                            (y, t) <- lex t'])


-- Board

newtype Board = Board {getMap :: BoardMap}

instance Show Board where
    show = writeBoard

instance Read Board where
   readsPrec _ s = [(readBoard s, "")]

displayField :: Maybe Color -> String
displayField (Just color) = show color
displayField Nothing = " "

showNumber :: Int -> String
showNumber number
  | number < 10 = " " ++ show number
  | otherwise = show number

displayRow :: BoardMap -> Int -> String
displayRow boardMap row =
    showNumber row ++ " " ++ concatMap ((\x -> "| " ++ x ++ " ") . displayField) [Map.lookup (Pos (row, y)) boardMap | y <- [1..boardSize]] ++ "|"

wrap :: String -> [String] -> [String]
wrap border [] = [border]
wrap border (x:xs) = border : x : wrap border xs

borderPattern :: String
borderPattern = "   " ++ concat (replicate boardSize "+---" ++ ["+"])

firstRow :: String
firstRow = "    " ++ concatMap (\x -> " " ++ showNumber x ++ " ") [1..boardSize]

writeBoard :: Board -> String
writeBoard (Board boardMap) = unlines $ firstRow : wrap borderPattern (map (displayRow boardMap) [1..boardSize])

readBoard :: String -> Board
readBoard s = Board (Map.fromList (read s :: [Field]))


-- Game

data Move = Move {getPos :: Position, getCol :: Color}

instance Show Move where
  show (Move pos col) = show col ++ "  on " ++ show pos

-- current board with evaluation and saved recent move
data Game = Game {getBoard :: Board, getMove :: Move, getVal :: Int}

instance Show Game where
    show (Game board _ _) = show board

insert :: Board -> Color -> Position -> Board
insert (Board boardMap) stone pos@(Pos (_,_)) = Board (Map.insert pos stone boardMap)

free :: Position -> BoardMap -> Bool
free pos@(Pos (_,_)) boardMap = Maybe.isNothing (Map.lookup pos boardMap)

availablePositions ::  Board -> [Position]
availablePositions (Board boardMap) = [Pos (x, y) | x <- [1..boardSize], y <- [1..boardSize], free (Pos (x, y)) boardMap]

otherColor :: Color -> Color
otherColor C = B
otherColor B = C

nextMoves :: Board -> Color -> [Move]
nextMoves board color = [Move pos color | pos <- availablePositions board]

nextBoard :: Board -> Move -> Board
nextBoard board move = insert board (getCol move) (getPos move)

moves :: Game -> [Game]
moves (Game board lastMove _) = [Game (nextBoard board move) move 0 | move <- nextMoves board switchedColor]
  where
    switchedColor = (otherColor . getCol) lastMove


-- Functions for gathering sequences of adjacent fields on board

rowSequence :: Int -> Point -> [Point]
rowSequence len (row,col) = zip (replicate len row) [y | y <- [col..col+len-1]]

colSequence :: Int -> Point -> [Point]
colSequence len (row,col) = zip [x | x <- [row..row+len-1]] (replicate len col)

rDiagSequence :: Int -> Point -> [Point]
rDiagSequence len (row,col) = zip [x | x <- [row..row+len-1]] [y | y <- [col..col+len-1]]

lDiagSequence :: Int -> Point -> [Point]
lDiagSequence len (row,col) = zip [x | x <- [row..row+len-1]] [y | y <- [col,col-1..col-len+1]]

seqToPos :: [Point] -> [Position]
seqToPos = map Pos

contents :: Board -> [Position] -> [Maybe Color]
contents board positions = map (\pos -> Map.lookup pos (getMap board)) positions

rowStartPoints :: Int -> Region -> [Point]
rowStartPoints seqLen region
  | colRadius region < seqLen = []
  | otherwise = [(x,y) | x <- [fromRow..toRow], y <- [fromCol..toCol-seqLen+1]]
  where
    ((fromRow, fromCol), (toRow, toCol)) = region

colStartPoints :: Int -> Region -> [Point]
colStartPoints seqLen region
  | rowRadius region < seqLen = []
  | otherwise = [(x,y) | y <- [fromCol..toCol], x <- [fromRow..toRow-seqLen+1]]
  where
    ((fromRow, fromCol), (toRow, toCol)) = region

rDiagStartPoints :: Int -> Region -> [Point]
rDiagStartPoints seqLen region
  | rowRadius region < seqLen || colRadius region < seqLen = []
  | otherwise = [(x,y) | x <- [fromRow..toRow-seqLen+1], y <- [fromCol..toCol-seqLen+1]]
  where
    ((fromRow, fromCol), (toRow, toCol)) = region

lDiagStartPoints :: Int -> Region -> [Point]
lDiagStartPoints seqLen region
  | rowRadius region < seqLen || colRadius region < seqLen = []
  | otherwise = [(x,y) | x <- [fromRow..toRow-seqLen+1], y <- [toCol,toCol-1..fromCol+seqLen-1]]
  where
    ((fromRow, fromCol), (toRow, toCol)) = region

allSequences :: Int -> Board -> [[Maybe Color]]
allSequences len board = concatMap (\fun -> fun len allBoardRegion board) [rows, cols, rDiag, lDiag]

allBoardRegion :: Region
allBoardRegion = ((1,1),(19,19))

rows :: Int -> Region -> Board -> [[Maybe Color]]
rows seqLen region board = map (contents board . seqToPos . rowSequence seqLen) (rowStartPoints seqLen region)

cols :: Int -> Region -> Board -> [[Maybe Color]]
cols seqLen region board = map (contents board . seqToPos . colSequence seqLen) (colStartPoints seqLen region)

rDiag :: Int -> Region -> Board -> [[Maybe Color]]
rDiag seqLen region board = map (contents board . seqToPos . rDiagSequence seqLen) (rDiagStartPoints seqLen region)

lDiag :: Int -> Region -> Board -> [[Maybe Color]]
lDiag seqLen region board = map (contents board . seqToPos . lDiagSequence seqLen) (lDiagStartPoints seqLen region)

-- maximum length patterns on all rows, cols and diagonals for validating end game (finding Seq 5)
boardWidePatterns :: Color -> Board -> [[Pattern Int]]
boardWidePatterns color board = map (\xs -> foldPattern color xs []) $
  rows 19 allBoardRegion board ++
  cols 19 allBoardRegion board ++
  rDiag 19 allBoardRegion board ++
  lDiag 19 allBoardRegion board ++
  subdiags
  where
    subdiags = map (contents board . seqToPos) diagPointSequences
    diagPointSequences = map (\len -> rDiagSequence len (19-len+1,1)) [5..18] ++
                         map (\len -> rDiagSequence len (1,19-len+1)) [18,17..5] ++
                         map (\len -> lDiagSequence len (19-len+1,19)) [18,17..5] ++
                         map (\len -> lDiagSequence len (1,len)) [5..18]



-- Evaluation function

-- white is a maximizer, black is a minimizer
staticEvaluation :: Board -> Int
staticEvaluation board = evaluateBoardForColor board B - evaluateBoardForColor board C

evaluateBoardForColor :: Board -> Color -> Int
evaluateBoardForColor board color
  | [Seq 5] `elem` fivesPatterns = 100000
  | [Gap, Seq 4, Gap] `elem` sixesPatterns = 50000
  | [Gap, Seq 3, Gap] `elem` fivesPatterns = 25000
  | otherwise = resultValue
  where
    resultValue = Maybe.fromJust result
    result = foldr (liftM2 (+)) (Just 0) filteredRates
    filteredRates = filter (/= Nothing) rates
    fivesPatterns = map (\xs -> foldPattern color xs []) fives
    sixesPatterns = map (\xs -> foldPattern color xs []) sixes
    rates = map (rateSequence color) fives
    fives = allSequences 5 board
    sixes = allSequences 6 board

data Pattern a = Gap | Seq a deriving (Show, Eq)

instance Functor Pattern where
  fmap _ Gap = Gap
  fmap f (Seq a) = Seq (f a)

foldPattern :: Color -> [Maybe Color] -> [Pattern Int] -> [Pattern Int]
foldPattern _ [] acc = reverse acc
foldPattern color (x:xs) acc = case length acc of
  0 | Maybe.isNothing x -> foldPattern color xs [Gap]
    | Just color == x -> foldPattern color xs [Seq 1]
    | otherwise -> [Gap]
  _ | Maybe.isNothing x -> case head acc of
      Gap -> foldPattern color xs acc
      Seq _ -> foldPattern color xs (Gap : acc)
    | Just color == x -> case head acc of
      Gap -> foldPattern color xs (Seq 1 : acc)
      Seq _ -> foldPattern color xs (fmap (+1) (head acc) : tail acc)
    | otherwise -> [Gap]

-- counts number of fields with stone of refColor
-- returns Nothing if a field stone of other color is found
rateSequence :: Color -> [Maybe Color] -> Maybe Int
rateSequence refColor sequence = sequenceResult
  where
    sequenceResult = Just (^2) <*> sequenceSum -- non-linear scaling
    sequenceSum = foldr (liftM2 (+)) (Just 0) (convert refColor sequence)

convert :: Color -> [Maybe Color] -> [Maybe Int]
convert refColor colorList = map toInt colorList
  where
    toInt color
      | color == Nothing = Just 0                 -- empty field
      | Maybe.fromJust color == refColor = Just 1 -- field with proponent's color
      | otherwise = Nothing                       -- field with opponent's stone


-- Game Tree

-- generates whole game tree without evaluation
gametree :: Game -> GameTree
gametree game = Tree.Node game (map gametree (moves game))

-- limit gametree to some depth
prune :: Int -> GameTree -> GameTree
-- prune _ tree@(Tree.Node _ []) = tree
prune 0 (Tree.Node node _) = Tree.Node node []
prune n (Tree.Node node subtrees) = Tree.Node node (map (prune (n - 1)) subtrees)

-- limits gametree to only moves that are adjacent to those already made
trim :: GameTree -> GameTree
trim leafNode@(Tree.Node _ []) = leafNode
trim (Tree.Node game subtrees) = Tree.Node game trimmedSubtrees
  where
    trimmedSubtrees = map trim filteredSubtrees
    filteredSubtrees = filter (adjacentGame game) subtrees

adjacentGame :: Game -> GameTree -> Bool
adjacentGame (Game board _ _) (Tree.Node (Game _ move _) _) = any (adjacent (getPos move)) (takenPositions board)

adjacent :: Position -> Position -> Bool
adjacent (Pos (x1, y1)) (Pos (x2, y2))
  | abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1 = True
  | otherwise = False

takenPositions :: Board -> [Position]
takenPositions (Board boardMap) = map fst $ Map.toList boardMap

-- terminates tree in nodes that are resulting in a win by whichever side
terminate :: GameTree -> GameTree
terminate (Tree.Node game subtrees)
  | isEndGame game == True = Tree.Node game []
  | otherwise = Tree.Node game (map terminate subtrees)

isEndGame :: Game -> Bool
isEndGame (Game board _ _)
  | radius (playingRegion board) < 5 = False
  | otherwise = any (Seq 5 `elem`) (boardWidePatterns C boardWithoutWhite) ||
                any (Seq 5 `elem`) (boardWidePatterns B boardWithoutBlack)
    where
      boardWithoutWhite = Board $ (Map.fromList . filter (isOfColor C) . Map.toList) (getMap board)
      boardWithoutBlack = Board $ (Map.fromList . filter (isOfColor B) . Map.toList) (getMap board)
      isOfColor :: Color -> (Position, Color) -> Bool
      isOfColor refColor (Pos (_,_), color) = refColor == color


rowRadius :: Region -> Int
rowRadius ((x1, _), (x2, _)) = x2 - x1 + 1

colRadius :: Region -> Int
colRadius ((_, y1), (_, y2)) = y2 - y1 + 1

radius :: Region -> Int
radius point = max (rowRadius point) (colRadius point)

playingRegion :: Board -> Region
playingRegion board =
    case length taken of
      0 -> ((0,0),(0,0))
      _ -> (start, end)
      where
        start = (minimum rowCoords, minimum colCoords)
        end = (maximum rowCoords, maximum colCoords)
        rowCoords = map (fst . getCoords) taken
        colCoords = map (snd . getCoords) taken
        taken = takenPositions board


-- MiniMax
-- This code is based on solution presented in "Why Functional Programming Matters" by John Hughes

maximize :: Tree.Tree Game -> Int
maximize = maximum . maximize'

maximize' :: Tree.Tree Game -> [Int]
maximize' (Tree.Node node []) = [staticEvaluation (getBoard node)]
maximize' (Tree.Node _ subtrees) = mapMinimum (map minimize' subtrees)

-- flatten list by calculating minimum of necessary sublists
mapMinimum :: [[Int]] -> [Int]
mapMinimum (xs:xss) = firstMinimum : (omitForPotentialMax firstMinimum xss)
  where
    firstMinimum = minimum xs

minimize :: Tree.Tree Game -> Int
minimize = minimum . minimize'

minimize' :: Tree.Tree Game -> [Int]
minimize' (Tree.Node node []) = [staticEvaluation (getBoard node)]
minimize' (Tree.Node _ subtrees) = mapMaximum (map maximize' subtrees)

-- flatten list by calculating maximums of necessary sublists
mapMaximum :: [[Int]] -> [Int]
mapMaximum (xs:xss) = firstMaximum : (omitForPotentialMin firstMaximum xss)
  where
    firstMaximum = maximum xs

-- discards all sublists which minimum is lower or equal to passed potential maximum
omitForPotentialMax :: Int -> [[Int]] -> [Int]
omitForPotentialMax _ [] = []
omitForPotentialMax potentialMax (xs:xss)
  | minimumLEQ potentialMax xs == True = omitForPotentialMax potentialMax xss
  | otherwise = (minimum xs) : (omitForPotentialMax (minimum xs) xss) -- new potential maximum

-- discards all sublists which maximum is greater or equal to passed potential minimum
omitForPotentialMin :: Int -> [[Int]] -> [Int]
omitForPotentialMin _ [] = []
omitForPotentialMin potentialMin (xs:xss)
  | maximumGEQ potentialMin xs == True = omitForPotentialMin potentialMin xss
  | otherwise = (maximum xs) : (omitForPotentialMin (maximum xs) xss) -- new potential minimum

-- returns true if minimum of passed list is lower or equal to value
minimumLEQ :: Int -> [Int] -> Bool
minimumLEQ _ [] = False
minimumLEQ value (x:xs)
  | x <= value = True
  | otherwise = minimumLEQ value xs

-- returns true if maximum of passed list is greater or equal to value
maximumGEQ :: Int -> [Int] -> Bool
maximumGEQ _ [] = False
maximumGEQ value (x:xs)
  | x >= value = True
  | otherwise = maximumGEQ value xs

evaluateNode :: (Tree.Tree Game -> Int) -> Tree.Tree Game -> Tree.Tree Game
evaluateNode function tree@(Tree.Node (Game board move _) subtrees) =
                            Tree.Node (Game board move (function tree)) subtrees

evaluateTree :: Tree.Tree Game -> Tree.Tree Game
evaluateTree (Tree.Node (Game board move _) subtrees) =
  case getCol move of
    B -> Tree.Node (Game board move minValue) maximizedNodes -- black's turn
    C -> Tree.Node (Game board move maxValue) minimizedNodes -- white's turn
    where
      minValue = minimum $ map (getVal . Tree.rootLabel) maximizedNodes
      maxValue = maximum $ map (getVal . Tree.rootLabel) minimizedNodes
      maximizedNodes = map (evaluateNode maximize) subtrees
      minimizedNodes = map (evaluateNode minimize) subtrees

moveValuePair :: Tree.Tree Game -> (Int, Move)
moveValuePair (Tree.Node (Game _ move value) _) = (value, move)

chooseBestNextMove :: Tree.Tree Game -> Move
chooseBestNextMove (Tree.Node (Game _ _ value) subtrees) = Maybe.fromJust $ (lookup value . map moveValuePair) subtrees

makeMove :: Game -> Int -> Game
makeMove game searchDepth = Game newBoard madeMove 0
  where
    madeMove = (chooseBestNextMove . evaluateTree . terminate . trim . prune searchDepth . gametree) game
    newBoard = insert (getBoard game) (getCol madeMove) (getPos madeMove)


-- Interactive

main :: IO ()
main = do
  putStrLn "GOMOKU by Jakub Gwizdała\n"
  chosenDifficulty <- askForDifficulty
  chosenColor <- askForColor
  startGame <- case chosenColor of
    C -> return emptyGame
    B -> return initGame
  print startGame
  play startGame chosenColor chosenDifficulty

play :: Game -> Color -> Int -> IO ()
play game playerColor searchDepth = do
  chosenPosition <- askForPosition game
  let afterPlayer = Game (insert (getBoard game) playerColor chosenPosition) (Move chosenPosition playerColor) 0
  print afterPlayer
  putStrLn "Computer's move in progress..."
  case isEndGame afterPlayer of
    True -> do
      putStrLn "You won! Congratulations!"
      return ()
    False -> do
      let afterComputer = makeMove afterPlayer searchDepth
      print afterComputer
      case isEndGame afterComputer of
        True -> do
          putStrLn "You lost :("
          return ()
        False -> do
          play afterComputer playerColor searchDepth

askForDifficulty :: IO Int
askForDifficulty = do
  putStrLn "Difficulty determines how many moves does the computer look ahead."
  putStrLn "The higher the difficulty level the longer you have to wait for computer's move, e.g.:"
  putStrLn "3 - a few minutes\n4 - up to 20 minutes"
  putStr "Choose difficulty level: "
  hFlush stdout
  input <- getLine
  return (read input :: Int)

askForColor :: IO Color
askForColor = do
  putStr "Choose your color (b - black, w - white): "
  hFlush stdout
  input <- getLine
  case input of
    "b" -> return C
    "w" -> return B
    _ -> do
      putStrLn "Invalid input, try again."
      askForColor

askForPosition :: Game -> IO Position
askForPosition game = do
  putStr "Enter next move as <row> <col>: "
  hFlush stdout
  input <- getLine
  coords <- return $ (map (\x -> read x :: Int) . words) input
  case length coords of
    2 -> case areInBounds coords of
      True -> do
        let pos = Pos (head coords, last coords)
        case free pos $ (getMap . getBoard) game of
          True -> return pos
          False -> do
            putStrLn "This field is already taken, please choose other."
            askForPosition game
      False -> do
        putStrLn "Invalid value of coordinates, try again."
        askForPosition game
    _ -> do
      putStrLn "Invalid number of coordinates, try again."
      askForPosition game

areInBounds :: [Int] -> Bool
areInBounds nums = all (>=1) nums && all (<=19) nums


-- Settings

boardSize :: Int
boardSize = 19

searchDepth :: Int
searchDepth = 3

emptyBoard :: Board
emptyBoard = Board Map.empty

emptyGame :: Game
emptyGame = Game emptyBoard (Move (Pos (-1,-1)) B) 0

initBoard :: Board
initBoard = Board (Map.fromList [(Pos (10, 10), C)])

initGame :: Game
initGame = Game initBoard (Move (Pos (10, 10)) C) 0


-- Tests

testBoard :: Board
testBoard = Board (Map.fromList [(Pos (6, 11), B),
                                 (Pos (7, 9), C), (Pos (7, 10), B),
                                 (Pos (8, 9), B), (Pos (8, 10), C),
                                 (Pos (9,8), B), (Pos (9, 9), B), (Pos (9, 10), C), (Pos (9, 11), B),
                                 (Pos (10,7), B), (Pos (10, 9), C), (Pos (10, 10), C),
                                 (Pos (11,6), B)])

testGame :: Game
testGame = Game testBoard (Move (Pos (6, 11)) B) 0

autoplay :: Int -> Game -> IO Game
autoplay 0 game = do
  showGameState game
  return game
autoplay n game = do
  showGameState game
  next <- return $ makeMove game searchDepth
  autoplay (n - 1) next

benchmarkPlay :: Int -> Game -> IO ()
benchmarkPlay nMoves game = do
  putStrLn "Gomoku Test"
  putStrLn $ "number of moves = " ++ show nMoves ++ ", search depth = " ++ show searchDepth
  putStrLn "Computing..."
  start <- getCPUTime
  endState <- autoplay nMoves game
  print endState
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
      hour = floor (diff / 3600)
      minutes = floor $ (diff - fromIntegral (hour * 3600)) / 60
      seconds = diff - fromIntegral (hour * 3600) - fromIntegral (minutes * 60)
  printf "Test time: %0d h %0d min %0.3f sec\n" (hour :: Int) (minutes :: Int) (seconds :: Double)
  let avgMoveDiff = diff / fromIntegral nMoves
      avgMoveMinutes = floor $ avgMoveDiff / 60
      avgMoveSeconds = avgMoveDiff - fromIntegral (avgMoveMinutes * 60)
  printf "Avg. move time: %0d min %0.3f sec\n" (avgMoveMinutes :: Int) (avgMoveSeconds :: Double)
  putStrLn "Done."
  return ()

benchmarkEvaluation :: IO ()
benchmarkEvaluation = do
  putStrLn "Evaluation"
  putStrLn "Computing..."
  start <- getCPUTime
  eval <- return $ staticEvaluation testBoard
  putStrLn $ "Eval: " ++ show eval
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
      hour = floor (diff / 3600)
      minutes = floor $ (diff - fromIntegral (hour * 3600)) / 60
      seconds = diff - fromIntegral (hour * 3600) - fromIntegral (minutes * 60)
  printf "Test time: %0d h %0d min %0.3f sec\n" (hour :: Int) (minutes :: Int) (seconds :: Double)
  putStrLn "Done."
  return ()


-- Debug

showGameState :: Game -> IO ()
showGameState (Game _ move _) = putStr $ show move ++ "\n"

treeToStringTree :: Tree.Tree Game -> Tree.Tree String
treeToStringTree (Tree.Node node forest) = Tree.Node (show node) (map treeToStringTree forest)

showTree :: Tree.Tree Game -> IO ()
showTree = putStrLn . Tree.drawTree . treeToStringTree

showTreeTop :: Tree.Tree Game -> IO ()
showTreeTop (Tree.Node node _) = print node

nTreeNodes :: Tree.Tree a -> Int
nTreeNodes (Tree.Node _ []) = 1
nTreeNodes (Tree.Node _ subtrees) = 1 + (sum . map nTreeNodes) subtrees
