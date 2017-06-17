module Main where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Control.Monad

import System.CPUTime
import Text.Printf

type Field = (Position, Color)
type BoardMap = Map.Map Position Color
type GameTree = Tree.Tree Game


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

-- current board with evaluation beeing result or recent move
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

type Point = (Int, Int)
type Region = (Point, Point)

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
  where
    allBoardRegion = ((1,1),(19,19))

rows :: Int -> Region -> Board -> [[Maybe Color]]
rows seqLen region board = map (contents board . seqToPos . rowSequence seqLen) (rowStartPoints seqLen region)

cols :: Int -> Region -> Board -> [[Maybe Color]]
cols seqLen region board = map (contents board . seqToPos . colSequence seqLen) (colStartPoints seqLen region)

rDiag :: Int -> Region -> Board -> [[Maybe Color]]
rDiag seqLen region board = map (contents board . seqToPos . rDiagSequence seqLen) (rDiagStartPoints seqLen region)

lDiag :: Int -> Region -> Board -> [[Maybe Color]]
lDiag seqLen region board = map (contents board . seqToPos . lDiagSequence seqLen) (lDiagStartPoints seqLen region)

-- evaluation function for MinMax

-- white is a maximizer, black is a minimizer
staticEvaluation :: Board -> Int
staticEvaluation board = evaluateBoardForColor board B - evaluateBoardForColor board C

evaluateBoardForColor :: Board -> Color -> Int
evaluateBoardForColor board color
  | Just 25 `elem` filteredRates = 100000   -- score for 5 in a row
  | otherwise = resultValue
  where
    resultValue = Maybe.fromJust result
    result = foldr (liftM2 (+)) (Just 0) filteredRates
    filteredRates = filter (/= Nothing) rates
    rates = map (ratePattern color) (allSequences 5 board)

-- counts number of fields with stone of refColor
-- returns Nothing if a field stone of other color is found
ratePattern :: Color -> [Maybe Color] -> Maybe Int
ratePattern refColor pattern = patternResult  -- non-linear scaling
  where
    patternResult = liftM2 (*) patternCount patternCount
    patternCount = foldr (liftM2 (+)) (Just 0) (convert refColor pattern)

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
  | otherwise = any (straightSeq 5 C) (contentsInRegion 5 (playingRegion board) board) ||
                any (straightSeq 5 B) (contentsInRegion 5 (playingRegion board) board)

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

contentsInRegion :: Int -> Region -> Board -> [[Maybe Color]]
contentsInRegion seqLen region board = map (contents board . seqToPos . rowSequence seqLen) (rowStartPoints seqLen region) ++
                                       map (contents board . seqToPos . colSequence seqLen) (colStartPoints seqLen region) ++
                                       map (contents board . seqToPos . rDiagSequence seqLen) (rDiagStartPoints seqLen region) ++
                                       map (contents board . seqToPos . lDiagSequence seqLen) (lDiagStartPoints seqLen region)

straightSeq :: Int -> Color -> [Maybe Color] -> Bool
straightSeq 0 _ [] = True
straightSeq n col (x:xs)
  | Just col == x = straightSeq (n - 1) col xs
  | otherwise = False


-- MiniMax

maximize :: Tree.Tree Game -> Int
maximize = maximum . maximize'

maximize' :: Tree.Tree Game -> [Int]
maximize' (Tree.Node node []) = [staticEvaluation (getBoard node)]
maximize' (Tree.Node _ subtrees) = mapMinimum (map minimize' subtrees)

-- flatten list by calculating minimum of necessary sublists
mapMinimum :: [[Int]] -> [Int]
mapMinimum (xs:xss) = firstMinimum : (omitMax firstMinimum xss)
  where
    firstMinimum = minimum xs

minimize :: Tree.Tree Game -> Int
minimize = minimum . minimize'

minimize' :: Tree.Tree Game -> [Int]
minimize' (Tree.Node node []) = [staticEvaluation (getBoard node)]
minimize' (Tree.Node _ subtrees) = mapMaximum (map maximize' subtrees)

-- flatten list by calculating maximums of necessary sublists
mapMaximum :: [[Int]] -> [Int]
mapMaximum (xs:xss) = firstMaximum : (omitMin firstMaximum xss)
  where
    firstMaximum = maximum xs

-- discards all sublists which minimum is lower or equal to passed potential maximum
omitMax :: Int -> [[Int]] -> [Int]
omitMax _ [] = []
omitMax potentialMax (xs:xss)
  | minimumLEQ potentialMax xs == True = omitMax potentialMax xss
  | otherwise = (minimum xs) : (omitMax (minimum xs) xss) -- new potential maximum

-- discards all sublists which maximum is greater or equal to passed potential minimum
omitMin :: Int -> [[Int]] -> [Int]
omitMin _ [] = []
omitMin potentialMin (xs:xss)
  | maximumGEQ potentialMin xs == True = omitMin potentialMin xss
  | otherwise = (maximum xs) : (omitMin (maximum xs) xss) -- new potential minimum

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

makeMove :: Game -> Game
makeMove game = Game newBoard madeMove 0
  where
    madeMove = (chooseBestNextMove . evaluateTree . terminate . trim . prune searchDepth . gametree) game
    newBoard = insert (getBoard game) (getCol madeMove) (getPos madeMove)



-- Settings

boardSize :: Int
boardSize = 19

startPoint :: Int
startPoint
  | odd boardSize = (boardSize + 1) `div` 2
  | otherwise = boardSize `div` 2

searchDepth :: Int
searchDepth = 4

emptyBoard :: Board
emptyBoard = Board Map.empty

emptyGame :: Game
emptyGame = Game emptyBoard (Move (Pos (-1,-1)) B) 0

initBoard :: Board
initBoard = Board (Map.fromList [(Pos (startPoint,startPoint), C)])

initGame :: Game
initGame = Game initBoard (Move (Pos (startPoint,startPoint)) C) 0


-- Tests

board6 :: Board
board6 = Board (Map.fromList [(Pos (1, 1), C), (Pos (1, 3), B), (Pos (1, 5), C), (Pos (1, 6), C),
                              (Pos (2, 2), B),
                              (Pos (3, 2), B), (Pos (3, 4), C),
                              (Pos (4, 2), B),
                              (Pos (5, 2), B), (Pos (5, 3), B), (Pos (5, 4), C), (Pos (5, 5), B),
                              (Pos (6, 2), B), (Pos (6, 4), C)])

game6 :: Game
game6 = Game board6 (Move (Pos (1, 1)) C) 0

winningBoard :: Board
winningBoard = Board (Map.fromList [(Pos (1,1), C), (Pos (1,2), C), (Pos (1,3), C), (Pos (1,4), C), (Pos (1,5), C)])

winningGame :: Game
winningGame = Game winningBoard (Move (Pos (1,5)) C) 0

autoplay :: Int -> Game -> IO Game
autoplay 0 game = do
  showGameState game
  return game
autoplay n game = do
  showGameState game
  next <- return $ makeMove game
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
  putStrLn "Done."
  return ()

benchmarkEvaluation :: IO ()
benchmarkEvaluation = do
  putStrLn "Evaluation"
  putStrLn "Computing..."
  start <- getCPUTime
  eval <- return $ staticEvaluation board6
  end <- getCPUTime
  putStrLn $ "Eval: " ++ show eval
  let diff = (fromIntegral (end - start)) / (10^12)
      hour = floor (diff / 3600)
      minutes = floor $ (diff - fromIntegral (hour * 3600)) / 60
      seconds = diff - fromIntegral (hour * 3600) - fromIntegral (minutes * 60)
  printf "Test time: %0d h %0d min %0.3f sec\n" (hour :: Int) (minutes :: Int) (seconds :: Double)
  putStrLn "Done."
  return ()

main :: IO ()
main = benchmarkEvaluation


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

-- lista funkcji aplikowanych na mapę
