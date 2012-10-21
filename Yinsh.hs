{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad (mfilter)
import Data.Foldable (fold, foldMap,foldl')
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map
import qualified Data.Set as Set

--
-- Game rule parameters
--
gridHeights     = [8,9,8,7,6,3]
gameRadius      = length gridHeights - 1
goalRunLength   = 5
startingRings   = 5
startingPlayer  = White

--
-- Game rendering parameters
--
playerToColor Black = black
playerToColor White = greyN 0.7
gridSize        = 70
ringRadius      = 30
ringWidth       = 7
solidRadius     = 22
turnIndicatorCoord = C (-5) 7

data Coord = C Int Int deriving (Ord, Eq)

data Player = Black | White
  deriving (Eq)

data PieceKind = Hollow | Solid
  deriving (Eq)

data Piece = Piece
  { _piecePlayer :: Player
  , _pieceKind  :: PieceKind
  }
  deriving (Eq)

data GameState = GameState
  { _board       :: Map Coord Piece
  , _cursor      :: Maybe Coord
  , _turn        :: Player
  , _mode        :: GameMode
  }

data GameMode
  = Setup Int | PickRing | PlaceRing Coord
              | RemoveFive Player (Set Coord)
              | RemoveRing Player

initialGameState = GameState
  { _board       = Map.empty
  , _cursor      = Nothing
  , _turn        = startingPlayer
  , _mode        = Setup startingRings
  }

makeLenses ''GameState
makeLenses ''Piece

main =
  play
    (InWindow "Yinsh" (700,700) (10,10))
    white
    0
    initialGameState
    drawGameState
    handleEvents
    handleTick

handleEvents (EventMotion pt) s
  | inBounds c = cursor ?~ pointCoord pt $ s
  | otherwise  = cursor .~ Nothing       $ s
  where
  c = pointCoord pt

handleEvents (EventKey (MouseButton LeftButton) Down _ pt) s
  | inBounds c = playMove c s
  where
  c = pointCoord pt

handleEvents _ s = s

handleTick   _ s = s

drawGameState s =
  pictures
   [drawTurn s
   ,foldMap drawCursor (cursor ^$ s)
   ,drawMode s
   ,hexGridPicture
   ,drawBoard (board ^$ s)
   ,drawPickFive s
   ]

drawMode s =
  case mode ^$ s of
    PlaceRing c -> drawPieceAt c (Piece (s^.turn) Solid)
    _ -> blank

drawPickFive s =
  case mode ^$ s of
    RemoveFive _ xs -> foldMap drawMarker xs
    _ -> blank

drawMarker coord = translateC coord
                 $ color yellow
                 $ rotate 45    bar
                <> rotate (-45) bar
  where
  bar = rectangleSolid 10 25

drawBoard = fold . Map.mapWithKey drawPieceAt

drawCursor c = translateC c
             $ color orange
             $ circleSolid solidRadius

drawPieceAt :: Coord -> Piece -> Picture
drawPieceAt c p = translateC c
                $ drawPiece  p

drawPiece p = color (playerToColor (piecePlayer ^$ p))
            $ drawToken (pieceKind ^$ p)

drawToken Hollow = thickCircle ringRadius ringWidth
drawToken Solid  = circleSolid solidRadius

drawTurn s       = translateC turnIndicatorCoord
                 $ case mode ^$ s of
                     Setup n  -> drawPiece (Piece (s^.turn) Hollow)
                                 <> translate (-7) (-10)
                                    (scale 0.2 0.2 (text (show n)))
                     PickRing -> drawPiece $ Piece (s^.turn) Solid
                     PlaceRing {} -> drawPiece $ Piece (s^.turn) Hollow
                     RemoveFive w _ -> drawPiece (Piece w Solid)
                                       <> drawMarker (C 0 0)
                     RemoveRing w -> drawPiece (Piece w Hollow)
                                       <> drawMarker (C 0 0)

translateC c = uncurry translate (coordPoint c)

coordPoint :: Coord -> Point
coordPoint (C x y) = (sqrt 3 / 2 * gridSize * fromIntegral x,
                      gridSize * (fromIntegral y + fromIntegral x / 2))

pointCoord :: Point -> Coord
pointCoord (x,y) = C (round xc) (round yc)
  where
  xc = 2/(gridSize*sqrt 3) * x
  yc = y/gridSize-xc/2

hexGridPicture :: Picture
hexGridPicture =
  pictures [onedir
           ,rotate 120 onedir
           ,rotate 240 onedir
           ]
  where
  onedir = pictures
    [ coordLine [C x ((-x-h)`div`2)
                ,C x ((-x-h)`div`2+h)]
    | x <- [negate gameRadius .. gameRadius]
    , let h = gridHeights !! abs x
    ]

inBounds (C x y) =
  abs x <= gameRadius &&
  colLo <= y && y <= colLo + h
  where
  h = gridHeights !! abs x
  colLo = (-x-h)`div`2

coordLine = line . map coordPoint

--
-- Game rules
--

playMove c s =
  case mode ^$ s of
    Setup n
      | available (board ^$ s) c ->
          advanceSetup $ turn %~ toggleTurn
                       $ board.at c ?~ Piece (turn ^$ s) Hollow $ s
      | otherwise -> s

    PickRing
      | s^.board.at c == Just (Piece (s^.turn) Hollow) ->
            mode .~ PlaceRing c $ s
      | otherwise -> s

    PlaceRing ring
      | legalMove ring c (board ^$ s) ->
           endTurn
         $ board.at ring ?~ Piece (s^.turn) Solid
         $ board.at c    ?~ Piece (s^.turn) Hollow
         $ board         %~ flipThrough (movesThrough ring c)
         $ s
      | otherwise -> s

    RemoveFive who chosen
      | s^.board.at c == Just expected ->
            removeFiveLogic who s (contains c %~ not $ chosen)
      | otherwise -> s
      where
      expected = Piece who Solid

    RemoveRing who
      | s^.board.at c == Just expected ->
           endTurn $ board.at c .~ Nothing $ s
      | otherwise -> s
      where
      expected = Piece who Hollow
      
endTurn s
  -- the current player gets to remove first
  | hasRun (s^.turn) (s^.board) =
     mode .~ RemoveFive (s^.turn) Set.empty $ s

  | hasRun other (s^.board) =
     mode .~ RemoveFive other Set.empty $ s

  | otherwise = turn %~ toggleTurn
              $ mode .~ PickRing
              $ s
  where
  other = toggleTurn $ turn ^$ s

hasRun who b = any startsRun $ Map.keys b
  where
  startsRun c = any (startsRunWithDir 0 c) runDirections

  expected = Piece who Solid

  startsRunWithDir n c step
    | n >= goalRunLength = True
    | b^.at c == Just expected = startsRunWithDir (n+1) (step c) step
    | otherwise = False

runDirections = [\(C x y) -> C (x+1) y
               ,\(C x y) -> C x (y+1)
               ,\(C x y) -> C (x+1) (y-1)
               ]

-- expects a sorted list
testChosenGroup xs = any (check1 xs) runDirections
  where
  check1 (x:y:z) step = step x == y && check1 (y:z) step
  check1 _ _ = True

removeFiveLogic who s chosen
  | Set.size chosen == goalRunLength
    && testChosenGroup (Set.toList chosen) =
                mode  .~ RemoveRing who
              $ board %~ deleteMany chosen
              $ s
  | otherwise = mode .~ RemoveFive who chosen $ s 

deleteMany xs m = foldl' (\acc i -> Map.delete i acc) m xs

flipThrough xs b = foldr (\i -> at i.mapped.piecePlayer%~toggleTurn) b xs

legalMove c1 c2 b =
  not (null xs) && available b c2 &&
  all (occupied b) (dropWhile (available b) (tail (init xs))) &&
  all (\i -> b^..at i.folded.pieceKind /= [Hollow]) (tail xs)
  where
  xs = movesThrough c1 c2

movesThrough (C x1 y1) (C x2 y2)
  | x1 == x2  = [C x1 y | y <- enum y1 y2]
  | y1 == y2  = [C x y1 | x <- enum x1 x2]
  | (x1-x2) == (y2-y1) = [C x y | (x,y) <- zip (enum x1 x2) (enum y1 y2)]
  | otherwise = []

enum a b
  | a <= b = [a,a+1..b]
  | otherwise = [a,a-1..b]

toggleTurn Black = White
toggleTurn White = Black

advanceSetup s =
  case s^.mode of
    Setup 1 | s^.turn == startingPlayer -> mode .~ PickRing    $ s
    Setup n | s^.turn == startingPlayer -> mode .~ Setup (n-1) $ s
    _                                   -> s

occupied  b c = Map.member c b
available b c = not $ Map.member c b
