{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad (mfilter)
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable (fold, foldMap)

gridSize :: Float
gridSize = 50

gridHeights :: [Int]
gridHeights = [3,6,7,8,9,8,9,8,7,6,3]

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

data GameMode = Setup Int | PickRing | PlaceRing Coord

initialGameState = GameState
  { _board       = Map.empty
  , _cursor      = Nothing
  , _turn        = White
  , _mode        = Setup 2 -- XXX should be 5
  }

makeLenses ''GameState
makeLenses ''Piece

main =
  play
    (InWindow "Yinsh" (500,500) (10,10))
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
   ]

drawMode s =
  case mode ^$ s of
    PlaceRing c -> drawPieceAt c (Piece (s^.turn) Solid)
    _ -> blank

drawBoard = fold . Map.mapWithKey drawPieceAt

drawCursor c = translateV (coordPoint c)
             $ color orange
             $ circleSolid 15

drawPieceAt :: Coord -> Piece -> Picture
drawPieceAt c p = translateV (coordPoint c)
                $ drawPiece  p

drawPiece p = color (playerToColor (piecePlayer ^$ p))
            $ drawToken (pieceKind ^$ p)

drawToken Hollow = thickCircle 20 5
drawToken Solid  = circleSolid 15

drawTurn s       = drawPieceAt (C (-5) 7) (Piece (s^.turn) modePiece)
  where
  modePiece = case mode ^$ s of
                Setup _ -> Hollow
                PickRing -> Solid
                PlaceRing _ -> Hollow

playerToColor Black = black
playerToColor White = greyN 0.7

translateV = uncurry translate

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
    | (x,h) <- zip [-5 .. 5] gridHeights
    ]

inBounds (C x y) =
  -5 <= x && x <= 5 &&
  colLo <= y && y <= colLo + h
  where
  h = gridHeights !! (x+5)
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
      | s^.board.at c == Just (Piece (s^.turn) Hollow) ->
            mode .~ PlaceRing c $ s -- change your mind
      | legalMove ring c (board ^$ s) ->
           turn %~ toggleTurn
         $ mode .~ PickRing
         $ board.at ring ?~ Piece (s^.turn) Solid
         $ board.at c    ?~ Piece (s^.turn) Hollow
         $ board %~ flipThrough (movesThrough ring c)
         $ s
      | otherwise -> s

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
  case (s^.mode,s^.turn) of
    (Setup 1, White) -> mode .~ PickRing $ s
    (Setup n, White) -> mode .~ Setup (n-1) $ s
    _                -> s

occupied  b c = Map.member c b
available b c = not $ Map.member c b
