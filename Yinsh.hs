{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (mfilter)
import Data.Foldable (any, fold, foldMap,foldl')
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (any)
import qualified Data.Map as Map
import qualified Data.Set as Set

--
-- Game rule parameters
--

gridHeights     = [8,9,8,7,6,3]
gameRadius      = length gridHeights - 1
goalRunLength   = 5
goalScore       = 3
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
windowSize      = (700,700)
windowLocation  = (10,10)
windowTitle     = "Yinsh"

--
-- Type definitions
--

data Coord = C Int Int deriving (Ord, Eq)

data Player = Black | White
  deriving (Eq)

data PieceKind = Hollow | Solid
  deriving (Eq)

data Piece = Piece
  { piecePlayer :: Player
  , pieceKind   :: PieceKind
  }
  deriving (Eq)

data GameState = GameState
  { board       :: Map Coord Piece
  , cursor      :: Maybe Coord
  , turn        :: Player
  , mode        :: GameMode
  , whiteScore  :: Int
  , blackScore  :: Int
  }

data GameMode
  = Setup Int | PickRing | PlaceRing Coord
              | RemoveFive Player (Set Coord)
              | RemoveRing Player
              | GameOver

initialGameState = GameState
  { board       = Map.empty
  , cursor      = Nothing
  , turn        = startingPlayer
  , mode        = Setup startingRings
  , whiteScore  = 0
  , blackScore  = 0
  }

main =
  play
    (InWindow windowTitle windowSize windowLocation)
    white
    0
    initialGameState
    drawGameState
    handleEvents
    handleTick

handleEvents (EventMotion pt) s
  | inBounds c = s { cursor = Just c }
  | otherwise  = s { cursor = Nothing }
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
   ,foldMap drawCursor (cursor s)
   ,drawMode s
   ,hexGridPicture
   ,drawBoard (board s)
   ,drawPickFive s
   ,drawScore White (whiteScore s) (C (-5) (-2))
   ,drawScore Black (blackScore s) (C 3 3)
   ]

drawScore who n pos
  = translateC pos
  $ foldMap (\offset -> translate offset 0 $ drawPiece (Piece who Hollow))
  $ take n [0, gridSize, 2*gridSize]

drawMode s =
  case mode s of
    PlaceRing c -> drawPieceAt c (Piece (turn s) Solid)
    _ -> blank

drawPickFive s =
  case mode s of
    RemoveFive _ xs -> foldMap drawMarker xs
    _ -> blank

drawMarker coord = translateC coord
                 $ color yellow
                 $ rotate 45    bar
                <> rotate (-45) bar
  where
  bar = rectangleSolid 10 25

drawBoard = fold . Map.mapWithKey drawPieceAt

drawPieceAt c = translateC c . drawPiece

drawCursor c = translateC c
             $ color orange
             $ circleSolid solidRadius

drawPiece p = color (playerToColor (piecePlayer p))
            $ drawToken (pieceKind p)

drawToken Hollow = thickCircle ringRadius ringWidth
drawToken Solid  = circleSolid solidRadius

drawTurn s       = translateC turnIndicatorCoord
                 $ case mode s of
                     Setup n            -> drawPiece (Piece (turn s) Hollow)
                                        <> drawCounter n
                     PickRing           -> drawPiece (Piece (turn s) Solid)
                     PlaceRing {}       -> drawPiece (Piece (turn s) Hollow)
                     RemoveFive w _     -> drawPiece (Piece w Solid)
                                        <> drawMarker (C 0 0)
                     RemoveRing w       -> drawPiece (Piece w Hollow)
                                        <> drawMarker (C 0 0)
                     GameOver           -> blank

drawCounter   = translate (-7) (-10)
              . scale 0.2 0.2
              . text
              . show

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
  case mode s of
    Setup n
      | available (board s) c ->
          endSetupTurn n
          s { turn = toggleTurn $ turn s
            , board = Map.insert c (Piece (turn s) Hollow)
                    $ board s
            }

    PickRing
      | clickedPiece == Just (Piece (turn s) Hollow) ->
            s { mode = PlaceRing c }

    PlaceRing ring
      | clickedPiece == Just (Piece (turn s) Hollow) ->
            s { mode = PlaceRing c }

      | legalMove ring c (board s) ->
           endTurn
           s { board = Map.insert ring (Piece (turn s) Solid)
                     $ Map.insert c    (Piece (turn s) Hollow)
                     $ flipThrough (movesThrough ring c)
                     $ board s
             }

    RemoveFive who chosen
      | clickedPiece == Just (Piece who Solid) ->
            removeFiveLogic who s (toggleMembership c chosen)

    RemoveRing who
      | clickedPiece == Just (Piece who Hollow) ->
           endTurn $ incScore who
                     s { board = Map.delete c $ board s }

    _ -> s

  where
  clickedPiece = Map.lookup c (board s)

toggleMembership c chosen
  | Set.member c chosen = Set.delete c chosen
  | otherwise           = Set.insert c chosen

incScore White s = s { whiteScore = whiteScore s + 1 }
incScore Black s = s { blackScore = blackScore s + 1 }

endTurn s
  -- the current player gets to remove first
  | whiteScore s >= goalScore || blackScore s >= goalScore =
     s { mode = GameOver }

  | hasRun (turn s) (board s) =
     s { mode = RemoveFive (turn s) Set.empty }

  | hasRun other (board s) =
     s { mode = RemoveFive other Set.empty }

  | otherwise =
     s { turn = toggleTurn $ turn s
       , mode = PickRing
       }
  where
  other = toggleTurn $ turn s

-- | Search the board for a run of solid pieces long enough
-- to be removed and owned by the specified player
hasRun who b = any startsRun coords
  where
  coords = Map.keysSet $ Map.filter (== expected) b

  startsRun c = any (startsRunWithDir 0 c) runDirections

  expected = Piece who Solid

  startsRunWithDir n c step
    | Set.member c coords = startsRunWithDir (n+1) (step c) step
    | otherwise = n >= goalRunLength

runDirections =
  [\(C x y) -> C (x+1) y
  ,\(C x y) -> C x     (y+1)
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
         s { mode = RemoveRing who
           , board = deleteMany chosen $ board s
           }
  | otherwise =
         s { mode = RemoveFive who chosen }

deleteMany xs m = foldl' (\acc i -> Map.delete i acc) m xs

flipThrough xs b = foldl' (\acc i -> flip1 i acc) b xs
  where
  flip1 = Map.update (\p -> Just p { piecePlayer = toggleTurn $ piecePlayer p })

legalMove c1 c2 b =
  not (null xs) && available b c2 &&
  all (occupied b) (dropWhile (available b) (tail (init xs))) &&
  all (\i -> fmap pieceKind (Map.lookup i b) /= Just Hollow) (tail xs)
  where
  xs = movesThrough c1 c2

movesThrough (C x1 y1) (C x2 y2)
  | x1 == x2  = [C x1 y | y <- enum y1 y2]
  | y1 == y2  = [C x y1 | x <- enum x1 x2]
  | (x1-x2) == (y2-y1) = [C x y | (x,y) <- zip (enum x1 x2) (enum y1 y2)]
  | otherwise = []

enum a b
  | a <= b      = [a,a+1..b]
  | otherwise   = [a,a-1..b]

toggleTurn Black = White
toggleTurn White = Black

endSetupTurn n s
  | turn s == startingPlayer    = s { mode = mode' }
  | otherwise                   = s
  where
  mode' | n == 1        = PickRing
        | otherwise     = Setup (n-1)

occupied  b c = Map.member c b
available b c = not $ Map.member c b
