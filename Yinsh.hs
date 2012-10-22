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
timerLength     = 60 -- seconds
turnIndicatorCoord = C (-5) 7
windowSize      = (700,700)
windowLocation  = (10,10)
windowTitle     = "Yinsh"

--
-- Type definitions
--

-- | Location on the hex grid
data Coord = C Int Int
  deriving (Ord, Eq)

data Player = Black | White
  deriving (Eq)

data PieceKind = Ring | Solid
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
  , timer       :: Float
  }

data Phase = PreTurn | PostTurn deriving Eq

data GameMode
  = Setup Int  -- ^ Players take turns placing their rings
  | PickRing   -- ^ Player choses a ring to place a solid in
  | PlaceRing Coord -- ^ Player choses a place to move his ring to
  | RemoveFive Phase (Set Coord) -- ^ Player selects run of 5 to remove
  | RemoveRing Phase -- ^ Player selects ring to remove
  | GameOver          -- ^ Game over, no more moves

initialGameState :: GameState
initialGameState = GameState
  { board       = Map.empty
  , cursor      = Nothing
  , turn        = startingPlayer
  , mode        = Setup startingRings
  , whiteScore  = 0
  , blackScore  = 0
  , timer       = 0
  }

main                            = play (InWindow windowTitle windowSize windowLocation)
                                       white
                                       1
                                       initialGameState
                                       drawGameState
                                       handleEvents
                                       handleTick

--
-- Game event logic
--

handleEvents                   :: Event -> GameState -> GameState
handleEvents (EventMotion pt) s
  | inBounds c                  = s { cursor = Just c }
  | otherwise                   = s { cursor = Nothing }
  where
  c                             = pointCoord pt

handleEvents (EventKey (MouseButton LeftButton) Down _ pt) s
  | inBounds c                  = playMove c s
  where
  c                             = pointCoord pt

handleEvents _ s                = s

handleTick                     :: Float -> GameState -> GameState
handleTick elapsed s            = s { timer = elapsed + timer s }

--
-- Game rendering
--

-- | Render a picture for the whole game.
drawGameState                  :: GameState -> Picture
drawGameState                   = fold
                                [ drawTimer . timer
                                , drawTurn
                                , foldMap drawCursor . cursor
                                , drawPieceInRing
                                , const hexGridPicture
                                , drawBoard . board
                                , drawPickFive . mode
                                , drawScore White (C (-5) (-2)) . whiteScore
                                , drawScore Black (C 3 3)       . blackScore
                                ]


drawTimer seconds               = translateC (C 5 (-7)) $ timers seconds timerColors
  where
  timerColors = [yellow,orange,red]

  timers n []                   = blank
  timers n (c:cs)
    | n < timerLength           = color c $ arcSolid 0 (n / timerLength * 360) solidRadius
    | otherwise                 = color c (circleSolid solidRadius) <> timers (n-timerLength) cs

drawScore who pos n             = translateC pos
                                $ foldMap (\offset -> translate offset 0 pic)
                                $ take n [0, gridSize, 2*gridSize]
  where
  pic                           = drawPiece (Piece who Ring)

drawPieceInRing s =
  case mode s of
    PlaceRing c                 -> drawPieceAt c (Piece (turn s) Solid)
    _                           -> blank

drawPickFive (RemoveFive _ xs)  = foldMap drawMarker xs
drawPickFive _                  = blank

drawMarker coord                = translateC coord
                                $ color yellow
                                $ rotate 45    bar
                               <> rotate (-45) bar
  where
  bar                           = rectangleSolid 10 25

drawBoard                       = fold . Map.mapWithKey drawPieceAt

drawPieceAt c                   = translateC c . drawPiece

drawCursor c                    = translateC c
                                $ color orange
                                $ circleSolid solidRadius

drawPiece p                     = color (playerToColor (piecePlayer p))
                                $ drawToken (pieceKind p)

drawToken Ring = thickCircle ringRadius ringWidth
drawToken Solid  = circleSolid solidRadius

drawTurn s                      = translateC turnIndicatorCoord pic
  where
  me  = turn s
  pic = case mode s of
          Setup n               -> drawPiece (Piece me Ring)
                                <> drawCounter n
          PickRing              -> drawPiece (Piece me Solid)
          PlaceRing {}          -> drawPiece (Piece me Ring)
          RemoveFive {}         -> drawPiece (Piece me Solid)
                                <> drawMarker (C 0 0)
          RemoveRing w          -> drawPiece (Piece me Ring)
                                <> drawMarker (C 0 0)
          GameOver              -> blank

-- | Render a small number to screen which fits inside a ring.
drawCounter                    :: Int -> Picture
drawCounter                     = translate (-7) (-10)
                                . scale 0.2 0.2
                                . text
                                . show

-- | Translate a picture to its hex coordinate location.
translateC                     :: Coord -> Picture -> Picture
translateC c                    = uncurry translate (coordPoint c)

-- | Convert a hex coordinate to a screen coordinate.
coordPoint                     :: Coord -> Point
coordPoint (C x y)              = (sqrt 3 / 2 * gridSize * xf,
                                   gridSize * (yf + xf/2))
  where
  xf                            = fromIntegral x
  yf                            = fromIntegral y

-- | Covert a screen coordinate to its nearest hex coordinate.
pointCoord                     :: Point -> Coord
pointCoord (x,y)                = C (round xc) (round yc)
  where
  xc                            = 2/(gridSize*sqrt 3) * x
  yc                            = y/gridSize-xc/2

-- | Static image of the hexagonal board.
hexGridPicture                 :: Picture
hexGridPicture                  = foldMap rotate [0, 60, 120]
                                $ pictures
                                [ lineC [C x colLo, C x colHi]
                                | x <- [- gameRadius .. gameRadius]
                                , let (colLo,colHi) = columnRange x
                                ]

-- | Return 'True' iff a hex coordinate is with-in the board boundaries.
inBounds                       :: Coord -> Bool
inBounds (C x y)                = abs x <= gameRadius &&
                                  colLo <= y && y <= colHi
  where
  (colLo,colHi)                 = columnRange x

columnRange                    :: Int -> (Int,Int)
columnRange x                   = (colLo, colLo+h)
  where
  h                             = gridHeights !! abs x
  colLo                         = (-x-h)`div`2

-- | Draw a line connecting hex coordinates
lineC                          :: [Coord] -> Picture
lineC                           = line . map coordPoint

--
-- Game rules
--

-- | Update the game state given a selected coordinate.
playMove                       :: Coord -> GameState -> GameState
playMove c s =
  case mode s of
    Setup n | available (board s) c
                                -> endSetupTurn n
                                   s { board = Map.insert c (Piece me Ring)
                                             $ board s
                                     , turn  = toggleTurn me
                                     , timer = 0
                                     }

    PickRing | clickedPiece == Just (Piece me Ring)
                                -> s { mode = PlaceRing c }

    PlaceRing _ | clickedPiece == Just (Piece me Ring)
                                -> s { mode = PlaceRing c }

    PlaceRing ring | clickedPiece == Nothing && legalMove ring c (board s)
                                -> endTurn PostTurn
                                   s { board = Map.insert ring (Piece me Solid)
                                             $ Map.insert c    (Piece me Ring)
                                             $ flipThrough (movesThrough ring c)
                                             $ board s
                                     }

    RemoveFive phase chosen | clickedPiece == Just (Piece me Solid)
                                -> removeFiveLogic phase s
                                 $ toggleMembership c chosen

    RemoveRing phase | clickedPiece == Just (Piece me Ring)
                                -> endTurn phase
                                 $ incScore
                                   s { board = Map.delete c $ board s }

    _                           -> s -- ignore all other selections

  where
  me                            = turn s
  clickedPiece                  = Map.lookup c $ board s

-- | Remove element from a set if it is a member, add it otherwise
toggleMembership               :: Ord a => a -> Set a -> Set a
toggleMembership c chosen
  | Set.member c chosen         = Set.delete c chosen
  | otherwise                   = Set.insert c chosen

-- | Add a point to the current player's score
incScore                       :: GameState -> GameState
incScore s = case turn s of
               White            -> s { whiteScore = whiteScore s + 1 }
               Black            -> s { blackScore = blackScore s + 1 }

-- | Update game state at the end of a turn. All runs will be
-- removed before the game advances to the next player's turn.
endTurn                        :: Phase -> GameState -> GameState
endTurn phase s
  | whiteScore s >= goalScore || blackScore s >= goalScore
                                = s { mode = GameOver }
  | hasRun (turn s) (board s)   = s { mode = RemoveFive phase Set.empty }
  | phase == PreTurn            = s { mode = PickRing }
  -- This player's turn is over
  | otherwise                   = endTurn PreTurn
                                  s { timer = 0, turn = toggleTurn $ turn s }

-- | Search the board for a run of solid pieces long enough
-- to be removed and owned by the specified player
hasRun                         :: Player -> Map Coord Piece -> Bool
hasRun who b                    = any startsRun coords
  where
  expected                      = Piece who Solid
  coords                        = Map.keysSet $ Map.filter (== expected) b
  startsRun c                   = any (startsRunWithDir 0 c) runDirections

  startsRunWithDir n c step
    | Set.member c coords       = startsRunWithDir (n+1) (step c) step
    | otherwise                 = n >= goalRunLength

-- | List of the possible movement functions which count for making runs
runDirections                  :: [Coord -> Coord]
runDirections                   = [\(C x y) -> C (x+1) y
                                  ,\(C x y) -> C x     (y+1)
                                  ,\(C x y) -> C (x+1) (y-1)
                                  ]

-- | Test if the given list of ordered coordinates froms a run
testChosenGroup                :: [Coord] -> Bool
testChosenGroup xs              = any (check1 xs) runDirections
  where
  check1 (x:y:z) step           = step x == y && check1 (y:z) step
  check1 _ _                    = True

-- | Update game state as the set of chosen pieces changes when picking
-- a run to remove from the board. If the player has selected a run,
-- remove it from the board and advance the game.
removeFiveLogic                :: Phase -> GameState -> Set Coord -> GameState
removeFiveLogic phase s chosen
  | Set.size chosen == goalRunLength && testChosenGroup (Set.toList chosen)
                                = s { mode      = RemoveRing phase
                                    , board     = deleteMany chosen $ board s
                                    }
  | otherwise                   = s { mode      = RemoveFive phase chosen }

-- | Delete many elements from a Map.
deleteMany                     :: Set Coord -> Map Coord Piece -> Map Coord Piece
deleteMany xs m                 = foldl' (\acc i -> Map.delete i acc) m xs

-- | Flip many pieces over given a collection of eligible coordinates.
flipThrough                    :: [Coord] -> Map Coord Piece -> Map Coord Piece
flipThrough xs b                = foldl' (\acc i -> flipAt i acc) b xs
  where
  flipAt                        = Map.update (Just . flipPiece)
  flipPiece p                   = p { piecePlayer = toggleTurn $ piecePlayer p }

-- | Check that it is legal to move a ring between two coordinates.
legalMove                      :: Coord -> Coord -> Map Coord Piece -> Bool
legalMove c1 c2 b               = not (null xs) -- check for non-straight move
                               && all (\i -> checkKind i == Just Solid)
                                      (dropWhile (available b) (tail (init xs)))
  where
  xs                            = movesThrough c1 c2
  checkKind c                   = fmap pieceKind $ Map.lookup c b

-- | Compute the coordinates between two points (inclusive) if they are on a line.
movesThrough                   :: Coord -> Coord -> [Coord]
movesThrough (C x1 y1) (C x2 y2)
  | x1 == x2                    = [C x1 y | y <- enum y1 y2]
  | y1 == y2                    = [C x y1 | x <- enum x1 x2]
  | (x1-x2) == (y2-y1)          = [C x y | (x,y) <- zip (enum x1 x2) (enum y1 y2)]
  | otherwise                   = []

-- | List of numbers between two bounds (inclusive) supporting decrementing
-- sequences.
enum a b | a <= b               = [a,a+1..b]
         | otherwise            = [a,a-1..b]

-- | Returns the other player
toggleTurn                     :: Player -> Player
toggleTurn Black                = White
toggleTurn White                = Black

-- | Update the game state given a number of rings remaining to place during
-- the startup phase of the game.
endSetupTurn                   :: Int -> GameState -> GameState
endSetupTurn n s
  | turn s == startingPlayer    = s { mode = mode' }
  | otherwise                   = s
  where
  mode' | n == 1                = PickRing
        | otherwise             = Setup (n-1)

available                      :: Map Coord Piece -> Coord -> Bool
available b c                   = Map.notMember c b
