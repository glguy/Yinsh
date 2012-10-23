{-# LANGUAGE PatternGuards #-}
module Main where

import Data.Foldable (any, fold, foldMap,foldl')
import Data.List (sortBy)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Graphics.Gloss.Data.Vector (mulSV, argV, magV)
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (any)
import qualified Data.Map as Map
import qualified Data.Set as Set

import DrawToken

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
playerToColor White = makeColor8 214 51 157 255 -- pinkish
cursorColor         = cyan
gridSize        = 74
ringRadius      = 30
ringWidth       = 7
solidRadius     = 20
timerLength     = 60 -- seconds
animationSpeed  = 200 -- pixels per second
flipTime        = 0.25
turnIndicatorCoord = C (-5) 7
windowSize      = (900,600)
windowLocation  = (10,10)
windowTitle     = "Yinsh"

--
-- Type definitions
--

-- | Location on the hex grid
data Coord = C Int Int
  deriving (Ord, Eq, Show)

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
  , transitions :: Map Coord (Float, Transition)
  , cursor      :: Maybe Coord
  , turn        :: Player
  , mode        :: GameMode
  , whiteScore  :: Int
  , blackScore  :: Int
  , timer       :: Float
  }

data Transition = Moving Coord | Preflip | Flipping deriving (Show)

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
  , transitions = Map.empty
  , cursor      = Nothing
  , turn        = startingPlayer
  , mode        = Setup startingRings
  , whiteScore  = 0
  , blackScore  = 0
  , timer       = 0
  }

main                            = play (InWindow windowTitle windowSize windowLocation)
                                       white
                                       30
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

-- | Advance the timers by the given elapsed time.
handleTick                     :: Float -> GameState -> GameState
handleTick elapsed s            = s { timer = elapsed + timer s
                                    , transitions = Map.mapMaybe (tickTransition elapsed)
                                                  $ transitions s }

tickTransition                 :: Float -> (Float, Transition) -> Maybe (Float, Transition)
tickTransition elapsed (x, y)
  | x > elapsed                 = Just (x-elapsed, y)
  | isPreflip y                 = Just (flipTime, Flipping)
  | otherwise                   = Nothing

isPreflip                      :: Transition -> Bool
isPreflip Preflip               = True
isPreflip _                     = False
--
-- Game rendering
--

-- | Render a picture for the whole game.
drawGameState                  :: GameState -> Picture
drawGameState s                 = scale (sqrt (3/2)) (1/sqrt 2) $ fold
                                [ drawTimer . timer
                                , drawTurn
                                , drawScore White . whiteScore
                                , drawScore Black . blackScore
                                , foldMap (drawCursor s) . cursor
                                , const hexGridPicture
                                , drawPieceInRing
                                , drawBoard
                                , drawPickFive . mode
                                ] s

-- | Draw a timer in the bottom of the screen to indicate how long a player's
-- turn has been going.
drawTimer                      :: Float -> Picture
drawTimer seconds               = translateC (C 5 (-7)) $ timers seconds timerColors
  where
  timerColors = [yellow,orange,red]

  timers _ []                   = blank
  timers n (c:cs)
    | n < timerLength           = color c $ arcSolid 0 (n / timerLength * 360) solidRadius
    | otherwise                 = color c (circleSolid solidRadius) <> timers (n-timerLength) cs

-- | Draw the rings indicating how many rings the player has removed.
drawScore                      :: Player -> Int -> Picture
drawScore who n                 = translateC pos
                                $ foldMap (\offset -> translate offset 0 pic)
                                $ take n [0, gridSize, 2*gridSize]
  where
  pic                           = drawPiece Nothing (Piece who Ring)
  pos = case who of
          White                -> C (-5) (-2)
          Black                -> C 3    3

-- | Draw a temporary solid piece inside the currently selected ring.
drawPieceInRing                :: GameState -> Picture
drawPieceInRing s =
  case mode s of
    PlaceRing c                -> drawPieceAt Nothing c (Piece (turn s) Solid)
    _                          -> blank

-- | Draw markers over all of the currently selected pieces when selecting
-- a run.
drawPickFive                   :: GameMode -> Picture
drawPickFive (RemoveFive _ xs)  = foldMap drawMarker xs
drawPickFive _                  = blank

-- | Draw the marker used to indicate that a piece is chosen for removal.
drawMarker                     :: Coord -> Picture
drawMarker coord                = translate 0 ringWidth
                                $ translateC coord
                                $ color yellow
                                $ rotate 45    bar
                               <> rotate (-45) bar
  where
  bar                           = rectangleSolid 10 25

-- | Draw all the pieces on the board.
drawBoard                      :: GameState -> Picture
drawBoard s                     = foldMap (uncurry drawWithTransition)
                                . sortBy pieceDrawOrder . Map.assocs . board $ s
  where drawWithTransition c    = drawPieceAt (Map.lookup c $ transitions s) c

pieceDrawOrder                 :: (Coord, Piece) -> (Coord, Piece) -> Ordering
pieceDrawOrder (c1,p1) (c2,p2)  = compareKinds (pieceKind p1) (pieceKind p2)
                               <> compareDepth c1 c2
  where
  compareKinds Solid Ring       = LT
  compareKinds Ring  Solid      = GT
  compareKinds _     _          = EQ

  compareDepth (C _ y1) (C _ y2) = compare y2 y1

-- | Draw a piece at the given hex coordinates.
drawPieceAt                    :: Maybe (Float, Transition) -> Coord -> Piece -> Picture
drawPieceAt mbTrans c           = translateC c . drawPiece mbTrans

-- | Draw a cursor image showing the player which coordinate their
-- cursor is hovering over.
drawCursor                     :: GameState -> Coord -> Picture
drawCursor g c
  | PlaceRing ring <- mode g
  , legalMove ring c (board g) = pictures
                                  [ color cursorColor
                                  $ thickLine 5 [ coordPoint ring, coordPoint c ]
                                  , drawPieceAt Nothing c (Piece (turn g) Ring)
                                  ]

drawCursor g c
  | Just p <- Map.lookup c $ board g
  , piecePlayer p == turn g
  , pieceKind p == Ring          = drawPieceAt Nothing c (Piece (turn g) Solid)

drawCursor g c                  = translateC c
                                $ color cursorColor
                                $ circleSolid solidRadius


-- | Draw a game piece at the origin.
drawPiece                      :: Maybe (Float, Transition) -> Piece -> Picture
drawPiece mbTrans p             = trans token
  where
  player                        = case mbTrans of
                                    Just (_,Preflip)       -> toggleTurn $ piecePlayer p
                                    _                      -> piecePlayer p

  cfront                        = playerToColor player
  cback                         = playerToColor $ toggleTurn player

  trans = case mbTrans of
    Just (t, Moving c)         -> let total = travelTime c
                                  in uncurry translate (mulSV (t/total) (coordPoint c))
    _                          -> id

  token = case (pieceKind p,mbTrans) of
    (Ring,_)                   -> drawRing cfront ringRadius ringWidth
    (Solid, Just (t,Flipping)) -> unaspect $ drawToken solidRadius (ringWidth/2) cback cfront blank (-5/6*pi + 2/3*pi*t/flipTime)
    (Solid, _                ) -> unaspect $ drawToken solidRadius (ringWidth/2) cfront cback blank (pi/6)

  unaspect                      = scale (recip (sqrt (3/2))) (sqrt 2)


travelTime                     :: Coord -> Float
travelTime coord                = magV (coordPoint coord) / animationSpeed

-- | Draw a picture indicating which mode the game is in.
drawTurn                       :: GameState -> Picture
drawTurn s                      = translateC turnIndicatorCoord pic
  where
  me  = turn s
  pic = case mode s of
          Setup n              -> drawPiece Nothing (Piece me Ring)
                               <> drawCounter n
          PickRing             -> drawPiece Nothing (Piece me Solid)
          PlaceRing {}         -> drawPiece Nothing (Piece me Ring)
          RemoveFive {}        -> drawPiece Nothing (Piece me Solid)
                               <> drawMarker (C 0 0)
          RemoveRing {}        -> color (greyN 0.5) (circleSolid ringRadius)
                               <> drawPiece Nothing (Piece me Ring)
                               <> drawMarker (C 0 0)
          GameOver             -> blank

-- | Render a small number to screen which fits inside a ring.
drawCounter                    :: Int -> Picture
drawCounter                     = translate (-7) (-10)
                                . scale 0.2 0.2
                                . text
                                . show

-- | Translate a picture to its hex coordinate location.
translateC                     :: Coord -> Picture -> Picture
translateC c                    = translateV (coordPoint c)

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
  xc                            = 2/(gridSize*sqrt 3) * (x / sqrt (3/2))
  yc                            = (y*sqrt 2)/gridSize-xc/2

-- drawGameState s                 = scale (sqrt (3/2)) (1/sqrt 2) $ fold

-- | Static image of the hexagonal board.
hexGridPicture                 :: Picture
hexGridPicture                  = color (makeColor8 0 0 0 50)
                                $ foldMap rotate [0, 60, 120]
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
    Setup n | clickedPiece == Nothing
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
                                            $ flipThrough affected
                                            $ board s
                                    , transitions = computeTransitions
                                                    ring c affected
                                    }
                        where affected = movesThrough ring c

    RemoveFive phase chosen | clickedPiece == Just (Piece me Solid)
                               -> removeFiveLogic phase s
                                $ toggleMembership c chosen

    RemoveRing phase | clickedPiece == Just (Piece me Ring)
                               -> endTurn phase
                                $ incScore
                                  s { board = Map.delete c $ board s }

    _                          -> s -- ignore all other selections

  where
  me                            = turn s
  clickedPiece                  = Map.lookup c $ board s

-- | Compute all the timings for the animations for this ring move.
computeTransitions             :: Coord -> Coord -> [Coord] -> Map Coord (Float, Transition)
computeTransitions c1 c2 xs     = Map.fromList
                                $ (c2,(total, Moving delta))
                                : [ (x,(total * fromIntegral (i :: Int)
                                              / fromIntegral distance, Preflip))
                                  | (i,x) <- zip [1..] (tail (init xs))
                                  ]
  where
  delta                         = subCoord c1 c2
  distance                      = length xs
  total                         = travelTime delta

-- | Subtract two coordinates as vectors.
subCoord                       :: Coord -> Coord -> Coord
subCoord (C x1 y1) (C x2 y2)    = C (x1-x2) (y1-y2)

-- | Remove element from a set if it is a member, add it otherwise
toggleMembership               :: Ord a => a -> Set a -> Set a
toggleMembership c chosen
  | Set.member c chosen         = Set.delete c chosen
  | otherwise                   = Set.insert c chosen

-- | Add a point to the current player's score
incScore                       :: GameState -> GameState
incScore s = case turn s of
               White           -> s { whiteScore = whiteScore s + 1 }
               Black           -> s { blackScore = blackScore s + 1 }

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
testChosenGroup                :: Set Coord -> Bool
testChosenGroup xs              = Set.size xs == goalRunLength
                               && any (check1 (Set.toAscList xs)) runDirections
  where
  check1 (x:y:z) step           = step x == y && check1 (y:z) step
  check1 _ _                    = True

-- | Update game state as the set of chosen pieces changes when picking
-- a run to remove from the board. If the player has selected a run,
-- remove it from the board and advance the game.
removeFiveLogic                :: Phase -> GameState -> Set Coord -> GameState
removeFiveLogic phase s chosen
  | testChosenGroup chosen      = s { mode      = RemoveRing phase
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
-- A legal move runs on a straight line and and spaces between the
-- start and end must zero or more empty spaces followed by zero or more
-- solid pieces.
legalMove                      :: Coord -> Coord -> Map Coord Piece -> Bool
legalMove c1 c2 b               = not (null xs) -- check for non-straight move
                               && Map.lookup c2 b == Nothing
                               && ( all       (== Just Solid)
                                  $ dropWhile (== Nothing)
                                  $ map checkKind $ tail $ init xs)
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
enum                           :: Int -> Int -> [Int]
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



--------------------------------------------------------------------------------

thickLine                      :: Float -> [Point] -> Picture
thickLine h xs                  = pictures $ zipWith (thickSegment h) xs
                                           $ drop 1 xs

thickSegment                   :: Float -> Point -> Point -> Picture
thickSegment h v1 v2            = translateV (addV v1 (mulSV (1/2)  dv))
                                $ rotate (negate $ radToDeg (argV dv))
                                $ rectangleSolid w h
  where
  dv                            = subV v2 v1
  w                             = magV dv

translateV                     :: Vector -> Picture -> Picture
translateV                      = uncurry translate

addV                           :: Vector -> Vector -> Vector
addV (x1,y1) (x2,y2)            = (x1+x2,y1+y2)

subV                           :: Vector -> Vector -> Vector
subV (x1,y1) (x2,y2)            = (x1-x2,y1-y2)
