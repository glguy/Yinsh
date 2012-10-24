module DrawToken (drawToken,drawRing) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.Monoid ((<>))

drawToken :: Float -> Float -> Color -> Color -> Picture -> Float -> Picture
drawToken radius thickness c1 c2 surface s = translate 0 (thickness/2) $ rotate 90 $ pictures xs'
  where
  w = thickness * cos s

  xs' | s > pi    = xs
      | otherwise = reverse xs

  center = case compare 0 s of
    LT -> [color c1 $ scale (sin s) 1 $ circleSolid radius]
    GT -> [color c2 $ scale (sin s) 1 $ circleSolid radius]
    EQ -> []

  xs = [ -- back detail
         translate (-w) 0 $ scale (sin s) 1 $ pictures
           [ color (light c1) $ circle (3/4*radius)
                             <> circle radius
           , surface
           ]
       | s < pi] ++
       [ color c1 $ translate (-w)   0 $ scale (sin s) 1 $ circleSolid radius
       , color c1 $ translate (-w/2) 0 $ scale (cos s) 1 $ rectangleSolid thickness (2*radius)
       ] ++
       center ++
       [ color c2 $ translate (w/2) 0 $ scale (cos s) 1 $ rectangleSolid thickness (2*radius)
       , color c2 $ translate w     0 $ scale (sin s) 1 $ circleSolid radius
       ] ++
       [ translate w 0 $ scale (sin s) 1 $ pictures
          [ color (light c2) $ circle (3/4*radius)
                            <> circle radius
          , surface
          ]
       | s > pi ]

drawRing cfront ringRadius ringWidth =
  pictures
   [ color cfront $ pictures
       [ thickCircle ringRadius ringWidth
       , translate ringRadius    (ringWidth/2) $ rectangleSolid ringWidth ringWidth
       , translate (-ringRadius) (ringWidth/2) $ rectangleSolid ringWidth ringWidth
       ]
   ,translate 0 ringWidth $
        color cfront (thickCircle ringRadius ringWidth)
      <> color (light cfront) (circle (ringRadius - ringWidth / 2))
      <> color (light cfront) (circle (ringRadius + ringWidth / 2))
   ]
