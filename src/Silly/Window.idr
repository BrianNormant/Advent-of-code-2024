module Silly.Window

import IdrisGL
import IdrisGL.Color as Color
import System.Random
import Data.Fin

SCREENX : Int
SCREENX = 3840
SCREENY : Int
SCREENY = 2160
DIMX : Int
DIMX = 50
DIMY : Int
DIMY = 50

normalize : Double -> Int
normalize x = cast $ ( (sin(x) + 1) * 255 )

unsafeRndColor : Double -> Color
unsafeRndColor s = unsafePerformIO $ do
  s' <- randomIO {a = Double}
  _ <- srand (cast (1 + s + s'))
  r <- rndFin 256
  g <- rndFin 256
  b <- rndFin 256
  pure $ MkRGB (cast $ finToInteger r)
               (cast $ finToInteger g)
               (cast $ finToInteger b)

--- Function that generate a image with pixels of random colors
img : Double -> Picture
img s = bisequence ([0..DIMX], [0..DIMY])
   |> map (\(x,y) => Pixel (MkCoor x y) (unsafeRndColor (s * (cast x) * (cast y))) )
   |> Pictures


main : IO ()
main = let window = InWindow "Silly Window" (MkRect
                                 (div SCREENX 2 - div DIMX 2 )
                                 (div SCREENY 2 - div DIMY 2 )
                                 DIMX
                                 DIMY
                                 )
        in animate window Color.white (1.0 / 60.0) img

