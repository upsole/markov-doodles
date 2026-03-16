module Image (writeToFile) where
import Codec.Picture
import qualified Data.List as List
import Data.Maybe (listToMaybe, fromJust)

data Vec4 = Vec4 Int Int Int Int

{- 
 - Generates a grid of w * h cells of pxSide pixels per cell
-}
genPxMap :: Int -> Int -> Int -> [ Vec4 ]
genPxMap pxSide w h = [ Vec4 (x * pxSide) ((x+1) * pxSide) (y * pxSide) ((y + 1) * pxSide)
                        | y <- [ 0 ..  (h - 1)], x <- [ 0 .. (w - 1)] 
                                          ]
{-
 - Query a point in our map of pixel ranges : color code
-}
findEntryFromPx :: [(Vec4, a)] -> Int -> Int -> Maybe a
findEntryFromPx lst x' y' = snd <$> element
    where
        predicate (Vec4 x0 x1 y0 y1, _) = inX && inY
            where inX = x' >= x0 && x' < x1
                  inY = y' >= y0 && y' < y1
        element = List.find predicate lst

imgFromChars :: [String] -> Image PixelRGBA8
imgFromChars rows = img
    where
        pxSide = 50
        widthChars = maybe 0 length (listToMaybe rows)
        heightChars = length rows
        width  =  widthChars * pxSide
        height = pxSide * heightChars
        img    = generateImage draw width height
            where
                {- 
                 -  We generate a map of pixel ranges : color code
                 -  for the draw function to query from
                 -}
                draw :: (Int -> Int -> PixelRGBA8)
                pxMap = genPxMap pxSide widthChars heightChars
                pxCodeMap = zip pxMap (concatMap id rows)
                draw x y = px
                    where 
                        c = fromJust $ findEntryFromPx pxCodeMap x y
                        px = case c of
                            '0' -> PixelRGBA8 0 0 0 0xff
                            _  -> PixelRGBA8 0xff 0xff 0xff 0xff
writeToFile :: String -> [String] -> IO ()
writeToFile filepath input = writePng filepath img
    where img = imgFromChars input
