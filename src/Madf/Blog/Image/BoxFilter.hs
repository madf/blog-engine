{-# LANGUAGE BangPatterns #-}

module Madf.Blog.Image.BoxFilter
    ( integralImage
    , filterInt
    , filterFloat
    ) where

import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM

integralImage :: (V.Storable a, VM.Storable b, Num b) => (a -> b) -> Int -> Int -> Int -> V.Vector a -> V.Vector b
integralImage toTarget w h cs src = V.create $ do
    intImg <- VM.new (w * h * cs)
    let f idx
            | idx >= w * h * cs = return ()
            | otherwise = do
                let pidx = idx `div` cs
                    y = pidx `div` w
                    x = pidx `mod` w

                left <- if x > 0 then VM.read intImg (idx - cs) else return 0
                top <- if y > 0 then VM.read intImg (idx - w * cs) else return 0
                leftTop <- if x == 0 || y == 0 then return 0 else VM.read intImg (idx - w * cs - cs)

                let val = toTarget (src V.! idx) + left + top - leftTop

                VM.write intImg idx val
                f (idx + 1)
    f 0
    return intImg
{-# INLINE integralImage #-}

filterInt :: (V.Storable a, Integral a) => Int -> Int -> Int -> Int -> V.Vector a -> V.Vector a
filterInt kSize w h cs img = V.generate (w * h * cs) f
    where
        intImg = integralImage fromIntegral w h cs img :: V.Vector Int
        half = kSize `div` 2
        f i =
            let pidx = i `div` cs
                c = i `mod` cs
                x = pidx `mod` w
                y = pidx `div` w

                x0 = max 0 (x - half)
                y0 = max 0 (y - half)
                x1 = min (w - 1) (x + half)
                y1 = min (h - 1) (y + half)

                !v = intImg V.! (y1 * w * cs + x1 * cs + c)
                !top = if y0 > 0 then intImg V.! ((y0 - 1) * w * cs + x1 * cs + c) else 0
                !left = if x0 > 0 then intImg V.! (y1 * w * cs + (x0 - 1) * cs + c) else 0
                !topLeft = if x0 > 0 && y0 > 0 then intImg V.! ((y0 - 1) * w * cs + (x0 - 1) * cs + c) else 0
                !rectSum = v - top - left + topLeft
                area = (x1 - x0 + 1) * (y1 - y0 + 1)
            in fromIntegral (rectSum `div` area)
{-# INLINE filterInt #-}

filterFloat :: Int -> Int -> Int -> Int -> V.Vector Float -> V.Vector Float
filterFloat kSize w h cs img = V.generate (w * h * cs) f
    where
        !intImg = integralImage id w h cs img
        !half = kSize `div` 2
        f !i =
            let pidx = i `div` cs
                c = i `mod` cs
                x = pidx `mod` w
                y = pidx `div` w

                x0 = max 0 (x - half)
                y0 = max 0 (y - half)
                x1 = min (w - 1) (x + half)
                y1 = min (h - 1) (y + half)

                !v = intImg V.! (y1 * w * cs + x1 * cs + c)
                !top = if y0 > 0 then intImg V.! ((y0 - 1) * w * cs + x1 * cs + c) else 0
                !left = if x0 > 0 then intImg V.! (y1 * w * cs + (x0 - 1) * cs + c) else 0
                !topLeft = if x0 > 0 && y0 > 0 then intImg V.! ((y0 - 1) * w * cs + (x0 - 1) * cs + c) else 0
                !rectSum = v - top - left + topLeft
                area = fromIntegral ((x1 - x0 + 1) * (y1 - y0 + 1))
            in rectSum / area
{-# INLINE filterFloat #-}
