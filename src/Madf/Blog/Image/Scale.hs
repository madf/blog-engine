{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Madf.Blog.Image.Scale
    ( Method (..)
    , scale
    ) where

import Codec.Picture qualified as CP
import Codec.Picture.Extra qualified as CPE
import Data.Vector.Storable qualified as V
import Madf.Blog.Image.BoxFilter qualified as BF

data Method = Filtered | Direct
    deriving (Show, Eq)

scale :: Method -> Int -> Int -> CP.DynamicImage -> CP.DynamicImage
scale Filtered w h = scaleFiltered w h
scale Direct   w h = scaleDirect   w h

scaleFiltered :: Int -> Int -> CP.DynamicImage -> CP.DynamicImage
scaleFiltered w h dImg = case dImg of
    CP.ImageRGB8   img -> CP.ImageRGB8   $ filtered img
    CP.ImageRGB16  img -> CP.ImageRGB16  $ filtered img
    CP.ImageY8     img -> CP.ImageY8     $ filtered img
    CP.ImageY16    img -> CP.ImageY16    $ filtered img
    CP.ImageY32    img -> CP.ImageY32    $ filtered img
    CP.ImageYA8    img -> CP.ImageYA8    $ filtered img
    CP.ImageYA16   img -> CP.ImageYA16   $ filtered img
    CP.ImageYCbCr8 img -> CP.ImageYCbCr8 $ filtered img
    CP.ImageCMYK8  img -> CP.ImageCMYK8  $ filtered img
    CP.ImageCMYK16 img -> CP.ImageCMYK16 $ filtered img
    CP.ImageRGBA8  img -> CP.ImageRGBA8  $ filtered img
    CP.ImageRGBA16 img -> CP.ImageRGBA16 $ filtered img
    CP.ImageYF     _   -> CP.ImageRGB8   $ filtered (CP.convertRGB8 dImg)
    CP.ImageRGBF   _   -> CP.ImageRGB8   $ filtered (CP.convertRGB8 dImg)
    where
        filtered :: forall a. (CP.Pixel a, Bounded (CP.PixelBaseComponent a), Integral (CP.PixelBaseComponent a)) => CP.Image a -> CP.Image a
        filtered img =
            let cs    = CP.componentCount (undefined :: a)
                ow    = CP.imageWidth img
                oh    = CP.imageHeight img
                kSize = oh `div` h
            in CPE.scaleBilinear w h . CP.Image ow oh . BF.filterInt kSize ow oh cs . CP.imageData $ img

scaleDirect :: Int -> Int -> CP.DynamicImage -> CP.DynamicImage
scaleDirect w h dImg = case dImg of
    CP.ImageRGB8   img -> CP.ImageRGB8   $ direct img
    CP.ImageRGB16  img -> CP.ImageRGB16  $ direct img
    CP.ImageY8     img -> CP.ImageY8     $ direct img
    CP.ImageY16    img -> CP.ImageY16    $ direct img
    CP.ImageY32    img -> CP.ImageY32    $ direct img
    CP.ImageYA8    img -> CP.ImageYA8    $ direct img
    CP.ImageYA16   img -> CP.ImageYA16   $ direct img
    CP.ImageYCbCr8 img -> CP.ImageYCbCr8 $ direct img
    CP.ImageCMYK8  img -> CP.ImageCMYK8  $ direct img
    CP.ImageCMYK16 img -> CP.ImageCMYK16 $ direct img
    CP.ImageRGBA8  img -> CP.ImageRGBA8  $ direct img
    CP.ImageRGBA16 img -> CP.ImageRGBA16 $ direct img
    CP.ImageYF     _   -> CP.ImageRGB8   $ direct (CP.convertRGB8 dImg)
    CP.ImageRGBF   _   -> CP.ImageRGB8   $ direct (CP.convertRGB8 dImg)
    where
        direct :: forall a. (CP.Pixel a, Integral (CP.PixelBaseComponent a)) => CP.Image a -> CP.Image a
        direct img =
            let !sw     = CP.imageWidth img
                !sh     = CP.imageHeight img
                !cs     = CP.componentCount (undefined :: a)
                !kSize  = max 1 (sh `div` h)
                !half   = kSize `div` 2
                !intImg = BF.integralImage (fromIntegral :: CP.PixelBaseComponent a -> Int) sw sh cs (CP.imageData img)
                f i =
                    let pidx  = i `div` cs
                        c     = i `mod` cs
                        ox    = pidx `mod` w
                        oy    = pidx `div` w
                        cx    = ox * sw `div` w
                        cy    = oy * sh `div` h
                        x0    = max 0        (cx - half)
                        y0    = max 0        (cy - half)
                        x1    = min (sw - 1) (cx + half)
                        y1    = min (sh - 1) (cy + half)
                        !br   = intImg V.! (y1 * sw * cs + x1 * cs + c)
                        !tr   = if y0 > 0 then intImg V.! ((y0-1) * sw * cs + x1 * cs + c) else 0
                        !bl   = if x0 > 0 then intImg V.! (y1 * sw * cs + (x0-1) * cs + c) else 0
                        !tl   = if x0 > 0 && y0 > 0 then intImg V.! ((y0-1) * sw * cs + (x0-1) * cs + c) else 0
                        !s    = br - tr - bl + tl
                        !area = (x1 - x0 + 1) * (y1 - y0 + 1)
                    in fromIntegral (s `div` area)
            in CP.Image w h (V.generate (w * h * cs) f)
