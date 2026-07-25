module Madf.Blog.Image.Orientation
    ( normalize
    ) where

import Codec.Picture
import Graphics.HsExif

normalize :: ImageOrientation -> DynamicImage -> DynamicImage
normalize o dImg = let w = dynamicMap imageWidth dImg
                       h = dynamicMap imageHeight dImg
                       (w', h', xform) = getTransform w h o
                   in dynamicPixelMap (applyTransform w' h' xform) dImg

type Transform = (Int, Int) -> (Int, Int)

applyTransform :: Pixel a => Int -> Int -> Transform -> Image a -> Image a
applyTransform w h xform img = generateImage f w h
    where
        f x y = let (x', y') = xform (x, y)
                in pixelAt img x' y'

getTransform :: Int -> Int -> ImageOrientation -> (Int, Int, Transform)
getTransform w h o = let chainXForm = case o of
                                         Normal                          -> id
                                         Mirror                          -> mirrorH
                                         MirrorRotation HundredAndEighty -> mirrorV
                                         Rotation HundredAndEighty       -> mirrorV . mirrorH
                                         MirrorRotation MinusNinety      -> transpose
                                         Rotation MinusNinety            -> mirrorH . transpose
                                         MirrorRotation Ninety           -> mirrorV . mirrorH . transpose
                                         Rotation Ninety                 -> mirrorV . transpose
                     in chainXForm (w, h, id)
    where
        mirrorH :: (Int, Int, Transform) -> (Int, Int, Transform)
        mirrorH (w', h', xform) = (w', h', \(x, y) -> xform (w' - x - 1, y))
        mirrorV :: (Int, Int, Transform) -> (Int, Int, Transform)
        mirrorV (w', h', xform) = (w', h', \(x, y) -> xform (x, h' - y - 1))
        transpose ::  (Int, Int, Transform) -> (Int, Int, Transform)
        transpose (w', h', xform) = (h', w', \(x, y) -> xform (y, x))
