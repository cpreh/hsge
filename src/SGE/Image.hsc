{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Image (
       RGBA(..),
       convertRGBA
)

#include <sgec/image/color/rgba.h>

where

import Data.Function ( ($) )

import Data.Word ( Word8 )

import Foreign.C ( CUChar(..), CUInt(..) )

import SGE.Utils ( toCUChar )

data RGBA = RGBA {
     red :: Word8
   , green :: Word8
   , blue :: Word8
   , alpha :: Word8
}

foreign import ccall unsafe "sgec_image_color_make_rgba" sgeMakeRGBA :: CUChar -> CUChar -> CUChar -> CUChar -> CUInt

convertRGBA :: RGBA -> CUInt
convertRGBA color = sgeMakeRGBA (toCUChar $ red color) (toCUChar $ green color) (toCUChar $ blue color) (toCUChar $ alpha color)
