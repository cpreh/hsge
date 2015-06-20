module SGE.Image (
       RGBA(RGBA, colorAlpha, colorBlue, colorGreen, colorRed),
       convertRGBA,
       makeRGBA
)

#include <sgec/image/color/rgba.h>

where

import Data.Function ( ($) )
import Data.Word ( Word8 )
import Foreign.C ( CUChar(..), CUInt(..) )

import SGE.Utils ( toCUChar )

data RGBA = RGBA {
     colorRed :: Word8
   , colorGreen :: Word8
   , colorBlue :: Word8
   , colorAlpha :: Word8
}

foreign import ccall unsafe "sgec_image_color_make_rgba" sgeMakeRGBA :: CUChar -> CUChar -> CUChar -> CUChar -> CUInt

convertRGBA :: RGBA -> CUInt
convertRGBA color = sgeMakeRGBA (toCUChar $ colorRed color) (toCUChar $ colorGreen color) (toCUChar $ colorBlue color) (toCUChar $ colorAlpha color)

makeRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> RGBA
makeRGBA r g b a = RGBA { colorRed = r, colorGreen = g, colorBlue = b, colorAlpha = a }
