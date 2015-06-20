{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Texture (
       PartPtr,
       RawPartPtr,
       destroyPart,
       dim,
       height,
       partRawExn,
       partRawRectExn,
       width,
       withPartRaw,
       withPartRawRect
)

#include <sgec/texture/part.h>
#include <sgec/texture/part_raw.h>

where

import Control.Exception( bracket )
import Control.Monad ( (>>=) )
import Data.Function ( ($) )
import Data.Int ( Int )
import Data.Maybe ( Maybe )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.C ( CSize(..) )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal.Utils ( maybePeek )
import System.IO ( IO )
import System.IO.Unsafe ( unsafeDupablePerformIO )

import SGE.Dim ( Dim(..) )
import SGE.Rect ( Rect, rectX, rectY, rectW, rectH )
import SGE.Renderer ( PlanarTexturePtr, RawPlanarTexturePtr )
import SGE.Utils ( failMaybe, fromCSize, toCSize )

data PartStruct
type RawPartPtr = Ptr PartStruct
type PartPtr = ForeignPtr PartStruct

foreign import ccall unsafe "sgec_texture_part_raw" sgeTexturePartRaw :: RawPlanarTexturePtr -> IO RawPartPtr

partRaw :: PlanarTexturePtr -> IO (Maybe PartPtr)
partRaw texture =
        withForeignPtr texture $ \ptex ->
        sgeTexturePartRaw ptex
        >>= maybePeek newForeignPtr_

partRawExn :: PlanarTexturePtr -> IO PartPtr
partRawExn texture =
           failMaybe "partRaw" (partRaw texture)

foreign import ccall unsafe "sgec_texture_part_raw_rect" sgeTexturePartRawRect :: RawPlanarTexturePtr -> CSize -> CSize -> CSize -> CSize -> IO RawPartPtr

partRawRect :: PlanarTexturePtr -> Rect -> IO (Maybe PartPtr)
partRawRect texture rect =
            withForeignPtr texture $ \ptex ->
            sgeTexturePartRawRect ptex (toCSize $ rectX rect) (toCSize $ rectY rect) (toCSize $ rectW rect) (toCSize $ rectH rect)
            >>= maybePeek newForeignPtr_

partRawRectExn :: PlanarTexturePtr -> Rect -> IO PartPtr
partRawRectExn texture rect =
               failMaybe "partRawRect" (partRawRect texture rect)

foreign import ccall unsafe "sgec_texture_part_destroy" sgeDestroyTexturePart :: RawPartPtr -> IO ()

destroyPart :: PartPtr -> IO ()
destroyPart texture =
            withForeignPtr texture sgeDestroyTexturePart

withPartRaw :: PlanarTexturePtr -> (PartPtr -> IO a) -> IO a
withPartRaw texture function =
            bracket (partRawExn texture) destroyPart function

withPartRawRect :: PlanarTexturePtr -> Rect -> (PartPtr -> IO a) -> IO a
withPartRawRect texture rect function =
                bracket (partRawRectExn texture rect) destroyPart function

foreign import ccall unsafe "sgec_texture_part_width" sgeTextureWidth :: RawPartPtr -> IO CSize

width :: PartPtr -> Int
width texture =
      fromCSize $ unsafeDupablePerformIO $ withForeignPtr texture sgeTextureWidth

foreign import ccall unsafe "sgec_texture_part_height" sgeTextureHeight :: RawPartPtr -> IO CSize

height :: PartPtr -> Int
height texture =
       fromCSize $ unsafeDupablePerformIO $ withForeignPtr texture sgeTextureHeight

dim :: PartPtr -> Dim
dim texture = Dim (width texture, height texture)
