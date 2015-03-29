{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Font (
       AddedPtr,
       ObjectPtr,
       RawSystemPtr,
       SystemPtr,
       addFontExn,
       draw,
       createFontExn,
       destroyAdded,
       destroyFont,
       withAdded,
       withFont
)

#include <sgec/font/object.h>
#include <sgec/font/system.h>
#include <sgec/font/draw/simple.h>

where

import Control.Exception( bracket )
import Control.Monad ( (>>=) )
import Data.Function ( ($) )
import Data.Int ( Int )
import Data.Maybe ( Maybe, fromMaybe )
import Data.String ( String )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.C ( CInt(..), CUInt(..), CWString )
import Foreign.C.String ( CString, withCString, withCWString )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( Ptr )
import System.IO ( IO )
import SGE.Image ( RGBA, convertRGBA )
import qualified SGE.Renderer ( ContextPtr, DevicePtr, RawContextPtr, RawDevicePtr )
import SGE.Types ( Pos, posX, posY )
import SGE.Utils ( failMaybe, failResultIO, maybeString, toCInt )

data SystemStruct
type RawSystemPtr = Ptr SystemStruct
type SystemPtr = ForeignPtr SystemStruct

data ObjectStruct
type RawObjectPtr = Ptr ObjectStruct
type ObjectPtr = ForeignPtr ObjectStruct

data AddedStruct
type RawAddedPtr = Ptr AddedStruct
type AddedPtr = ForeignPtr AddedStruct

foreign import ccall unsafe "sgec_font_system_create_font" sgeCreateFont :: RawSystemPtr -> CString -> CInt -> IO RawObjectPtr

createFont :: SystemPtr -> Maybe String -> Maybe Int -> IO (Maybe ObjectPtr)
createFont system family size =
           withForeignPtr system $ \psystem ->
           maybeString family $ \pfamily -> sgeCreateFont psystem pfamily (toCInt (fromMaybe (-1) size))
           >>= maybePeek newForeignPtr_

createFontExn :: SystemPtr -> Maybe String -> Maybe Int -> IO ObjectPtr
createFontExn system family size =
              failMaybe "createFont" (createFont system family size)

foreign import ccall unsafe "sgec_font_object_destroy" sgeDestroyFont :: RawObjectPtr -> IO ()

destroyFont :: ObjectPtr -> IO ()
destroyFont font =
            withForeignPtr font sgeDestroyFont

withFont :: SystemPtr -> Maybe String -> Maybe Int -> (ObjectPtr -> IO a) -> IO a
withFont system family size function =
         bracket (createFontExn system family size) destroyFont function

foreign import ccall unsafe "sgec_font_system_add_font" sgeAddFont :: RawSystemPtr -> CString -> IO RawAddedPtr

addFont :: SystemPtr -> String -> IO (Maybe AddedPtr)
addFont system path =
        withForeignPtr system $ \psystem ->
        withCString path $ \ppath ->
        sgeAddFont psystem ppath
        >>= maybePeek newForeignPtr_

addFontExn :: SystemPtr -> String -> IO AddedPtr
addFontExn system path =
           failMaybe "addFont" (addFont system path)

foreign import ccall unsafe "sgec_font_added_destroy" sgeDestroyAdded :: RawAddedPtr -> IO ()

destroyAdded :: AddedPtr -> IO ()
destroyAdded added =
             withForeignPtr added sgeDestroyAdded

withAdded :: SystemPtr -> String -> IO a -> IO a
withAdded system path function =
          bracket (addFontExn system path) destroyAdded (\_ -> function)

foreign import ccall unsafe "sgec_font_draw_simple" sgeDrawFont :: SGE.Renderer.RawDevicePtr -> SGE.Renderer.RawContextPtr -> RawObjectPtr -> CWString -> CInt -> CInt -> CUInt -> IO (CInt)

draw :: SGE.Renderer.DevicePtr -> SGE.Renderer.ContextPtr -> ObjectPtr -> String -> Pos -> RGBA -> IO ()
draw renderer context font text pos color =
     withForeignPtr renderer $ \prenderer ->
     withForeignPtr context $ \pcontext ->
     withForeignPtr font $ \pfont ->
     withCWString text $ \pstring ->
     failResultIO "draw text" $ sgeDrawFont prenderer pcontext pfont pstring (toCInt $ posX pos) (toCInt $ posY pos) (convertRGBA color)
