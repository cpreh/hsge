module SGE.Audio (
       RawLoaderPtr,
       LoaderPtr,
       Repeat(..),
       createBuffer,
       createSound,
       destroyBuffer,
       destroyFile,
       destroySound,
       loadFile
)

#include <sgec/audio/buffer.h>
#include <sgec/audio/file.h>
#include <sgec/audio/loader.h>
#include <sgec/audio/sound/base.h>
#include <sgec/audio/sound/play_status.h>
#include <sgec/audio/sound/repeat.h>

where

import Control.Monad ( (>>=) )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.Maybe ( Maybe )
import Data.String ( String )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.C ( CInt(..), CString, withCString )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( Ptr )
import Prelude ( Enum (enumFromTo, enumFrom, fromEnum, pred, succ, toEnum),  error )
import System.IO ( IO )
import Text.Show ( Show, show )

data LoaderStruct
type RawLoaderPtr = Ptr LoaderStruct
type LoaderPtr = ForeignPtr LoaderStruct

data FileStruct
type RawFilePtr = Ptr FileStruct
type FilePtr = ForeignPtr FileStruct

data PlayerStruct
type RawPlayerPtr = Ptr PlayerStruct
type PlayerPtr = ForeignPtr PlayerStruct

data BufferStruct
type RawBufferPtr = Ptr BufferStruct
type BufferPtr = ForeignPtr BufferStruct

data SoundStruct
type RawSoundPtr = Ptr SoundStruct
type SoundPtr = ForeignPtr SoundStruct

{#enum sgec_audio_sound_repeat as Repeat {underscoreToCase} with prefix = "sgec_audio_sound_repeat" add prefix = "Repeat" deriving (Eq, Show)#}

foreign import ccall unsafe "sgec_audio_loader_load" sgeLoadFile :: RawLoaderPtr -> CString -> IO RawFilePtr

loadFile :: LoaderPtr -> String -> IO (Maybe FilePtr)
loadFile loader fileName =
         withForeignPtr loader $ \ploader ->
         withCString fileName $ \pFileName ->
         sgeLoadFile ploader pFileName
         >>= maybePeek newForeignPtr_

foreign import ccall unsafe "sgec_audio_file_destroy" sgeDestroyFile :: RawFilePtr -> IO ()

destroyFile :: FilePtr -> IO ()
destroyFile file =
            withForeignPtr file sgeDestroyFile

foreign import ccall unsafe "sgec_audio_player_create_buffer" sgeCreateBuffer :: RawPlayerPtr -> RawFilePtr -> IO RawBufferPtr

createBuffer :: PlayerPtr -> FilePtr -> IO (Maybe BufferPtr)
createBuffer player file =
             withForeignPtr player $ \pplayer ->
             withForeignPtr file $ \pfile ->
             sgeCreateBuffer pplayer pfile
             >>= maybePeek newForeignPtr_

foreign import ccall unsafe "sgec_audio_buffer_destroy" sgeDestroyBuffer :: RawBufferPtr -> IO ()

destroyBuffer :: BufferPtr -> IO ()
destroyBuffer buffer =
              withForeignPtr buffer sgeDestroyBuffer

foreign import ccall unsafe "sgec_audio_buffer_create_nonpositional" sgeCreateSound :: RawBufferPtr -> IO RawSoundPtr

createSound :: BufferPtr -> IO (Maybe SoundPtr)
createSound buffer =
            withForeignPtr buffer $ \pbuffer ->
            sgeCreateSound pbuffer
            >>= maybePeek newForeignPtr_

foreign import ccall unsafe "sgec_audio_sound_base_destroy" sgeDestroySound :: RawSoundPtr -> IO ()

destroySound :: SoundPtr -> IO ()
destroySound sound =
             withForeignPtr sound sgeDestroySound

foreign import ccall unsafe "sgec_audio_sound_base_play" sgePlaySound :: RawSoundPtr -> CInt -> IO ()

-- play :: SoundPtr -> Repeat -> IO ()
-- play sound repeat =
--     withForeignPtr sound $ \psound
--     sgePlaySound sound repeat
