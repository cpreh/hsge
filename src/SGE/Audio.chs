module SGE.Audio (
       BufferPtr,
       FilePtr,
       LoaderPtr,
       PlayerPtr,
       PlayStatus(..),
       RawLoaderPtr,
       RawPlayerPtr,
       Repeat(..),
       SoundPtr,
       createBuffer,
       createBufferExn,
       createSound,
       createSoundExn,
       destroyBuffer,
       destroyFile,
       destroySound,
       loadFile,
       loadFileExn,
       play,
       status,
       withFile
)

#include <sgec/audio/buffer.h>
#include <sgec/audio/file.h>
#include <sgec/audio/loader.h>
#include <sgec/audio/sound/base.h>
#include <sgec/audio/sound/play_status.h>
#include <sgec/audio/sound/repeat.h>

where

import Control.Exception( bracket )
import Control.Monad ( (>>=), return )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.Maybe ( Maybe )
import Data.String ( String )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.C ( CInt(..), CString, withCString )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( Ptr )
import Prelude ( Enum (fromEnum, toEnum)  )
import System.IO ( IO )
import Text.Show ( Show )

import SGE.Utils ( failMaybe, failResultIO, fromCInt, failWithMessage, toCInt )

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

{#enum sgec_audio_sound_play_status as SgePlayStatus {underscoreToCase} with prefix = "sgec_audio_sound_play_status" add prefix = "SgePlayStatus" deriving (Eq, Show)#}

foreign import ccall unsafe "sgec_audio_loader_load" sgeLoadFile :: RawLoaderPtr -> CString -> IO RawFilePtr

data PlayStatus =
     PlayStatusPlaying
     | PlayStatusStopped
     | PlayStatusPaused
     deriving(Eq,Show)

loadFile :: LoaderPtr -> String -> IO (Maybe FilePtr)
loadFile loader fileName =
         withForeignPtr loader $ \ploader ->
         withCString fileName $ \pFileName ->
         sgeLoadFile ploader pFileName
         >>= maybePeek newForeignPtr_

loadFileExn :: LoaderPtr -> String -> IO FilePtr
loadFileExn loader fileName =
            failMaybe "load audio file" (loadFile loader fileName)

foreign import ccall unsafe "sgec_audio_file_destroy" sgeDestroyFile :: RawFilePtr -> IO ()

destroyFile :: FilePtr -> IO ()
destroyFile file =
            withForeignPtr file sgeDestroyFile

withFile :: LoaderPtr -> String -> (FilePtr -> IO a) -> IO a
withFile loader fileName function =
         bracket (loadFileExn loader fileName) destroyFile function

foreign import ccall unsafe "sgec_audio_player_create_buffer" sgeCreateBuffer :: RawPlayerPtr -> RawFilePtr -> IO RawBufferPtr

createBuffer :: PlayerPtr -> FilePtr -> IO (Maybe BufferPtr)
createBuffer player file =
             withForeignPtr player $ \pplayer ->
             withForeignPtr file $ \pfile ->
             sgeCreateBuffer pplayer pfile
             >>= maybePeek newForeignPtr_

createBufferExn :: PlayerPtr -> FilePtr -> IO BufferPtr
createBufferExn player file =
                failMaybe "create audio buffer" (createBuffer player file)

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

createSoundExn :: BufferPtr -> IO SoundPtr
createSoundExn buffer =
               failMaybe "create sound" (createSound buffer)

foreign import ccall unsafe "sgec_audio_sound_base_destroy" sgeDestroySound :: RawSoundPtr -> IO ()

destroySound :: SoundPtr -> IO ()
destroySound sound =
             withForeignPtr sound sgeDestroySound

foreign import ccall unsafe "sgec_audio_sound_base_play" sgePlaySound :: RawSoundPtr -> CInt -> IO CInt

play :: SoundPtr -> Repeat -> IO ()
play sound repeat =
     withForeignPtr sound $ \psound ->
     failResultIO "play sound" $ sgePlaySound psound (toCInt (fromEnum repeat))

foreign import ccall unsafe "sgec_audio_sound_base_status" sgeSoundStatus :: RawSoundPtr -> IO CInt

status :: SoundPtr -> IO (PlayStatus)
status sound =
       withForeignPtr sound $ \psound -> do
       res <- sgeSoundStatus psound
       case toEnum (fromCInt res) of
            SgeplaystatusPlaying -> return PlayStatusPlaying
            SgeplaystatusPaused -> return PlayStatusPaused
            SgeplaystatusStopped -> return PlayStatusStopped
            SgeplaystatusError -> failWithMessage "playStatus"
