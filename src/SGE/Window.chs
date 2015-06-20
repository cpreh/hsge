module SGE.Window (
       SystemPtr,
       RawSystemPtr,
       poll,
       quit
)

#include <sgec/window/system.h>
#include <sgec/window/system_poll_result.h>

where

import Control.Monad ( liftM, return )
import Data.Bool( Bool(False, True) )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.Int ( Int )
import Foreign ( ForeignPtr, withForeignPtr )
import Foreign.C ( CInt(..) )
import Foreign.Ptr ( Ptr )
import Prelude ( Enum( toEnum ) )
import System.IO ( IO )
import Text.Show ( Show )

import SGE.Utils ( failResultIO, failWithMessage, fromCInt, toCInt )

{#enum sgec_window_system_poll_result as PollResult {underscoreToCase} with prefix = "sgec_window_system_poll_result" add prefix = "PollResult" deriving (Eq, Show)#}

data SystemStruct

type RawSystemPtr = Ptr SystemStruct

type SystemPtr = ForeignPtr SystemStruct

foreign import ccall safe "sgec_window_system_poll" sgeWindowPoll :: RawSystemPtr -> IO (CInt)

poll :: SystemPtr -> IO (Bool)
poll system =
     withForeignPtr system $ \ptr -> do
                    result <- liftM toEnum $ liftM fromCInt $ sgeWindowPoll ptr
                    case result of
                         PollresultRunning -> return True
                         PollresultFinished -> return False
                         PollresultError -> failWithMessage "poll"


foreign import ccall unsafe "sgec_window_system_quit" sgeWindowQuit :: RawSystemPtr -> CInt -> IO (CInt)

quit :: SystemPtr -> Int -> IO ()
quit system code =
     withForeignPtr system $ \ptr ->
     failResultIO "window quit" $ sgeWindowQuit ptr (toCInt code)
