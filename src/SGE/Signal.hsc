module SGE.Signal (
       CallbackState,
       ConnectionPtr,
       RawConnectionPtr,
       destroyCallback,
       makeCallbackState
)

#include <sgec/signal/connection.h>

where

import Control.Monad ( (>>), return )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.Ptr ( FunPtr, Ptr, freeHaskellFunPtr )
import System.IO ( IO )

data ConnectionStruct
type RawConnectionPtr = Ptr ConnectionStruct
type ConnectionPtr = ForeignPtr ConnectionStruct

foreign import ccall unsafe "sgec_signal_connection_destroy" sgeDestroyConnection :: RawConnectionPtr -> IO ()

destroy :: ConnectionPtr -> IO ()
destroy sig =
        withForeignPtr sig sgeDestroyConnection

type CallbackState a = (SGE.Signal.ConnectionPtr, FunPtr a)

makeCallbackState :: FunPtr a -> SGE.Signal.RawConnectionPtr -> IO (CallbackState a)
makeCallbackState fun ptr = do
                  fptr <- newForeignPtr_ ptr
                  return (fptr, fun)

destroyCallback :: CallbackState a -> IO ()
destroyCallback (ptr, fun) = destroy ptr >> freeHaskellFunPtr fun
