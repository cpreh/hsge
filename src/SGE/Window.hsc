{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Window (
	SystemPtr,
	RawSystemPtr,
	poll,
	quit
)

#include <sgec/window/system.h>

where

import Control.Monad ( liftM, return )

import Data.Bool( Bool(False, True) )

import Data.Eq ( Eq )

import Data.Function ( ($) )

import Data.Int ( Int )

import Foreign ( ForeignPtr, withForeignPtr )

import Foreign.C ( CInt(..) )

import Foreign.Ptr ( Ptr )

import Prelude ( Enum (fromEnum, toEnum), error )

import SGE.Utils ( failResultIO, failWithMessage, fromCInt, toCInt )

import System.IO ( IO )

data PollResult =
	PollResultRunning
	| PollResultFinished
	| PollResultError
	deriving(Eq)

instance Enum PollResult where
	fromEnum PollResultRunning = 0
	fromEnum PollResultFinished = 1
	fromEnum PollResultError = 2

	toEnum 0 = PollResultRunning
	toEnum 1 = PollResultFinished
	toEnum 2 = PollResultError
	toEnum _ = error "invalid poll result"

data SystemStruct

type RawSystemPtr = Ptr SystemStruct

type SystemPtr = ForeignPtr SystemStruct

foreign import ccall safe "sgec_window_system_poll" sgeWindowPoll :: RawSystemPtr -> IO (CInt)

poll :: SystemPtr -> IO (Bool)
poll system =
	withForeignPtr system $ \ptr ->
	do
		result <- liftM toEnum $ liftM fromCInt $ sgeWindowPoll ptr
		case result of
			PollResultRunning -> return True
			PollResultFinished -> return False
			PollResultError -> failWithMessage "poll"


foreign import ccall unsafe "sgec_window_system_quit" sgeWindowQuit :: RawSystemPtr -> CInt -> IO (CInt)

quit :: SystemPtr -> Int -> IO ()
quit system code =
	withForeignPtr system $ \ptr ->
	failResultIO "window quit" $ sgeWindowQuit ptr (toCInt code)
