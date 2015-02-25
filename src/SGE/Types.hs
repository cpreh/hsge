{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Types (
	Result(..),
	Dim(..),
	Pos(..),
	posX,
	posY,
	dimW,
	dimH
)

where

import Data.Eq ( Eq )
import Data.Int ( Int )
import Prelude ( Enum (fromEnum, toEnum), error )

data Result =
	ResultOk
	| ResultError
	deriving(Eq)

-- TODO: Use c2hs
instance Enum Result where
	fromEnum ResultOk = 0
	fromEnum ResultError = 1

	toEnum 0 = ResultOk
	toEnum 1 = ResultError
	toEnum _ = error "Invalid result"

data Dim = Dim (Int, Int)
	deriving(Eq)

data Pos = Pos (Int, Int)
	deriving(Eq)

posX :: Pos -> Int
posX (Pos (x, _)) = x

posY :: Pos -> Int
posY (Pos (_, y)) = y

dimW :: Dim -> Int
dimW (Dim (w, _)) = w

dimH :: Dim -> Int
dimH (Dim (_, h)) = h
