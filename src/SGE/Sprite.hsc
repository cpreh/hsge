{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Sprite (
	Object(..),
	draw
)

#include <sgec/sprite/object.h>

where

import Control.Monad ( return )

import Data.Eq ( Eq )

import Data.Function ( ($) )

import Data.Int ( Int )

import Data.List ( map )

import Foreign ( Storable(..) )

import Foreign.C ( CInt, CSize(..) )

import Foreign.ForeignPtr ( withForeignPtr )

import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

import Foreign.Marshal.Array ( withArrayLen )

import Foreign.Ptr ( Ptr )

import SGE.Renderer ( ContextPtr, DevicePtr, RawContextPtr, RawDevicePtr )

import SGE.Texture ( PartPtr, RawPartPtr )

import SGE.Utils ( toCInt, toCSize )

import System.IO ( IO )

data RawObject = RawObject {
	x :: CInt,
	y :: CInt,
	w :: CInt,
	h :: CInt,
	tex :: RawPartPtr
} deriving(Eq)

data Object = Object {
	pos_x :: Int,
	pos_y :: Int,
	width :: Int,
	height :: Int,
	texture :: PartPtr
} deriving(Eq)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable RawObject where
	sizeOf _ = (#size struct sgec_sprite_object)
	alignment _ = (#alignment struct sgec_sprite_object)
	peek ptr = do
		x <- (#peek struct sgec_sprite_object, pos_x) ptr
		y <- (#peek struct sgec_sprite_object, pos_y) ptr
		w <- (#peek struct sgec_sprite_object, width) ptr
		h <- (#peek struct sgec_sprite_object, height) ptr
		tex <- (#peek struct sgec_sprite_object, texture) ptr
		return $ RawObject { x = x, y = y, w = w, h = h, tex = tex }
	poke ptr (RawObject x y w h tex) = do
		(#poke struct sgec_sprite_object, texture) ptr tex
		(#poke struct sgec_sprite_object, pos_x) ptr x
		(#poke struct sgec_sprite_object, pos_y) ptr y
		(#poke struct sgec_sprite_object, width) ptr w
		(#poke struct sgec_sprite_object, height) ptr h

foreign import ccall unsafe "sgec_sprite_draw" sgeSpriteDraw :: RawDevicePtr -> RawContextPtr -> Ptr RawObject -> CSize -> IO ()

draw :: DevicePtr -> ContextPtr -> [Object] -> IO ()
draw device context sprites =
	let toRawObject obj =
		RawObject {
			x = toCInt $ pos_x obj,
			y = toCInt $ pos_y obj,
			w = toCInt $ width obj,
			h = toCInt $ height obj,
			tex = unsafeForeignPtrToPtr $ texture obj
		}
	in
	withArrayLen (map toRawObject sprites) $
	\length -> \array -> withForeignPtr device $
	\dp -> withForeignPtr context $
	\cp -> sgeSpriteDraw dp cp array $ toCSize length
