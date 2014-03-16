module SGE.Sprite (
	Object,
	draw
)

#include <sgec/sprite/object.h>

where

import Foreign ( Storable(..) )

import Foreign.C ( CInt(..) )

import Foreign.ForeignPtr ( withForeignPtr )

import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

import Foreign.Marshal.Array ( withArray )

import Foreign.Ptr ( Ptr )

import SGE.Renderer ( ContextPtr, DevicePtr, PlanarTexturePtr, RawContextPtr, RawDevicePtr, RawPlanarTexturePtr )

import SGE.Utils ( toCInt )

data RawObject = RawObject {
	x :: CInt,
	y :: CInt,
	w :: CInt,
	h :: CInt,
	tex :: RawPlanarTexturePtr
} deriving(Eq, Show)

data Object = Object {
	pos_x :: Int,
	pos_y :: Int,
	width :: Int,
	height :: Int,
	texture :: PlanarTexturePtr
} deriving(Eq, Show)

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

foreign import ccall unsafe "sgec_sprite_draw" sgeSpriteDraw :: RawDevicePtr -> RawContextPtr -> Ptr RawObject ->  IO ()

draw :: DevicePtr -> ContextPtr -> [Object] -> IO ()
draw device context sprites =
	let toRawObject obj =
		RawObject {
			x = toCInt $ pos_x obj,
			y = toCInt $ pos_y obj,
			w = toCInt $ width obj,
			h = toCInt $ height obj,
			-- TODO: We probably have to touch all of these pointers after the C function has been called
			tex = unsafeForeignPtrToPtr $ texture obj
		}
	in
	withArray (map toRawObject sprites) $
	\array -> withForeignPtr device $
	\dp -> withForeignPtr context $
	\cp -> sgeSpriteDraw dp cp array
