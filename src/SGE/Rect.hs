module SGE.Rect (
       Rect(..),
       rectX,
       rectY,
       rectW,
       rectH
)

where

import SGE.Dim ( Dim(..), dimW, dimH )
import SGE.Pos ( Pos(..), posX, posY )

import Data.Eq ( Eq )
import Data.Int ( Int )

data Rect = Rect (Pos, Dim)
     deriving(Eq)

rectX :: Rect -> Int
rectX (Rect (pos, _)) = posX pos

rectY :: Rect -> Int
rectY (Rect (pos, _)) = posY pos

rectW :: Rect -> Int
rectW (Rect (_, dim)) = dimW dim

rectH :: Rect -> Int
rectH (Rect (_, dim)) = dimH dim
