module Dibujos.Ejemplo
  ( interpBas,
    ejemploConf,
  )
where

import Dibujo (Dibujo, espejar, figura, r180, r270, rotar90)
import FloatingPic (Output, half, zero)
import Graphics.Gloss (line, pictures, polygon, white)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Interp (Conf (..), interp)

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = rotar90 (rotar90 (figura ()))

interpBas :: Output Basica
interpBas () a b c = pictures [cara a b c]
  where
    triangulo a b c = map (a V.+) [zero, c, b, zero]

    -- cara abc calls the polygon with the  vector list from triangulo a b c
    -- where a = (a V.+ half c) and b = (half b) and c = (half c)
    -- polygon is a data struc of type Picture
    cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)

ejemploConf :: Conf
ejemploConf =
  Conf
    { name = "Ejemplo",
      pic = interp interpBas ejemplo
    }
