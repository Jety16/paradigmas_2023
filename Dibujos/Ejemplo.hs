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
ejemplo = r270 (figura ())

--ejemplo = (rotar90 (rotar90 (rotar90 (figura ()))))

interpBas :: Output Basica
interpBas () a b c = pictures [cuadrado a b c, triangulo a b c]
  where
    triangulo a b c = polygon $ map (a V.+) [zero, b V.+ c, b, zero]
    cuadrado a b c = line [a, a V.+ c, a V.+ c V.+ b, zero, a]

-- cara abc calls the polygon with the  vector list from triangulo a b c
-- where a = (a V.+ half c) and b = (half b) and c = (half c)
-- polygon is a data struc of type Picture
--cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)

ejemploConf :: Conf
ejemploConf =
  Conf
    { name = "Ejemplo",
      pic = interp interpBas ejemplo
    }
