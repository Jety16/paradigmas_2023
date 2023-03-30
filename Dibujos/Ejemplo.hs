module Dibujos.Ejemplo (
    interpBas,
    ejemploConf
) where
    
import Graphics.Gloss (white, line, polygon, pictures)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, rotar90, espejar)
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = figura ()
--como es output y de donde vine a b c
interpBas :: Output Basica
interpBas () a b c = pictures [triangulo a b c, triangulo2 a b c]
  where
      triangulo a b c = polygon $ map (a V.+) [zero, b V.+ c, b , zero]
      triangulo2 a b c = polygon $ map (a V.+) [zero, half b V.+ c, b,zero]
        -- cara abc calls the polygon with the  vector list from triangulo a b c 
        -- where a = (a V.+ half c) and b = (half b) and c = (half c)
        -- polygon is a data struc of type Picture
        -- c= alto h
        -- b = ancho w
      -- cara a b c = polygon $ triangulo (a) (half b) (half c)
      --                     triangulo [half c, half b + half c , c, half c]
                    --       triangulo half b half c half c
      --                     triangulo [half b, half c + half b , c  ]
ejemploConf :: Conf
ejemploConf = Conf {
    name = "Ejemplo",
    pic = interp interpBas ejemplo
}

