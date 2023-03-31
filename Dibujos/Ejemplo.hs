module Dibujos.Ejemplo
  ( interpBas,
    ejemploConf,
  )
where

import Dibujo (Dibujo, encimar, espejar, figura, r180, r270, rot45, rotar90)
import FloatingPic (Output, half, zero)
import Graphics.Gloss (line, pictures, polygon, white,text)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Interp (Conf (..), interp)

type Basica = () 

ejemplo :: Dibujo Basica
ejemplo = encimar (rotar90 (figura ())) (figura ())
-- como reescribo lo de arriba pero con $?


--como es output y de donde vine a b c
interpBas :: Output Basica
interpBas () a b c = pictures [triangulo a b c]
  where
      triangulo a b c = polygon $ map (a V.+) [zero, c, b, zero]
      triangulo1 a b c = polygon $ map (a V.+) [zero, b V.+ c, b , zero]
      triangulo2 a b c = polygon $ map (a V.+) [zero, half b V.+ c, b,zero]
      cuadrado a b c = line [a , a V.+ c, a V.+ b V.+ c, a V.+ b, a]
            --s = "MINITAS'NT"
      --texto s = text s
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
ejemploConf =
  Conf
    { name = "Ejemplo",
      pic = interp interpBas ejemplo
    }
