module Dibujos.Ejemplo (
    interpBas,
    ejemploConf
) where
    
import Graphics.Gloss (white, line, polygon, pictures)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura)
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

type Triangulo = ()

ejemplo :: Dibujo Triangulo
-- Dibujo constructor
ejemplo = figura ()

interpBas :: Output Triangulo
interpBas () a b c = pictures [line $ triangulo a b c, cara a b c]
  where
    -- Here the triangulo took 4 vertices, where first == last
        -- triangulo is a function defined like this
        -- triangulo(a, b, c){
        --  return [a+zero, a+c, a+b, a+zero]
        --}
      triangulo a b c = map (a V.+) [zero, c, b, zero]

        -- cara abc calls the polygon with the  vector list from triangulo a b c 
        -- where a = (a V.+ half c) and b = (half b) and c = (half c)
        -- polygon is a data struc of type Picture
      cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)


-- Note that Conf is defined in interp
-- call el interp (interpicture) with the output InterBas

ejemploConf :: Conf
ejemploConf = Conf {
    name = "Triangulo",
    pic = interp interpBas ejemplo
}

