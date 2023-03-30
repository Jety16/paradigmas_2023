{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Interp
  ( interp,
    Conf (..),
    interpConf,
    initial,
  )
where

import Dibujo (Dibujo (..), apilar, encimar, figura, foldDib, juntar, r180, r270, rot45, rotar90)
import FloatingPic (FloatingPic, Output, grid, half, vacia, zero)
import Graphics.Gloss (Display (InWindow), Picture, color, display, makeColorI, pictures, translate, white)
import Graphics.Gloss.Data.Picture (Picture, blank, rotate)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Data.Vector (Vector)
import Language.Haskell.TH (Role)

-- Convert a FloatingPic to another FloatingPic.
-- Basically take the vectors and modify it
rotatePic :: FloatingPic -> FloatingPic
rotatePic pic v1 v2 v3 = pic (v1 V.+ v2) (v3) (zero V.- v2)

espejarPic :: FloatingPic -> FloatingPic
espejarPic pic v1 v2 v3 = pic (v1 V.+ v2) (zero V.- v2) (v3)

rot45Pic :: FloatingPic -> FloatingPic
rot45Pic pic v1 v2 v3 = pic (v1 V.+ half (v2 V.+ v3)) (half (v2 V.+ v3)) (half (v3 V.- v2))

-- IDEA DE MODIFICADOR DE VECTOR
--espejarVector :: Vector -> Vector
--espejarVector (x, y) = (-x, y)
--
-- rotar(espejar(figura))

-- Interpretation of the Dibujos
interp :: Output a -> Output (Dibujo a)
interp f Vacia = vacia
interp f (Figura a) = f a
interp f (Rotar90 a) = rotatePic (interp f a)
interp f (Espejar a) = espejarPic (interp f a)
interp f (Rot45 a) = rot45Pic (interp f a)

-- Configuración de la interpretación
data Conf = Conf
  { name :: String,
    pic :: FloatingPic
  }

interpConf :: Conf -> Float -> Float -> Picture
interpConf (Conf _ p) x y = p (0, 0) (x, 0) (0, y)

-- Dada una computación que construye una configuración (AKA. Dibujos/*.hs), mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación (que es una computacion?????)
-- para poder leer
-- archivos, tomar argumentos, etc.

-- SOOO
-- el pictures toma una lista de dibujos para hacer, donde  el
-- color grey $ grid (ceiling $ size / 10) (0, 0) x 10
-- nos genera la grid basicamente

initial :: Conf -> Float -> IO ()
initial cfg size = do
  let n = name cfg
      win = InWindow n (ceiling size, ceiling size) (0, 0)
  display win white $ withGrid (interpConf cfg size size) size size
  where
    withGrid p x y = translate (- size / 2) (- size / 2) $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 120 120 120 120