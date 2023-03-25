{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Interp (
    interp,
    Conf(..),
    interpConf,
    initial
) where

import Graphics.Gloss(Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display)
import Dibujo (Dibujo(..), foldDib, figura, rotar90, rot45, r180, apilar, juntar, encimar, r270)
import FloatingPic (FloatingPic, Output, grid, vacia)

import Graphics.Gloss.Data.Picture (Picture, rotate, blank)
import Graphics.Gloss.Data.Vector (Vector)

-- Convert a FloatingPic to another FloatingPic. 
-- Basically take the vectors and modify it
rotatePic :: FloatingPic -> FloatingPic
rotatePic pic v1 v2 v3 = pic (rotateVector v1) (rotateVector v2) (rotateVector v3)

espejarPic :: FloatingPic -> FloatingPic
espejarPic pic v1 v2 v3 = pic (espejarVector v1) (espejarVector v2) (espejarVector v3)

-- Verificar si estan ok las operaciones
-- Modify the vector oen by one
rotateVector :: Vector -> Vector
rotateVector (x, y) = ( y, x)

espejarVector :: Vector -> Vector
espejarVector (x, y) = (-x, y)


-- Interpretation of the Dibujos
interp :: Output a -> Output (Dibujo a)
interp f Vacia = vacia
interp f (Figura a) = f a 
interp f (Rotar90 a) = rotatePic (interp f a)
interp f (Espejar a) = espejarPic (interp f a)

-- Configuración de la interpretación
data Conf = Conf {
        name :: String,
        pic :: FloatingPic
    }

interpConf :: Conf -> Float -> Float -> Picture
interpConf (Conf _ p) x y = p (0, 0) (x,0) (0,y)

-- Dada una computación que construye una configuración (AKA. Dibujos/*.hs), mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación (que es una computacion?????) 
-- para poder leer
-- archivos, tomar argumentos, etc.

-- SOOO 
-- el pictures toma una lista de dibujos para hacer, donde 
-- color grey $ grid (ceiling $ size / 10) (0, 0) x 10
-- nos genera la grid basicamente

-- el withGrid es god porque lo que hace es dibujarnos las pictures sobre la grilla
-- yo le saque el p, del comienzo de la lista ya que este no esta hecho todavia xdddd
-- pero bueno loco, quedaria hacer eso y repetirlo y tamos.()

initial :: Conf -> Float -> IO ()
initial cfg size = do
    let n = name cfg
        win = InWindow n (ceiling size, ceiling size) (0, 0)
    display win white $ withGrid (interpConf cfg size size) size size
  where withGrid p x y = translate (-size/2) (-size/2) $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
        grey = makeColorI 120 120 120 120