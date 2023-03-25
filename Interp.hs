{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Interp (
    interp,
    Conf(..),
    interpConf,
    initial
) where

import Graphics.Gloss(Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display)
import Dibujo (Dibujo(..), foldDib, figura, rotar, rot45, r180, apilar, juntar, encimar, r270)
import FloatingPic (FloatingPic, Output, grid, vacia)

import Graphics.Gloss.Data.Picture (Picture, rotate, blank)
import Graphics.Gloss.Data.Vector (Vector)

-- Interpretación de un dibujo
-- formulas sacadas del enunciado
-- Toma un Dibujos/*.hs 

-- type FloatingPic = Vector -> Vector -> Vector -> Picture
-- type Output a = a -> FloatingPic



-- rotateDibujo :: Output a -> Picture
-- rotateDibujo a = rotate 90

interp :: Output a -> Output (Dibujo a)
interp _ Vacia = vacia
interp f (Figura a) = f a
-- interp f (Rotar a) = rotateDibujo f a

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