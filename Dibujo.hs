module Dibujo
  ( Dibujo (..),
    figura,
    rotar90,
    espejar,
    rot45,
    apilar,
    juntar,
    encimar,
    r180,
    r270,
    (.-.),
    (///),
    (^^^),
    cuarteto,
    encimar4,
    ciclar,
    foldDib,
    mapDib,
    vacia,
    figuras,
  )
where

{-
Gramática de las figuras:
<Fig> ::= Figura <Bas> | Rotar <Fig> | Espejar <Fig> | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> <Fig>
    | Juntar <Float> <Float> <Fig> <Fig>
    | Encimar <Fig> <Fig>
-}

-- Preguntar y comentarle al profe sobre el caso vacio.
-- Plantear una especie de "recursion" donde ese es el ultimo Vacío
data Dibujo base_dibujo
  = Vacia
  | Figura base_dibujo
  | Rotar90 (Dibujo base_dibujo)
  | Espejar (Dibujo base_dibujo)
  | Rot45 (Dibujo base_dibujo)
  | Apilar Float Float (Dibujo base_dibujo) (Dibujo base_dibujo)
  | Juntar Float Float (Dibujo base_dibujo) (Dibujo base_dibujo)
  | Encimar (Dibujo base_dibujo) (Dibujo base_dibujo)
  deriving (Eq, Show)

-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

vacia :: Dibujo base_dibujo
vacia = Vacia

espejar :: Dibujo base_dibujo -> Dibujo base_dibujo
espejar = Espejar

figura :: base_dibujo -> Dibujo base_dibujo
figura = Figura

-- rotar90
rotar90 :: Dibujo base_dibujo -> Dibujo base_dibujo
rotar90 a = Rotar90 a

-- rot45
rot45 :: Dibujo base_dibujo -> Dibujo base_dibujo
rot45 = Rot45

-- Apilar
apilar :: Float -> Float -> Dibujo base_dibujo -> Dibujo base_dibujo -> Dibujo base_dibujo
apilar float1 float2 a b = Apilar float1 float2 a b

-- Juntar
juntar :: Float -> Float -> Dibujo base_dibujo -> Dibujo base_dibujo -> Dibujo base_dibujo
juntar fl1 fl2 a b  = Juntar fl1 fl2 a b

-- Encimar
encimar :: Dibujo base_dibujo ->  Dibujo base_dibujo -> Dibujo base_dibujo
encimar a b = Encimar a b


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo base_dibujo -> Dibujo base_dibujo
r180 p_1 = (rotar90 . rotar90) p_1

r270 :: Dibujo base_dibujo -> Dibujo base_dibujo
r270 p_1 = (rotar90 . rotar90 . rotar90) p_1

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio. Apilar compartido
(.-.) :: Dibujo base_dibujo -> Dibujo base_dibujo -> Dibujo base_dibujo
(.-.) a b =  apilar 0.5 0.5 a b

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio. Encimar Compartido
(///) :: Dibujo base_dibujo -> Dibujo base_dibujo -> Dibujo base_dibujo
(///) a b = juntar 0.5 0.5 a b

-- Superpone una figura con otra.
(^^^) = undefined

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo base_dibujo -> Dibujo base_dibujo ->  Dibujo base_dibujo -> Dibujo base_dibujo -> Dibujo base_dibujo
cuarteto a b c d = (///) ((.-.) a b) ((.-.) c d)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 = undefined

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar = undefined

-- Estructura general para la semántica (a no asustarse). Ayuda:
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib = undefined

-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib = undefined

-- Junta todas las figuras básicas de un dibujo.
figuras = undefined
