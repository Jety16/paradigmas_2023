module Main (main) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Interp (Conf(name), initial)
import Dibujos.Ejemplo (ejemploConf)

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [ejemploConf]

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
    putStrLn $ "No hay un dibujo llamado " ++ n
initial' (win_name : cs) n = 
    if n == name win_name then
        -- create a windows with size 900x900 and name "c"
        initial win_name 900
    else
        initial' cs n

main :: IO ()
main = do
    args <- getArgs
    initial' configs $ head args
