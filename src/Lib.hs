module Lib
    ( someFunc
    ) where

import MinimonTypes (MiniType(..))
import Minimon (Minimon(..), roundz)
import Creatures (createGenericCreature)

mainHelp :: Minimon -> Minimon -> IO ()
mainHelp m1 m2 = do
  putStrLn $ "You are" ++ show m1
  putStrLn "Please select an attack (1-4)"
  input <- getLine
  let num = read input :: Int
      next = roundz num m1 m2
    in case next of
        Left msg ->
          if msg == "No more attacks" then do
              putStrLn "Vous ne pouvez plus utiliser cette attaque"
              mainHelp m1 m2
            else putStrLn $ "Victoire de " ++ show m1
        Right (a1, a2, _) -> mainHelp a2 a1


someFunc :: IO ()
someFunc = do
  mainHelp (createGenericCreature Fire) (createGenericCreature Plant)
