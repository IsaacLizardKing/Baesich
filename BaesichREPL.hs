{-# LANGUAGE GADTSyntax #-}

import           Baesich                  (baesichEval)
import           System.Console.Haskeline
import           Data.List                (isPrefixOf)
import           Data.Char                (isDigit)
import           GHC.Word                 (Word8)
import           Control.Monad.State

description :: String
description = "Prepare to have your Baes iched"

helpMsg :: String
helpMsg = unlines
  [ ":syntax      - get a nice guide on how to Baes your ich"
  , ":help        - print this message."
  , ":quit        - quit."
  ]

syntaxMsg :: String
syntaxMsg = unlines
    [ "" ]

eval :: String -> String
eval s = (show (baesichEval s))

baesichREPL :: IO ()
baesichREPL = putStrLn description >> runInputT settings loop
    where
        settings = defaultSettings { historyFile = Just ".baesich-hist" }

loop :: InputT IO ()
loop = do
    minput <- getInputLine "> "
    case minput of
        Nothing      -> return ()
        Just s | s `isPrefixOf` ":quit" -> return ()
            | s `isPrefixOf` ":help" -> (outputStrLn $ helpMsg) >> loop
            | s `isPrefixOf` ":syntax" -> (outputStrLn $ syntaxMsg) >> loop
        Just input   -> do
            outputStrLn $ eval input
            loop




main :: IO ()
main = baesichREPL