{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Internal

data Cellular = Cellular { width :: Int, ruleNum :: Rule } deriving (Show,Data,Typeable)

myArgs = Cellular { width = def, ruleNum = def }

main = do args <- cmdArgs myArgs
          run (ruleNum args) (singleLiveCell (width args))



singleLiveCell :: Int -> Zipper Int
singleLiveCell width = Z (replicate width 0) 1 (replicate width 0)

toChar :: Int -> Char
toChar 0 = ' '
toChar 1 = '#'

run :: Rule -> Zipper Int -> IO ()
run rule initial = mapM_ (putStrLn . map toChar) $ map toList $ iterate (automata rule) initial