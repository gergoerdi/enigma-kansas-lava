{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Enigma.Board

import Development.KansasLava.Shake
import Development.KansasLava.Shake.Xilinx
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.Console.GetOpt
import System.Exit

data Flag = XilinxRoot FilePath
          | PapilioModel String

mkXilinxConfig :: [Flag] -> IO XilinxConfig
mkXilinxConfig flags = do
    xilinxRoot <- case [path | XilinxRoot path <- flags] of
        [] -> return "/home/cactus/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64"
        [path] -> return path
        _ -> do
            putStrLn "Conflicting flags: --xilinx"
            exitFailure
    let xilinxPlatform = "XC3S500E-VQ100-5"
    return XilinxConfig{..}

main :: IO ()
main = do
    createDirectoryIfMissing True "ise"
    setCurrentDirectory "ise"
    shakeArgsWith shakeOptions flags $ \flags targets -> do
        xilinxConfig <- mkXilinxConfig flags

        (vhdl, ucf, xaws) <- synthesize modName
        return $ Just $ do
            want $ if null targets then [modName <.> "bit"] else targets

            lavaRules modName vhdl ucf
            xilinxRules xilinxConfig modName xaws
  where
    flags = [ Option [] ["xilinx"] (ReqArg (Right . XilinxRoot) "path") "Path to Xilinx toolchain"
            ]

    modName = "Enigma"
