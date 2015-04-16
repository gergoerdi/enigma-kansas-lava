module Enigma.Keyboard (eventPS2, keypress) where

import Language.KansasLava
import Data.Char
import qualified Data.Map as Map
import Data.Sized.Unsigned as Unsigned

-- TODO: This doesn't work for multi-byte scancodes
eventPS2 :: (Clock clk)
         => Signal clk (Enabled U8) -> Signal clk (Enabled (Bool, U8))
eventPS2 scancode = runRTL $ do
    releasing <- newReg False
    lastKey <- newReg Nothing

    CASE [ match scancode $ \code -> do
                CASE [ IF (reg releasing) $ do
                            releasing := low
                     , IF (code .==. 0xF0) $ do
                            releasing := high
                     ]
                lastKey := enabledS $ pack (bitNot (reg releasing), code)
         , OTHERWISE $ do
                lastKey := disabledS
         ]

    return $ reg lastKey

fromScanCode :: U8 -> Maybe U8
fromScanCode c = Map.lookup c scancodes
  where
    scancodes = Map.fromList $ flip zip [fromIntegral $ ord 'A'..] $
                [ 0x1C, 0x32, 0x21, 0x23, 0x24, 0x2B, 0x34, 0x33, 0x43
                , 0x3B, 0x42, 0x4B, 0x3A, 0x31, 0x44, 0x4D, 0x15, 0x2D
                , 0x1B, 0x2C, 0x3C, 0x2A, 0x1D, 0x22, 0x35, 0x1A
                ]

keypress :: (Clock clk) => Signal clk (Enabled (Bool, U8)) -> Signal clk (Enabled U8)
keypress event = commentS "keypress" $ funMap (Just . (fromScanCode =<<)) scancode'
  where
    (pressed, scancode) = unpack $ enabledVal event
    scancode' = runRTL $ do
        held <- newReg False
        WHEN (isEnabled event) $ do
            held := pressed
        let newPress = pressed .&&. bitNot (reg held)
        return $ packEnabled (isEnabled event .&&. newPress) scancode
