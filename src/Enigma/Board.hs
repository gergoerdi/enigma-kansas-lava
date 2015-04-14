{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Enigma.Board where

import Enigma
import Enigma.Keyboard

import Language.KansasLava
import Language.KansasLava.VHDL
import Hardware.KansasLava.Boards.UCF (filterUCF)
import Language.Netlist.GenVHDL (genVHDL)
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.PS2

import Control.Applicative

import Data.Sized.Unsigned as Unsigned
import Data.Sized.Matrix as Matrix

data LCD clk = LCD{ lcdRs :: Signal clk U1
                  , lcdD :: Signal clk U4
                  , lcdEn :: Signal clk Bool
                  }

lcdOut :: LCD CLK -> Fabric ()
lcdOut LCD{..} = do
    outStdLogicVector "LCD_D" . unsignedMatrix $ lcdD
    outStdLogic "LCD_RS" lcdRs
    outStdLogic "LCD_EN" lcdEn

ps2In :: Fabric (PS2 CLK)
ps2In = PS2 <$> inStdLogic "PS2_CLK" <*> inStdLogic "PS2_DAT"

unsignedMatrix :: (Clock clk, Size n)
               => Signal clk (Unsigned n) -> Signal clk (Matrix n Bool)
unsignedMatrix = bitwise

fabric :: Fabric ()
fabric = do
    ps2 <- keypress . eventPS2 . decodePS2 . samplePS2 <$> ps2In
    let lcd = board ps2
    lcdOut lcd

circuit :: (Clock clk) => Signal clk (Enabled U8) -> Signal clk (Enabled U8)
circuit = mapEnabled toByte . enigma testEnigma . mapEnabled fromByte
  where
    fromByte x = unsigned $ x - 0x41
    toByte x = unsigned x + 0x41

board :: (Clock clk) => Signal clk (Enabled U8) -> LCD clk
board key = LCD{..}
  where
    output = circuit key

    -- mapEnabled unsigned key

    lcdPatch = init_LCD $$ phy_Inst_4bit_LCD
    (_lcdAck, lcdSig) = epilogueP (pureS ReturnHome) lcdPatch (mapEnabled writeChar output, ())
    (lcdRs, lcdD, lcdEn) = unpack lcdSig

epilogueP :: (Clock clk, Rep a)
          => Signal clk a
          -> Patch (Signal clk (Enabled a)) b (Signal clk Ack) ()
          -> Patch (Signal clk (Enabled a)) b (Signal clk Ack) ()
epilogueP epilogue patch ~(trigger, ()) = (ack, out)
  where
    (ack, out) = patch (sig', ())
    (_, sig) = fifo1 (trigger, epiAck)

    (epiAck, epiSig) = fifo1 (packEnabled (fromAck ack) epilogue, ack)
    sig' = mergeEnabled epiSig sig

mergeEnabled :: (Clock clk, Rep a)
             => Signal clk (Enabled a) -> Signal clk (Enabled a) -> Signal clk (Enabled a)
mergeEnabled s1 s2 = packEnabled (isEnabled s1 .||. isEnabled s2) $
                     mux (isEnabled s1) (enabledVal s2, enabledVal s1)

synthesize :: String -> IO (String, String)
synthesize modName = do
    kleg <- reifyFabric $ do
        theClk "CLK_32MHZ"
        -- theRst "RESET"
        fabric

    mod <- netlistCircuit modName kleg
    let vhdl = genVHDL mod ["work.lava.all", "work.all"]
        ucf = filterUCF Nothing kleg ucf0
    return (vhdl, ucf)
  where
    ucf0 = unlines
           [ "CONFIG PART=XC3S500E-VQ100-5;"
           , ""
           , "CONFIG PROHIBIT=P99;"
           , "CONFIG PROHIBIT=P43;"
           , "CONFIG PROHIBIT=P42;"
           , "CONFIG PROHIBIT=P39;"
           , "CONFIG PROHIBIT=P49;"
           , "CONFIG PROHIBIT=P48;"
           , "CONFIG PROHIBIT=P47;"
           , ""
           , "NET \"CLK_32MHZ\" LOC=P89 | IOSTANDARD = LVCMOS25 | PERIOD=31.25ns;"
           , ""
           , "NET \"LCD_D<0>\"  LOC=P91 | IOSTANDARD=LVTTL;"
           , "NET \"LCD_D<1>\"  LOC=P92 | IOSTANDARD=LVTTL;"
           , "NET \"LCD_D<2>\"  LOC=P94 | IOSTANDARD=LVTTL;"
           , "NET \"LCD_D<3>\"  LOC=P95 | IOSTANDARD=LVTTL;"
           , "NET \"LCD_RS\"    LOC=P98 | IOSTANDARD=LVTTL;"
           , "NET \"LCD_EN\"    LOC=P2  | IOSTANDARD=LVTTL;"
           , ""
           , "NET \"PS2_DAT\"   LOC=P85 | IOSTANDARD=LVTTL | DRIVE=8 | SLEW=FAST | PULLUP;"
           , "NET \"PS2_CLK\"   LOC=P83 | IOSTANDARD=LVTTL | DRIVE=8 | SLEW=FAST | PULLUP;"
           ]
