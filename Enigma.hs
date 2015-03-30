{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Enigma where

import Language.KansasLava
import Data.Sized.Matrix as Matrix
import Data.Char (ord, chr)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

type Letter = X26 -- 26 letters 'A'..'Z'

fromLetter :: Letter -> Char
fromLetter = chr . (+ ord 'A') . fromIntegral

toLetter :: Char -> Letter
toLetter = fromIntegral . (subtract (ord 'A')) . ord

type Permutation a = Matrix a a

mkPermutation :: String -> Permutation Letter
mkPermutation = Matrix.fromList . map toLetter

findS :: (Clock clk, Rep a, Size a, Rep b)
      => (Signal clk b -> Signal clk Bool)
      -> Signal clk (Matrix a b) -> Signal clk a
findS match = foldr (\(y, x) y0 -> mux (match x) (y0, pureS y)) undefinedS . Matrix.assocs . unpack

-- Applying permutations as substitutions
substFwd :: (Clock clk, Rep a, Size a)
         => Signal clk (Permutation a) -> Signal clk a -> Signal clk a
substFwd = (.!.)

substBwd :: (Clock clk, Rep a, Size a)
         => Signal clk (Permutation a) -> Signal clk a -> Signal clk a
substBwd pi x = findS (.==. x) pi

scramble :: (Clock clk, Rep a, Size a, Enum a)
         => ((Signal clk Bool, Signal clk a), Signal clk (Rotor a))
         -> (Signal clk (Rotor a), (Signal clk Bool, Signal clk a))
scramble ((rotateThis, c), rotor) = (rotor', (rotateNext, c'))
  where
    c' = fst . unpack $ rotor .!. c
    rotateNext = snd . unpack $ rotor .!. minBound
    rotor' = mux rotateThis (rotor, rotate rotor)

rotate :: (Clock clk, Rep a, Size a, Enum a, Rep b)
       => Signal clk (Matrix a b) -> Signal clk (Matrix a b)
rotate = pack . ixmap xform . unpack
  where
    xform i = if i == maxBound then minBound else succ i

joinRotors :: (Clock clk, Size n, Enum n, Rep a, Size a, Enum a)
           => Matrix n (Signal clk (Rotor a)) -> Signal clk a
           -> (Matrix n (Signal clk (Rotor a)), Signal clk a)
joinRotors rotors inputChar = (rotors', outputChar)
  where
    (rotors', (_, outputChar)) = Matrix.scanR scramble ((high, inputChar), rotors)

backSignal :: (Clock clk, Size n, Rep a, Size a)
           => Matrix n (Signal clk (Rotor a)) -> Signal clk a -> Signal clk a
backSignal rotors inputChar = foldr (substBwd . dropNotches) inputChar $ Matrix.toList $ rotors
  where
    dropNotches :: (Clock clk, Rep a, Size a)
                => Signal clk (Rotor a) -> Signal clk (Permutation a)
    dropNotches = pack . fmap (fst . unpack) . unpack

type Plugboard = Permutation Letter

plugboard :: Plugboard
plugboard = mkPermutation "HBGDEFCAIJKOWNLPXRSVYTMQUZ"

type Reflector = Permutation Letter

reflector :: Reflector
reflector = mkPermutation "FEIPBATSCYVUWZQDOXHGLKMRJN"

type Rotor a = Matrix a (a, Bool)

mkRotor :: String -> String -> Rotor Letter
mkRotor perm notches = addNotch <$> mkPermutation perm
  where
    addNotch x = (x, x `elem` notches')
    notches' = map toLetter notches

rotors :: Matrix X3 (Rotor Letter)
rotors = Matrix.fromList $
         [ "RJICAWVQZODLUPYFEHXSMTKNGB" >< "IO"
         , "DWYOLETKNVQPHURZJMSFIGXCBA" >< "B"
         , "FGKMAJWUOVNRYIZETDPSHBLCQX" >< "CK"
         ]
  where
    (><) = mkRotor

enigmaLoop :: (Clock clk, Size n, Enum n)
           => Plugboard -> Reflector
           -> (Matrix n (Signal clk (Rotor Letter)), Signal clk Letter)
           -> (Matrix n (Signal clk (Rotor Letter)), Signal clk Letter)
enigmaLoop plugboard reflector (rotors, c0) = (rotors', c5)
  where
    c1 = substFwd (pureS plugboard) c0
    (rotors', c2) = joinRotors rotors c1
    c3 = substFwd (pureS reflector) c2
    c4 = backSignal rotors c3
    c5 = substBwd (pureS plugboard) c4

data EnigmaCfg n = EnigmaCfg{ cfgPlugboard :: Plugboard
                            , cfgReflector :: Reflector
                            , cfgRotors :: Matrix n (Rotor Letter)
                            }
                 deriving Show

mkEnigma :: (Size n)
         => Plugboard -> Reflector -> Matrix n (Rotor Letter)
         -> Matrix n Letter -> EnigmaCfg n
mkEnigma plugboard reflector rotors startingPositions =
    EnigmaCfg{ cfgPlugboard = plugboard
             , cfgReflector = reflector
             , cfgRotors = rotors' }
  where
    rotors' = Matrix.zipWith rotateBy startingPositions rotors
    rotateBy i = ixmap (shiftLetter i)

    shiftLetter :: Letter -> Letter -> Letter
    shiftLetter i c = fromIntegral $ (fromIntegral i + fromIntegral c) `mod` n
      where
        n = fromIntegral (maxBound :: Letter) + 1 :: Int

enigma :: (Clock clk, Size n, Enum n) => EnigmaCfg n -> Signal clk (Enabled Letter) -> Signal clk Letter
enigma EnigmaCfg{..} s = s'
  where
    (rotors', s') = enigmaLoop cfgPlugboard cfgReflector
                    (unpack $ register cfgRotors rotors, enabledVal s)
    rotors = mux (isEnabled s) (delay rotors, pack rotors')

testEnigma :: EnigmaCfg X3
testEnigma = mkEnigma plugboard reflector rotors (Matrix.fromList . map toLetter $ "GCR")

test1 :: String
test1 = test "ENIGMAWASAREALLYCOOLMACHINE"

test :: String -> String
test s = fromSignal $ enigma_ $ toSignal s
  where
    enigma_ = enigma testEnigma :: Seq (Enabled Letter) -> Seq Letter
    toSignal = toS . concatMap (\c -> [Just $ toLetter c, Nothing, Nothing])
    fromSignal = map fromLetter . take (Prelude.length s) . catMaybes . fromS
    fromSignal = map fromLetter . take (Prelude.length s) . catMaybes . fromS
