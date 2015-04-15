{-# LANGUAGE ScopedTypeVariables #-}
import Language.KansasLava
import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.VHDL
import Data.Sized.Unsigned
import Data.Sized.Matrix as Matrix
import Data.Bits
import Data.List (find)
import Control.Applicative
import Control.Arrow
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)

import Data.Char (ord, chr)

import Debug.Trace

type Letter = X26

fromLetter :: Letter -> Char
fromLetter = chr . (+ ord 'A') . fromIntegral

toLetter :: Char -> Letter
toLetter = fromIntegral . (subtract (ord 'A')) . ord

type Permutation a = Matrix a a

mkPermutation :: String -> Permutation Letter
mkPermutation = Matrix.fromList . map toLetter

type Decoded clk n = Matrix n (Signal clk Bool)

permuteBwd :: (Size n) => Permutation n -> Decoded clk n -> Decoded clk n
permuteBwd p = Matrix.ixmap (p !)

permuteFwd :: (Size n) => Permutation n -> Matrix n (Signal clk Bool) -> Matrix n (Signal clk Bool)
permuteFwd p = Matrix.ixmap $ \i -> maybe (error "Not surjective") fst $ find ((== i) . snd) $ Matrix.assocs p

-- inp :: (Rep a, Size a, Eq a) => a -> Decoded CLK a
-- inp x = Matrix.forAll $ pureS . (== x)

inp :: Char -> Decoded CLK Letter
inp x = Matrix.forAll $ pureS . (== x')
  where
    x' = toLetter x

encode :: (Rep a, Size a) => Decoded clk a -> Signal clk a
encode = foldr (\(n, s) x -> mux s (x, pureS n)) undefinedS . Matrix.assocs

decode :: (Rep n, Size n) => Signal clk n -> Decoded clk n
decode x = Matrix.forAll $ (.==. x) . pureS

rotateFwd :: (Size a, Rep a, Integral a) => Signal clk a -> Decoded clk a -> Decoded clk a
rotateFwd r = unpackMatrix . bitwise . rotateLS r . bitwise . packMatrix

rotateBwd :: (Size a, Rep a, Integral a) => Signal clk a -> Decoded clk a -> Decoded clk a
rotateBwd r = unpackMatrix . bitwise . rotateRS r . bitwise . packMatrix

type Plugboard = Permutation Letter
type Reflector = Permutation Letter
type Rotor a = Matrix a (a, Bool)

rotorFwd :: (Size n, Rep n, Integral n)
         => Rotor n -> Signal clk Bool -> Signal clk n -> Decoded clk n
         -> (Signal clk Bool, Signal clk n, Decoded clk n)
rotorFwd rotor rotateThis r sig = (rotateNext, r', sig')
  where
    (p, notches) = (fmap fst &&& fmap snd) rotor
    rotateNext = pureS notches .!. r
    r' = mux rotateThis (r, next r)
    sig' = rotateFwd r >>> permuteFwd p $ sig

    next x = mux (x .==. pureS maxBound) (x + 1, minBound)

rotorBwd :: (Size n, Rep n, Integral n)
         => Rotor n -> Signal clk n -> Decoded clk n -> Decoded clk n
rotorBwd rotor r = permuteBwd p >>> rotateBwd r
  where
    p = fmap fst rotor

joinRotors :: (Size n, Bounded n, Enum n, Rep a, Size a, Integral a)
           => Matrix n (Rotor a) -> Matrix n (Signal clk a) -> Decoded clk a
           -> (Matrix n (Signal clk a), Decoded clk a)
joinRotors rotors rs sig =
    let (rs', (_, sig')) = Matrix.scanR step ((high, sig), Matrix.zipWith (,) rotors rs)
    in (rs', sig')
  where
    step ((rotateThis, x), (rotor, r)) =
        let (rotateNext, r', x') = rotorFwd rotor rotateThis r x
        in (r', (rotateNext, x'))

backSignal :: (Size n, Bounded n, Enum n, Rep a, Size a, Integral a)
           => Matrix n (Rotor a) -> Matrix n (Signal clk a) -> Decoded clk a
           -> Decoded clk a
backSignal rotors rs sig = F.foldr (uncurry rotorBwd) sig $ Matrix.zipWith (,) rotors rs

enigmaPipe :: (Clock clk, Size n, Enum n)
       => Plugboard -> Matrix n (Rotor Letter) -> Reflector
       -> Matrix n (Signal clk Letter) -> Decoded clk Letter
       -> (Matrix n (Signal clk Letter), Decoded clk Letter)
enigmaPipe plugboard rotors reflector rs sig0 = (rs', sig5)
  where
    sig1 = permuteFwd plugboard $ sig0
    (rs', sig2) = joinRotors rotors rs sig1
    sig3 = permuteFwd reflector sig2
    sig4 = backSignal rotors rs sig3
    sig5 = permuteBwd plugboard sig4

enigma :: (Clock clk, Size n, Enum n)
       => Plugboard -> Matrix n (Rotor Letter) -> Reflector
       -> Matrix n Letter
       -> Decoded clk Letter -> Decoded clk Letter
enigma plugboard rotors reflector rs0 sig = sig'
  where
    (rs', sig') = enigmaPipe plugboard rotors reflector rs sig
    rs = Matrix.zipWith register rs0 rs'

plugboard :: Plugboard
plugboard = mkPermutation "HBGDEFCAIJKOWNLPXRSVYTMQUZ"

reflector :: Reflector
reflector = mkPermutation "FEIPBATSCYVUWZQDOXHGLKMRJN"

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

rotorInit :: Matrix X3 Letter
rotorInit = Matrix.fromList . map toLetter $ "GCR"

rotateRS :: (Size n, Rep n, Integral n)
         => Signal clk n -> Signal clk (Unsigned n) -> Signal clk (Unsigned n)
rotateRS = flip $ primXS2 shallow "rotateR"
  where
    shallow arg count = optX $ do
        arg <- unX arg
        count <- unX count
        return $ rotateR arg $ fromIntegral count

rotateLS :: (Size n, Rep n, Integral n)
         => Signal clk n -> Signal clk (Unsigned n) -> Signal clk (Unsigned n)
rotateLS = flip $ primXS2 shallow "rotateL"
  where
    shallow arg count = optX $ do
        arg <- unX arg
        count <- unX count
        return $ rotateL arg $ fromIntegral count

{-
test :: Signal CLK Letter
test = takeS 1 $ encode $ permuteFwd plugboard $ rotateFwd rotation input
  where
    input = inp 'A'
    rotation = pureS 1

test' :: IO KLEG
test' = reifyFabric $ do
    input <- unpackMatrix <$> inStdLogicVector "INPUT"
    (rot :: Signal CLK Letter) <- inStdLogicVector "ROT"
    outStdLogicVector "OUTPUT" $ packMatrix $ rotateFwd rot input
-}

testInput :: String
testInput = "ENIGMAWASAREALLYCOOLMACHINE"

test :: String -> String
test s = fromSignal $ enigma_ $ toSignal s
  where
    enigma_ = enigma plugboard rotors reflector rotorInit
    toSignal s = decode (toS $ map toLetter s :: Seq Letter)
    fromSignal = map fromLetter . take (Prelude.length s) . catMaybes . fromS . encode
