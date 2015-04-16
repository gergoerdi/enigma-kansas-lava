{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Enigma where

import Language.KansasLava
import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.VHDL
import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Matrix as Matrix
import Data.Bits
import Data.List (find, concatMap)
import Control.Applicative
import Control.Arrow ((&&&), (>>>))
import Control.Monad (join)
import qualified Data.Foldable as F
import Data.Maybe (mapMaybe, catMaybes)

import Data.Char (ord, chr)

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

encode :: (Rep a, Size a) => Decoded clk a -> Signal clk a
encode = foldr (\(n, s) x -> mux s (x, pureS n)) undefinedS . Matrix.assocs

decode :: (Rep n, Size n) => Signal clk n -> Decoded clk n
decode x = Matrix.forAll $ (.==. x) . pureS

rotateFwd :: forall clk a n. (Size a, Rep a, Num a, n ~ W a, Size (SUCC n))
          => Signal clk a -> Decoded clk a -> Decoded clk a
rotateFwd r = decode . rotate . encode
  where
    rotate x = restrict $ extend x + extend r

    extend :: Signal clk a -> Signal clk (Unsigned (SUCC n))
    extend = unsigned

    restrict :: Signal clk (Unsigned (SUCC n)) -> Signal clk a
    restrict x = unsigned $ mux (x .>=. 26) (x, x - 26)

rotateBwd :: forall clk a n. (Size a, Rep a, Num a, n ~ W a, Size (SUCC n))
          => Signal clk a -> Decoded clk a -> Decoded clk a
rotateBwd r = decode . rotate . encode
  where
    rotate x = restrict $ extend x - extend r

    extend :: Signal clk a -> Signal clk (Unsigned (SUCC n))
    extend = unsigned

    restrict :: Signal clk (Unsigned (SUCC n)) -> Signal clk a
    restrict x = unsigned $ mux (x .>=. 26) (x, x + 26)

type Plugboard = Permutation Letter
type Reflector = Permutation Letter
type Rotor a = Matrix a (a, Bool)

unsignedFromBits :: (Size n) => Matrix n Bool -> Unsigned n
unsignedFromBits = F.foldr (\b x -> 2 * x + if b then 1 else 0) 0

rotorFwd :: forall clk a. (Size a, Rep a, Num a, Size (SUCC (W a)))
         => Rotor a -> Signal clk Bool -> Signal clk a -> Decoded clk a
         -> (Signal clk Bool, Signal clk a, Decoded clk a)
rotorFwd rotor rotateThis r sig = (rotateNext, r', sig')
  where
    (p, notches) = (fmap fst &&& (unsignedFromBits . fmap snd)) rotor
    rotateNext = commentS "rotateNext" (pureS notches) `testABit` r
    r' = mux rotateThis (r, next r)
    sig' = rotateFwd r >>> permuteFwd p $ sig

    next x = mux (x .==. pureS maxBound) (x + 1, minBound)

rotorBwd :: (Size a, Rep a, Num a, Size (SUCC (W a)))
         => Rotor a -> Signal clk a -> Decoded clk a -> Decoded clk a
rotorBwd rotor r = permuteBwd p >>> rotateBwd r
  where
    p = fmap fst rotor

joinRotors :: (Size n, Bounded n, Enum n, Rep a, Size a, Num a, Size (SUCC (W a)))
           => Matrix n (Rotor a) -> Matrix n (Signal clk a) -> Decoded clk a
           -> (Matrix n (Signal clk a), Decoded clk a)
joinRotors rotors rs sig =
    let (rs', (_, sig')) = Matrix.scanR step ((high, sig), Matrix.zipWith (,) rotors rs)
    in (rs', sig')
  where
    step ((rotateThis, x), (rotor, r)) =
        let (rotateNext, r', x') = rotorFwd rotor rotateThis r x
        in (r', (rotateNext, x'))

backSignal :: (Size n, Bounded n, Enum n, Rep a, Size a, Num a, Size (SUCC (W a)))
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

enigma_ :: (Clock clk, Size n, Enum n)
        => Plugboard -> Matrix n (Rotor Letter) -> Reflector
        -> Matrix n Letter
        -> (Signal clk Ack, Decoded clk Letter) -> (Signal clk Ack, Decoded clk Letter)
enigma_ plugboard rotors reflector rs0 (inputReady, sig) = (outputReady, sig')
  where
    (rs', sig') = enigmaPipe plugboard rotors reflector rs sig
    rs'' = Matrix.zipWith (curry $ mux $ fromAck inputReady) (delay <$> rs) rs'
    rs = Matrix.zipWith register rs0 rs''
    outputReady = inputReady

enigma :: (Clock clk, Size n, Enum n)
        => Plugboard -> Matrix n (Rotor Letter) -> Reflector
        -> Matrix n Letter
        -> (Signal clk (Enabled Letter)) -> Signal clk (Enabled Letter)
enigma plugboard rotors reflector rs0 sig = packEnabled (fromAck ready) $ encode sig'
  where
    (ready, sig') = enigma_ plugboard rotors reflector rs0
                    (toAck $ isEnabled sig, decode $ enabledVal sig)


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

{-
testInput :: String
testInput = "ENIGMAWASAREALLYCOOLMACHINE"

test :: String -> String
test s = fromSignal $ enigma' $ toSignal s
  where
    enigma' = enigma plugboard rotors reflector rotorInit
    toSignal s = toS $ concatMap (\c -> [Just $ toLetter c, Nothing, Nothing]) s :: Seq (Enabled Letter)
    fromSignal = map fromLetter . take (Prelude.length s) . mapMaybe join . fromS
-}

testEnigma :: (Clock clk) => (Signal clk (Enabled Letter)) -> Signal clk (Enabled Letter)
testEnigma = enigma plugboard rotors reflector rotorInit
