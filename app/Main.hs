{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields -fexcess-precision -funfolding-use-threshold=16 -fmax-simplifier-iterations=8 -flate-dmd-anal -fspec-constr-keen -fstatic-argument-transformation -fcross-module-specialise -fworker-wrapper -fno-warn-partial-type-signatures #-}

{- lightning_stick.hs — Text colorizer utility
   (C) 2025 Pepijn van der Stap <github@vanderstap.info>
   
   Build: ghc -O2 Main.hs
   Usage: ./rainbow [text]
-}

module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import GHC.Exts
import GHC.Base (Int(I#))
import Data.Bits
import Data.Char
import Data.Proxy
import GHC.TypeLits (Nat, KnownNat, natVal, CmpNat, TypeError, ErrorMessage(..))
import Data.Kind (Type, Constraint)
import Data.Type.Bool
import Data.Type.Equality

data ThunderState# = Thunder# (State# RealWorld -> (# State# RealWorld, Int# #))

type family IsPrime (n :: Nat) :: Bool where
    IsPrime 2 = 'True
    IsPrime 3 = 'True
    IsPrime 5 = 'True
    IsPrime 11 = 'True
    IsPrime 17 = 'True
    IsPrime 37 = 'True
    IsPrime 67 = 'True
    IsPrime 131 = 'True
    IsPrime 257 = 'True
    IsPrime 523 = 'True
    IsPrime 1031 = 'True
    IsPrime 2053 = 'True
    IsPrime 4099 = 'True
    IsPrime 8219 = 'True
    IsPrime 16421 = 'True
    IsPrime 32771 = 'True
    IsPrime 65539 = 'True
    IsPrime n = 'False

type family Generator (p :: Nat) :: Nat where
    Generator 3 = 2
    Generator 5 = 2
    Generator 11 = 2
    Generator 17 = 3
    Generator 37 = 2
    Generator 67 = 2
    Generator 131 = 2
    Generator 257 = 3
    Generator p = 1

type family Coprime (n :: Nat) (p :: Nat) :: Nat where
    Coprime 3 p = 1
    Coprime 5 p = 1
    Coprime 11 p = 3
    Coprime 17 p = 3
    Coprime 37 p = 5
    Coprime 67 p = 5
    Coprime 131 p = 3
    Coprime 257 p = 3
    Coprime n p = 1

data Phase = Init | Transform | Render | Complete

data Security = Secure | Insecure
data Performance = Fast | Slow

type family Optimize (s :: Security) (p :: Performance) :: Constraint where
    Optimize 'Secure 'Fast = ()
    Optimize 'Secure 'Slow = TypeError ('Text "Secure but slow configuration detected")
    Optimize 'Insecure 'Fast = TypeError ('Text "Fast but insecure configuration")
    Optimize 'Insecure 'Slow = TypeError ('Text "Neither secure nor fast")

data Pipeline :: Phase -> * -> * where
    Initialize :: String -> Pipeline 'Init String
    Transformed :: String -> Pipeline 'Transform String  
    Rendered :: String -> Pipeline 'Render String
    Done :: forall a. a -> Pipeline 'Complete a

data Tagged (s :: Security) (p :: Performance) a = Tagged a

newtype SafeInt (n :: Nat) = SafeInt { getSafeInt :: Int }

type family SafeRange (n :: Nat) :: Constraint where
    SafeRange n = IfConstraint (CmpNat n 65536) () (TypeError ('Text "Range too large"))

type family IfConstraint (o :: Ordering) (a :: Constraint) (b :: Constraint) :: Constraint where
    IfConstraint 'LT a b = a
    IfConstraint 'EQ a b = a  
    IfConstraint 'GT a b = b

data Config = Config
    { config_seed  :: {-# UNPACK #-} !Word
    , config_phase :: {-# UNPACK #-} !Double
    , config_text  :: String
    }

data ColorScheme :: * where
    Rainbow :: Int -> ColorScheme
    Static :: Int -> ColorScheme
    Gradient :: Int -> Int -> ColorScheme
    Cyclic :: forall (p :: Nat). (KnownNat p, IsPrime p ~ 'True) => Proxy p -> Int -> ColorScheme

data CyclicGroup (p :: Nat) where
    MkCyclic :: (KnownNat p, IsPrime p ~ 'True) => 
                { cyclic_prime :: Integer
                , cyclic_generator :: Integer  
                , cyclic_coprime :: Integer
                } -> CyclicGroup p

class ModularArithmetic a b | a -> b where
    modExp :: a -> a -> a -> b
    modMul :: a -> a -> a -> b
    
instance ModularArithmetic Integer Integer where
    modExp base exp modulus = go base exp 1
      where
        go _ 0 acc = acc `mod` modulus
        go b e acc 
            | e `mod` 2 == 1 = go (b * b `mod` modulus) (e `div` 2) (acc * b `mod` modulus)
            | otherwise = go (b * b `mod` modulus) (e `div` 2) acc
    modMul a b m = (a * b) `mod` m

data RangeIterator = RangeIterator
    { iter_prime :: !Integer
    , iter_generator :: !Integer
    , iter_current :: !Integer
    , iter_start :: !Integer
    , iter_limit :: !Integer
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> processText Nothing
        ("--text":text:_) -> processText (Just text)
        text -> processText (Just $ unwords text)

validateInput :: String -> Either String String
validateInput input
    | length input > 1024 = Left "Input too long (max 1024 characters)"
    | any (\c -> ord c < 32 && c /= '\t' && c /= '\n') input = Left "Invalid control characters detected"
    | otherwise = Right $ take 1024 $ filter isValidChar input
  where
    isValidChar c = isPrint c || c == ' ' || c == '\t' || c == '\n'

processText :: Maybe String -> IO ()
processText mtext = do
    putStr "Lightning Stick Text Colorizer\n\n"
    hFlush stdout
    
    rawInput <- case mtext of
        Nothing -> do
            putStr "Enter text: "
            hFlush stdout
            getLine
        Just t -> pure t
    
    input <- case validateInput rawInput of
        Left err -> do
            putStrLn $ "Validation failed: " ++ err
            pure ""
        Right validated -> pure validated
    
    if null input
        then putStrLn "No valid input provided."
        else do
            putStrLn "Processing...\n"
    
    let pipeline = Initialize input
        config = Config 0xC000_0082 3.14159 input
    putStrLn $ runPipeline pipeline config
    
    putStrLn "Done."

runPipeline :: Pipeline 'Init String -> Config -> String
runPipeline (Initialize text) config =
    let transformed = Transformed (transform text)
        rendered = case transformed of
            Transformed t -> Rendered (makeBanner t)
            _ -> error "unreachable"
    in case rendered of
        Rendered banner -> applyColorScheme (selectScheme config) banner
        _ -> error "unreachable"

selectScheme :: Config -> ColorScheme
selectScheme (Config seed phase _) = 
    if phase > 2.0 
    then Cyclic (Proxy @32771) (fromIntegral seed)  -- use larger prime for more complex patterns
    else Rainbow 0

render :: Config -> String
render (Config seed phase text) = 
    let banner = makeBanner $ transform text
    in applyColorScheme (Rainbow 0) banner

applyColorScheme :: ColorScheme -> String -> String
applyColorScheme scheme text = case scheme of
    Rainbow n -> colorize n text
    Static c -> map (\ch -> "\ESC[38;5;" ++ show c ++ ";48;5;233m" ++ [ch] ++ "\ESC[0m") text >>= id
    Gradient s e -> colorize s text
    Cyclic p seed -> cyclicColorize p seed text

cyclicColorize :: forall p. (KnownNat p, IsPrime p ~ 'True) => Proxy p -> Int -> String -> String
cyclicColorize _ seed text = 
    let p = natVal (Proxy @p)
        g = getGenerator p
        iter = mkRangeIterator (fromIntegral p) g (fromIntegral seed)
    in zipWith (\c i -> genColor i c) text (iterate nextCyclic iter) >>= id
  where
    genColor it c = 
        let rawCode = fromIntegral (iter_current it) `mod` 216
            code = max 16 $ min 231 $ rawCode + 16  
            ansi = safeAnsiColor code 233
        in ansi ++ [c] ++ "\ESC[0m"
    
    nextCyclic it = it { iter_current = modMul (iter_current it) (iter_generator it) (iter_prime it) }

getGenerator :: Integer -> Integer
getGenerator 3 = 2      -- 2^1 + 1
getGenerator 5 = 2      -- 2^2 + 1  
getGenerator 11 = 2     -- 2^3 + 3
getGenerator 17 = 3     -- 2^4 + 1
getGenerator 37 = 2     -- 2^5 + 5
getGenerator 67 = 2     -- 2^6 + 3
getGenerator 131 = 2    -- 2^7 + 3
getGenerator 257 = 3    -- 2^8 + 1
getGenerator 523 = 2    -- 2^9 + 11
getGenerator 1031 = 21  -- 2^10 + 7
getGenerator 2053 = 2   -- 2^11 + 5
getGenerator 4099 = 2   -- 2^12 + 3
getGenerator 8219 = 2   -- 2^13 + 27
getGenerator 16421 = 2  -- 2^14 + 37
getGenerator 32771 = 2  -- 2^15 + 3
getGenerator 65539 = 2  -- 2^16 + 3
getGenerator _ = 2

mkRangeIterator :: Integer -> Integer -> Integer -> RangeIterator
mkRangeIterator p g seed = RangeIterator p g start start p
  where start = modExp g seed p

data Endianness = BigEndian | LittleEndian

type family SystemEndian :: Endianness where
    SystemEndian = 'LittleEndian  -- x86_64 assumption

class BitTwiddling a where
    rotateLeft# :: a -> Int -> a
    popCount# :: a -> Int
    reverseBits# :: a -> a

instance BitTwiddling Word where
    rotateLeft# w n = rotateL w n
    popCount# = popCount  
    reverseBits# w = foldl (\acc i -> if testBit w i then setBit acc (63-i) else acc) 0 [0..63]

makeBanner :: String -> String
makeBanner content =
    let footer1 = "░░░ Pepijn van der Stap ░░░ x-stp ░░░ LIGHTNING STICK ░░░"
        footer2 = "github@vanderstap.info"
        minWidth = maximum [length content + 4, length footer1 + 6, length footer2 + 6]
        width = if minWidth < 70 then 70 else minWidth
        innerWidth = width - 4
        
        makeLine c = "║  " ++ replicate innerWidth c ++ "  ║"
        topBorder = "╔" ++ replicate (width - 2) '═' ++ "╗"
        botBorder = "╚" ++ replicate (width - 2) '═' ++ "╝"
        divider = "╠" ++ replicate (width - 2) '═' ++ "╣"
        
        contentLine = "║  " ++ pad innerWidth content ++ "  ║"
        footer1Line = "║  " ++ pad innerWidth footer1 ++ "  ║"
        footer2Line = "║  " ++ pad innerWidth footer2 ++ "  ║"
        
    in unlines 
        [ topBorder
        , makeLine '█'
        , makeLine '█'
        , contentLine
        , makeLine '█'
        , makeLine '█'
        , divider
        , footer2Line
        , footer1Line
        ]

safeAnsiColor :: Int -> Int -> String
safeAnsiColor fg bg
    | fg >= 0 && fg <= 255 && bg >= 0 && bg <= 255 = 
        "\ESC[38;5;" ++ show fg ++ ";48;5;" ++ show bg ++ "m"
    | otherwise = ""

safeColorCalc :: Int -> Int
safeColorCalc n
    | n < 0 = 16
    | n > 10000 = 231  -- Cap at reasonable value
    | otherwise = 
        let phase = fromIntegral (n `mod` 1000) * 0.08
            r = max 0 $ min 255 $ round (128 + 127 * sin phase)
            g = max 0 $ min 255 $ round (128 + 127 * sin (phase + 2.0944))
            b = max 0 $ min 255 $ round (128 + 127 * sin (phase + 4.1888))
            code = 16 + 36 * (r `quot` 51) + 6 * (g `quot` 51) + (b `quot` 51)
        in max 16 $ min 231 code

colorize :: Int -> String -> String
colorize !_ [] = []
colorize !n (c:cs) = 
    let !code = safeColorCalc n
        !ansi = safeAnsiColor code 233
    in ansi ++ [c] ++ "\ESC[0m" ++ colorize (n+1) cs

class Transformable a where
    transform' :: a -> a

instance Transformable Char where
    transform' = subst . toUpper
      where
        subst = \case
            'A' -> '4'; 'E' -> '3'; 'I' -> '1'; 'O' -> '0'
            'S' -> '5'; 'T' -> '7'; 'G' -> '6'; 'B' -> '8'
            'L' -> '£'; 'Z' -> '2'; 'C' -> '©'; 'K' -> 'K'
            'X' -> '×'; 'U' -> 'µ'; 'N' -> 'И'; 'R' -> 'Я'
            c -> c
        
        toUpper c
            | c >= 'a' && c <= 'z' = chr (ord c - 32)
            | otherwise = c

instance Transformable String where
    transform' = map transform'

transform :: String -> String
transform = transform'

pad :: Int -> String -> String
pad width s = 
    let len = length s
        total = width - len
        left = total `div` 2
    in replicate left ' ' ++ s ++ replicate (total - left) ' '

type family Min (a :: Nat) (b :: Nat) :: Nat where
    Min 0 b = 0
    Min a 0 = 0  
    Min a b = IfNat (CmpNat a b) a b

type family IfNat (cmp :: Ordering) (a :: Nat) (b :: Nat) :: Nat where
    IfNat 'LT a b = a
    IfNat 'EQ a b = a
    IfNat 'GT a b = b
