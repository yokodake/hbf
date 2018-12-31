-- | Interactive mode for the interpreter
-- would kind of like a REPL with a debugger experience here:
-- we can step/break/read/modify our state as we'd like
-- while also alter the state with normal brainfuck commands.
--
-- there would be a difference between executing code and injecting it:
-- @
-- > ++++
-- @
-- would increment the current cell by 4
-- @
-- > :inj +++
-- @
-- would inject @++++@ where at right before ip is pointing to.
-- and so we'd be able to step through that code we just gave in the REPL
module Interactive where

import           Data.Bits (Bits, (.|.), bit)
import           Data.Char (isPrint, chr)
import           Data.List (intersperse)
import           Data.Maybe (isNothing)
import           Data.Word (Word8)
import           Text.Printf (printf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import           Core

import           Debug.Trace

-- | @showMem start n i mem@ prints a block of memory of length @n@
-- from @start@ highlighting the current cell by pointer
-- TODO bit mask for printing options (addr, hex, ASCII)
showMem :: (Bits b, BFMemory a) => Int -> Int -> b -> IState a -> String
showMem s n _ st@IState{memory=mem,ptr=i} =
  concat [addr, "  ", values, "  ", ascii block]

  where
    (MemS addr block eidx) = getMemBlock s n st
    ascii = map (printOrDot . chr . fromIntegral)
    values :: String
    values | Just i <- eidx = concat [before, elem, after]
           | otherwise      = fmt block
           where
             i' = i - s
             before = fmt $ take i' block
             after  = fmt $ drop (succ i') block
             elem = highlight . printf "%02x" . head $ drop i' block
             highlight x = concat ["[",x,"]"]

    fmt :: [Word8] -> String
    fmt = unwords . map (printf "%02x")
    printOrDot w | isPrint w = w
                 | otherwise = '.'

data MemS = MemS { _addr :: String
                 , _block :: [Word8]
                 , _eidx :: Maybe Int
                 }

getMemBlock :: (BFMemory a) => Int -> Int -> IState a -> MemS
getMemBlock s n st@IState{memory=mem, ptr=p} =
  MemS addr block eidx
  where
    addr = printf "0x%04x" s
    block = slice s (min n $ msize mem - s) mem
    eidx = if p `inRange` (s, msize mem) then
             Just (p - s)
           else
             Nothing

-- | @a <= x && x < b@
inRange :: Ord a => a -> (a, a) -> Bool
inRange x (a, b) = a <= x && x < b
{-# INLINABLE inRange #-}

foo = IState (VMemory $ V.fromList [97..102]) (V.fromList []) [] 0 0 []

addrm, asciim, decm :: Bits a => a
addrm  = bit 0
asciim = bit 1
decm   = bit 2
