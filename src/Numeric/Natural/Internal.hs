{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Natural.Internal
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module exposes the potentially unsafe operations that are sometimes
-- needed for efficiency: The Natural data constructor and unsafePred.
--
----------------------------------------------------------------------------
module Numeric.Natural.Internal
  ( Natural(..)
  , Whole(..)
  , natural
  ) where

import Data.Word
import Data.Bits
import Data.Ix
#ifdef LANGUAGE_DeriveDataTypeable
import Data.Typeable
#endif

newtype Natural = Natural { runNatural :: Integer } deriving
  ( Eq
  , Ord
  , Ix
#ifdef LANGUAGE_DeriveDataTypeable
  , Typeable
#endif
  )

natural :: a -> (a -> a) -> Natural -> a
natural a _ 0 = a
natural a f n = natural (f a) f (unsafePred n)
{-# INLINEABLE natural #-}

instance Show Natural where
  showsPrec d (Natural n) = showsPrec d n

instance Read Natural where
  readsPrec d = map (\(n, s) -> (Natural n, s)) . readsPrec d

instance Num Natural where
  Natural n + Natural m = Natural (n + m)
  {-# INLINE (+) #-}
  Natural n * Natural m = Natural (n * m)
  {-# INLINE (*) #-}
  Natural n - Natural m | result < 0 = error "Natural.(-): negative result"
                        | otherwise  = Natural result
    where result = n - m
  {-# INLINE (-) #-}
  abs (Natural n) = Natural n
  {-# INLINE abs #-}
  signum (Natural n) = Natural (signum n)
  {-# INLINE signum #-}
  fromInteger n
    | n >= 0 = Natural n
    | otherwise = error "Natural.fromInteger: negative"
  {-# INLINE fromInteger #-}

instance Bits Natural where
  Natural n .&. Natural m = Natural (n .&. m)
  {-# INLINE (.&.) #-}
  Natural n .|. Natural m = Natural (n .|. m)
  {-# INLINE (.|.) #-}
  xor (Natural n) (Natural m) = Natural (xor n m)
  {-# INLINE xor #-}
  complement _ = error "Bits.complement: Natural complement undefined"
  {-# INLINE complement #-}
  shift (Natural n) = Natural . shift n
  {-# INLINE shift #-}
  rotate (Natural n) = Natural . rotate n
  {-# INLINE rotate #-}
  bit = Natural . bit
  {-# INLINE bit #-}
  setBit (Natural n) = Natural . setBit n
  {-# INLINE setBit #-}
  clearBit (Natural n) = Natural . clearBit n
  {-# INLINE clearBit #-}
  complementBit (Natural n) = Natural . complementBit n
  {-# INLINE complementBit #-}
  testBit = testBit . runNatural
  {-# INLINE testBit #-}
  bitSize = bitSize . runNatural
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  shiftL (Natural n) = Natural . shiftL n
  {-# INLINE shiftL #-}
  shiftR (Natural n) = Natural . shiftR n
  {-# INLINE shiftR #-}
  rotateL (Natural n) = Natural . rotateL n
  {-# INLINE rotateL #-}
  rotateR (Natural n) = Natural . rotateR n
  {-# INLINE rotateR #-}
#if MIN_VERSION_base(4,6,0)
  popCount = popCountDefault
  {-# INLINE popCount #-}
#endif

instance Real Natural where
  toRational (Natural a) = toRational a
  {-# INLINE toRational #-}

instance Enum Natural where
  pred (Natural 0) = error "Natural.pred: 0"
  pred (Natural n) = Natural (pred n)
  {-# INLINE pred #-}
  succ (Natural n) = Natural (succ n)
  {-# INLINE succ #-}
  fromEnum (Natural n) = fromEnum n
  {-# INLINE fromEnum #-}
  toEnum n | n < 0     = error "Natural.toEnum: negative"
           | otherwise = Natural (toEnum n)
  {-# INLINE toEnum #-}

instance Integral Natural where
  quot (Natural a) (Natural b) = Natural (quot a b)
  {-# INLINE quot #-}
  rem (Natural a) (Natural b) = Natural (rem a b)
  {-# INLINE rem #-}
  div (Natural a) (Natural b) = Natural (div a b)
  {-# INLINE div #-}
  mod (Natural a) (Natural b) = Natural (mod a b)
  {-# INLINE mod #-}
  divMod (Natural a) (Natural b) = (Natural q, Natural r) where (q,r) = divMod a b
  {-# INLINE divMod #-}
  quotRem (Natural a) (Natural b) = (Natural q, Natural r) where (q,r) = quotRem a b
  {-# INLINE quotRem #-}
  toInteger = runNatural
  {-# INLINE toInteger #-}

-- | A refinement of 'Integral' to represent types that do not contain negative numbers.
class Integral n => Whole n where
  toNatural :: n -> Natural
  unsafePred :: n -> n

instance Whole Word where
  toNatural = Natural . toInteger
  unsafePred n = n - 1
  {-# INLINE toNatural #-}
  {-# INLINE unsafePred #-}

instance Whole Word8 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1
  {-# INLINE toNatural #-}
  {-# INLINE unsafePred #-}

instance Whole Word16 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1
  {-# INLINE toNatural #-}
  {-# INLINE unsafePred #-}

instance Whole Word32 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1
  {-# INLINE toNatural #-}
  {-# INLINE unsafePred #-}

instance Whole Word64 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1
  {-# INLINE toNatural #-}
  {-# INLINE unsafePred #-}

instance Whole Natural where
  toNatural = id
  unsafePred (Natural n) = Natural (n - 1)
  {-# INLINE toNatural #-}
  {-# INLINE unsafePred #-}
