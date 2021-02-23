{-# LANGUAGE FlexibleInstances, MagicHash #-}

{- |
    Module      :  SDP.Binary
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Binary@ provides @sdp@ instances for binary.
-}
module SDP.Binary
(
  -- * Export
  module Data.Binary
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Unboxed
import SDP.Linear

import SDP.Templates.AnyBorder
import SDP.Templates.AnyChunks

import SDP.Prim.SArray
import SDP.Prim.SBytes

import Data.Binary

default ()

--------------------------------------------------------------------------------

instance (Binary e) => Binary (SArray# e)
  where
    get = do n <- get; fromListN n <$> replicateM n get
    put = putList . listL

instance (Binary e, Unboxed e) => Binary (SBytes# e)
  where
    get = do n <- get; fromListN n <$> replicateM n get
    put = putList . listL

instance (Nullable (rep e), Binary (rep e)) => Binary (AnyChunks rep e)
  where
    put = \ es -> let cs = toChunks es in do put (sizeOf cs); putList cs
    get = do n <- get; fromChunks <$> replicateM n get

instance (Binary i, Index i, Binary (rep e)) => Binary (AnyBorder rep i e)
  where
    put = \ (AnyBorder l u es) -> do put l; put u; put es
    get = liftA3 AnyBorder get get get

