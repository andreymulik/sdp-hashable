{-# LANGUAGE MagicHash, FlexibleInstances #-}

{- |
    Module      :  SDP.Hashable
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Hashable@ provides 'Hashable' for SDP structures.
-}
module SDP.Hashable
  (
    -- * Export
    module Data.Hashable
  )
where

import Prelude ()
import SDP.SafePrelude
import SDP.Templates.AnyBorder
import SDP.Templates.AnyChunks
import SDP.Prim.SArray
import SDP.Prim.SBytes

import Data.Hashable

default ()

--------------------------------------------------------------------------------

instance (Hashable e) => Hashable (SArray# e)
  where
    hashWithSalt = foldl' hashWithSalt

instance (Unboxed e) => Hashable (SBytes# e)
  where
    hashWithSalt = hashSBytesWith#

instance (Hashable (rep e), Hashable i, Index i) => Hashable (AnyBorder rep i e)
  where
    hashWithSalt salt (AnyBorder l u rep) = salt `hashWithSalt` l
                                                 `hashWithSalt` u
                                                 `hashWithSalt` rep

instance (Hashable (rep e)) => Hashable (AnyChunks rep e)
  where
    hashWithSalt salt (AnyChunks rep) = foldl' hashWithSalt salt rep


