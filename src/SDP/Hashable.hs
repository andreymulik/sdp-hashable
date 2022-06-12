{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Hashable
    Copyright   :  (c) Andrey Mulik 2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Hashable@ provides 'Hashable' instances for @sdp@ structures.
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
    hashWithSalt salt (AnyBorder _ _ rep) = salt `hashWithSalt` rep

instance (Hashable (rep e), Bordered1 rep Int e, Linear1 rep e) => Hashable (AnyChunks rep e)
  where
    hashWithSalt salt = foldl' hashWithSalt salt . toChunks



