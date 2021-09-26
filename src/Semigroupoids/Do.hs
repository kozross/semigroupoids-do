{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Semigroupoids.Do
-- Copyright: (C) Koz Ross 2021
-- License: Apache 2.0
-- Maintainer: koz.ross@retro-freedom.nz
-- Stability: Experimental
-- Portability: GHC only
--
-- Provides definitions needed to use do-notation (by way of @QualifiedDo@)
-- using the more general type classes provided by @semigroupoids@.
--
-- To use this module, enable @QualifiedDo@, import this module qualified, and
-- then prefix @do@ with the qualified name:
--
-- > {-# LANGUAGE QualifiedDo #-}
-- >
-- > module MyModule where
-- >
-- > import Data.Functor.Bind (Bind)
-- > import qualified Semigroupoids.Do as S
-- >
-- > foo :: (Bind m) => m a
-- > foo = S.do
-- >  ...
--
-- This module is designed to work correctly (and similarly generally) with
-- @ApplicativeDo@ and @RecursiveDo@ (inasfar as that is possible).
module Semigroupoids.Do
  ( (>>=),
    (>>),
    fail,
    fmap,
    (<*>),
    join,
    mfix,
    return,
  )
where

import Control.Monad.Fix (mfix)
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import qualified Data.Functor.Bind as Bind
import Data.Functor.Plus (Plus (zero))
import Data.Kind (Type)
import Prelude hiding
  ( fail,
    return,
    (<*>),
    (>>),
    (>>=),
  )

-- | @since 1.0
(>>=) ::
  forall (m :: Type -> Type) (a :: Type) (b :: Type).
  (Bind m) =>
  m a ->
  (a -> m b) ->
  m b
(>>=) = (>>-)

-- | @since 1.0
(>>) ::
  forall (m :: Type -> Type) (a :: Type) (b :: Type).
  (Bind m) =>
  m a ->
  m b ->
  m b
x >> y = x >>- const y

-- | = Important note
--
-- This /ignores/ whatever 'String' you give it. It is a bad idea to use 'fail'
-- as a form of labelled error; instead, it should only be defaulted to when a
-- pattern match fails.
--
-- @since 1.0
fail ::
  forall (m :: Type -> Type) (a :: Type).
  (Plus m) =>
  String ->
  m a
fail _ = zero

-- | @since 1.0
(<*>) ::
  forall (f :: Type -> Type) (a :: Type) (b :: Type).
  (Apply f) =>
  f (a -> b) ->
  f a ->
  f b
(<*>) = (<.>)

-- | @since 1.0
join ::
  forall (m :: Type -> Type) (a :: Type).
  (Bind m) =>
  m (m a) ->
  m a
join = Bind.join

-- | @since 1.0
return ::
  forall (f :: Type -> Type) (a :: Type).
  (Applicative f) =>
  a ->
  f a
return = pure
