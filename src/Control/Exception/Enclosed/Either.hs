{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Exception.Enclosed.Either
  ( eTry
  , eExIO
  , eIoTry
  , eIOExIO
  , eTextTry
  , eTxIO
  , eIOExTxIO
  ) where

import Data.Text as T
import Data.Maybe
import Data.Monoid
import Data.Either.Combinators
import Control.DeepSeq

import Control.Monad.Trans.Control
import Control.Exception.Enclosed
import Control.Exception.Lifted
import Control.Monad.Trans.Either

fromIOException' :: SomeException -> IOException
fromIOException' e
  = fromMaybe (throw . AssertionFailed $ "Not an IOException:" <> show e)
  $ fromException e

-- | Runs provided @IO@ action, captures synchronous exceptions as @Left@ values,
-- re-throws asynchronous exceptions.
--
-- /Note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @NFData@ typeclass
eTry  , eExIO   :: (MonadBaseControl IO (EitherT e IO), NFData a)
                => IO a -> EitherT SomeException IO a
eTry    = EitherT . tryAnyDeep
eExIO   = EitherT . tryAnyDeep

-- | Runs provided @IO@ action, captures synchronous @IOException@ as @Left@
-- values, re-throws asynchronous exceptions (and synchronous non-IOExceptions).
--
-- /note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @nfdata@ typeclass
eIoTry, eIOExIO :: (MonadBaseControl IO (EitherT e IO), NFData a)
                => IO a -> EitherT IOException IO a
eIoTry  = EitherT . fmap (mapLeft fromIOException') . tryAnyDeep
eIOExIO = EitherT . fmap (mapLeft fromIOException') . tryAnyDeep

-- | Runs provided @IO@ action, captures synchronous @IOException@ as left @Text@
-- values, re-throws asynchronous exceptions (and synchronous non-IOExceptions).
--
-- /note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @nfdata@ typeclass
eTextTry, eTxIO, eIOExTxIO
  :: (MonadBaseControl IO (EitherT e IO), NFData a)
  => IO a -> EitherT Text IO a
eTextTry = bimapEitherT (T.pack . show) id . eIOExIO
eTxIO    = eTextTry
eIOExTxIO= eTextTry


