{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module GtkT where

import Control.Monad.Trans.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative
import Data.IORef

newtype GtkT s m a = GtkT { myGtkT :: ReaderT (IORef s) m a } deriving (Functor, Applicative, Monad, MonadIO)
runGtkT = runReaderT . myGtkT

instance MonadIO m => MonadState s (GtkT s m) where
	get = GtkT (ask >>= liftIO . readIORef)
	put s = GtkT (ask >>= liftIO . flip writeIORef s)