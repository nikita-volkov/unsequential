module Disorderly.Prelude
( 
  module Exports,
  modifyM,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (Alt, scanl)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Except as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Writer.Strict as Exports

-- dlist
-------------------------
import Data.DList as Exports (DList)

-- Utils
-------------------------

{-# INLINE modifyM #-}
modifyM :: Monad m => (a -> m a) -> StateT a m ()
modifyM f =
  get >>= lift . f >>= put
