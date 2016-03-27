module Disorderly.Prelude
( 
  module Exports,
  modifyM,
  skipSepBy,
  skipSepBy1,
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
  StateT (fmap (\s -> ((), s)) . f)

{-# INLINE skipSepBy #-}
skipSepBy :: Alternative m => m () -> m () -> m ()
skipSepBy one sep =
  skipSepBy1 one sep <|> pure ()

{-# INLINABLE skipSepBy1 #-}
skipSepBy1 :: Alternative m => m () -> m () -> m ()
skipSepBy1 one sep =
  one *> remainders
  where
    remainders =
      (sep *> one *> remainders) <|> pure ()
