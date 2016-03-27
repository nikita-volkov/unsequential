module Disorderly
(
  Disorderly,
  runDisorderly,
  disorderly,
)
where

import Disorderly.Prelude
import qualified Disorderly.Execution as Execution


-- |
-- Allows to use the 'Applicative' interface to 
-- compose the actions of the base monad
-- while being abstracted from the order of their successful execution.
data Disorderly m a =
  forall b. Disorderly !(DList (m b)) !([b] -> Maybe a)

instance Functor m => Functor (Disorderly m) where
  fmap f (Disorderly alternatives extractor) =
    Disorderly alternatives (fmap (fmap f) extractor)

instance Applicative m => Applicative (Disorderly m) where
  pure a =
    Disorderly empty (const (pure a))
  (<*>) (Disorderly alternatives1 extractor1) (Disorderly alternatives2 extractor2) =
    Disorderly alternatives3 extractor3
    where
      alternatives3 =
        (fmap . fmap) Left alternatives1 <>
        (fmap . fmap) Right alternatives2
      extractor3 intermediates =
        case partitionEithers intermediates of
          (intermediates1, intermediates2) ->
            ($) <$> extractor1 intermediates1 <*> extractor2 intermediates2

instance MonadTrans Disorderly where
  {-# INLINE lift #-}
  lift =
    disorderly

-- |
-- Runs 'Disorderly' given an implementation of the \"skip\" action,
-- which might as well be just @return ()@ in case you don't want
-- skipping or @mzero@ if you want to fail on the attempt to skip.
runDisorderly :: MonadPlus m => Disorderly m a -> m () -> m a
runDisorderly (Disorderly alternatives extractor) skip =
  do
    (remainingAlternatives, results) <- Execution.run (Execution.process skip) (toList alternatives)
    guard (null remainingAlternatives)
    maybe mzero return (extractor results)

-- |
-- Lift a computation in the base monad.
-- 
-- Same as 'lift'.
disorderly :: Monad m => m a -> Disorderly m a
disorderly alternative =
    Disorderly alternatives extractor
    where
      alternatives =
        pure alternative
      extractor =
        \case
          head : _ ->
            return head
          _ ->
            mzero
