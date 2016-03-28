module Unsequential
(
  Unsequential,
  runUnsequential,
  unsequential,
)
where

import Unsequential.Prelude
import qualified Unsequential.Execution as Execution


-- |
-- Allows to use the 'Applicative' interface to 
-- compose the actions of the base monad
-- while being abstracted from the order of their successful execution.
data Unsequential m a =
  forall b. Unsequential !(DList (m b)) !([b] -> Maybe a)

instance Functor m => Functor (Unsequential m) where
  fmap f (Unsequential alternatives extractor) =
    Unsequential alternatives (fmap (fmap f) extractor)

instance Applicative m => Applicative (Unsequential m) where
  pure a =
    Unsequential empty (const (pure a))
  (<*>) (Unsequential alternatives1 extractor1) (Unsequential alternatives2 extractor2) =
    Unsequential alternatives3 extractor3
    where
      alternatives3 =
        (fmap . fmap) Left alternatives1 <>
        (fmap . fmap) Right alternatives2
      extractor3 intermediates =
        case partitionEithers intermediates of
          (intermediates1, intermediates2) ->
            ($) <$> extractor1 intermediates1 <*> extractor2 intermediates2

instance MonadTrans Unsequential where
  {-# INLINE lift #-}
  lift =
    unsequential

-- |
-- Runs 'Unsequential' given an implementation of the \"skip\" effect.
-- 
-- The \"skip\" effect can be just @return ()@ in case you don't want
-- skipping or @mzero@ if you want to fail on the attempt to skip.
runUnsequential :: MonadPlus m => Unsequential m a -> m () -> m a
runUnsequential (Unsequential alternatives extractor) skip =
  do
    (remainingAlternatives, results) <- Execution.run (Execution.process skip) (toList alternatives)
    guard (null remainingAlternatives)
    maybe mzero return (extractor results)

-- |
-- Lift a computation in the base monad.
-- 
-- Same as 'lift'.
unsequential :: Monad m => m a -> Unsequential m a
unsequential alternative =
    Unsequential alternatives extractor
    where
      alternatives =
        pure alternative
      extractor =
        \case
          head : _ ->
            return head
          _ ->
            mzero
