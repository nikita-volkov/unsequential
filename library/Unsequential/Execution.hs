module Unsequential.Execution
where

import Unsequential.Prelude


type Execution b m a =
  StateT ([m b], [b]) m a

{-# INLINE run #-}
run :: Monad m => Execution b m () -> [m b] -> m ([m b], [b])
run execution alternatives =
  {-# SCC "run" #-} 
  execStateT execution (alternatives, [])

{-# INLINABLE tryAlternatives #-}
tryAlternatives :: MonadPlus m => m () -> Execution b m ()
tryAlternatives skip =
  {-# SCC "tryAlternatives" #-} 
  modifyM loop
  where
    loop (alternatives, results) =
      case alternatives of
        alternativesHead : alternativesTail ->
          mplus tryHead tryTail
          where
            tryHead =
              liftM (\result -> (alternativesTail, result : results)) $
              alternativesHead
            tryTail =
              liftM (\(alternatives, results) -> (alternativesHead : alternatives, results)) $
              loop (alternativesTail, results)
        _ ->
          skip $> (alternatives, results)

untilNoAlternativesLeft :: Monad m => Execution b m () -> Execution b m ()
untilNoAlternativesLeft action =
  loop
  where
    loop =
      anyAlternativesLeft >>=
      bool (return ()) (action >> loop)

{-# INLINE ifAnyAlternativesLeft #-}
ifAnyAlternativesLeft :: MonadPlus m => Execution b m a -> Execution b m a
ifAnyAlternativesLeft action =
  anyAlternativesLeft >>=
  bool (mzero) action

anyAlternativesLeft :: Monad m => Execution b m Bool
anyAlternativesLeft =
  gets $
  \(alternatives, _) -> not (null alternatives)

getResults :: Monad m => Execution b m [b]
getResults =
  gets $
  \(_, results) -> results

{-# INLINE process #-}
process :: MonadPlus m => m () -> Execution b m ()
process skip =
  inline skipMany (ifAnyAlternativesLeft (inline tryAlternatives skip))
