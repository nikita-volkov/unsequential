module Unsequential.Execution
where

import Unsequential.Prelude


type Execution b m a =
  StateT ([m b], [b]) m a

run :: Monad m => Execution b m () -> [m b] -> m ([m b], [b])
run execution alternatives =
  execStateT execution (alternatives, [])

tryAlternatives :: MonadPlus m => m () -> Execution b m ()
tryAlternatives skip =
  modifyM loop
  where
    loop (alternatives, results) =
      case alternatives of
        alternativesHead : alternativesTail ->
          mplus tryHead tryTail
          where
            tryHead =
              fmap (\result -> (alternativesTail, result : results)) $
              alternativesHead
            tryTail =
              fmap (\(alternatives, results) -> (alternativesHead : alternatives, results)) $
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

process :: MonadPlus m => m () -> Execution b m ()
process skip =
  skipMany (ifAnyAlternativesLeft (tryAlternatives skip))
