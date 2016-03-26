module Disorderly.Execution
where

import Disorderly.Prelude


type Execution b m a =
  StateT ([m b], [b]) m a

run :: Monad m => Execution b m () -> [m b] -> m ([m b], [b])
run execution alternatives =
  execStateT execution (alternatives, [])

tryAlternatives :: MonadPlus m => Execution b m ()
tryAlternatives =
  modifyM loop
  where
    loop (alternatives, results) =
      case alternatives of
        alternativesHead : alternativesTail ->
          tryHead <|> tryTail
          where
            tryHead =
              fmap (\result -> (alternativesTail, result : results)) $
              alternativesHead
            tryTail =
              fmap (\(alternatives, results) -> (alternativesHead : alternatives, results)) $
              loop (alternativesTail, results)
        _ ->
          pure (alternatives, results)

untilNoAlternativesIsLeft :: Monad m => Execution b m () -> Execution b m ()
untilNoAlternativesIsLeft action =
  loop
  where
    loop =
      anyAlternativesLeft >>= bool (return ()) (action >> loop)

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
  untilNoAlternativesIsLeft (tryAlternatives <|> lift skip)
