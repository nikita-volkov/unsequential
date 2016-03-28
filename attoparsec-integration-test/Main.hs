module Main where

import Rebase.Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Unsequential
import Interspersed
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit


main =
  defaultMain $
  testGroup "All tests"
  [
    testCase "Primitive" $
    let
      input =
        "{\"year\" : \"2001\", \"month\": \"1\", \"day\": \"2\"}"
      result =
        parseOnly dateOutOfAnyObject input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
    ,
    testCase "In different order" $
    let
      input =
        "{\"month\": \"1\", \"day\": \"2\", \"year\" : \"2001\"}"
      result =
        parseOnly dateOutOfAnyObject input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
    ,
    testCase "With redundant fields" $
    let
      input =
        "{\"redundant1\": \"4\", \"month\": \"1\", \"redundant2\": \"3\", \"day\": \"2\", \"year\" : \"2001\"}"
      result =
        parseOnly dateOutOfAnyObject input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
    ,
    testCase "With trailing fields" $
    let
      input =
        "{\"month\": \"1\", \"day\": \"2\", \"year\" : \"2001\", \"trailing\": \"3\"}"
      result =
        parseOnly dateOutOfAnyObject input
      in assertEqual (show result) (Right ("2001", "1", "2")) result
  ]


-- * Parsers
-------------------------

dateOutOfAnyObject :: Parser (ByteString, ByteString, ByteString)
dateOutOfAnyObject =
  objectWithUnsequentialRows $
  (,,) <$>
  unsequential (interspersed (objectRow (== "year") stringLit)) <*>
  unsequential (interspersed (objectRow (== "month") stringLit)) <*>
  unsequential (interspersed (objectRow (== "day") stringLit))

objectWithUnsequentialRows :: Unsequential (Interspersed Parser) a -> Parser a
objectWithUnsequentialRows unsequentialRows =
  object rows
  where
    rows =
      runInterspersed (runUnsequential unsequentialRows skip <* skipMany skip) comma
      where
        skip =
          interspersed $
          objectRow (const True) stringLit $> ()

-- |
-- Given a rows parser produces a parser of the object.
object :: Parser a -> Parser a
object rows =
  char '{' *> skipSpace *> rows <* skipSpace <* char '}'

-- |
-- Object row parser, which tests the key with a predicate and parses the value.
objectRow :: (ByteString -> Bool) -> Parser a -> Parser a
objectRow keyPredicate valueParser =
  do
    key <- stringLit
    guard (keyPredicate key)
    skipSpace
    char ':'
    skipSpace
    valueParser 

-- |
-- Note: this parser does not satisfy the JSON standards,
-- but suffices the purposes of the test.
stringLit :: Parser ByteString
stringLit =
  char '"' *> takeWhile (/= '"') <* char '"'

comma :: Parser ()
comma =
  skipSpace *> char ',' *> skipSpace
