{- Copyright (c) Jaap Weel 2007.  Permission is hereby granted, free
of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the
Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.  THE
SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. -}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
module Text.CSV.Aeson (CSV
                 , Record
                 , Field
                 , csv
                 ) where

import Text.ParserCombinators.Parsec
import Data.List (intersperse)
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import Data.Text.Encoding qualified as TSE
import Data.Text.Lazy.Encoding qualified as TLE
import Data.ByteString.Lazy.UTF8 qualified as BLU

-- | A CSV file is a series of records. According to the RFC, the
-- records all have to have the same length. As an extension, I
-- allow variable length records.
type CSV = [Record]

-- | A record is a series of fields
type Record = [Field]

-- | A field is an aeson Value
type Field = Value

-- | A Parsec parser for parsing CSV files
csv :: Parser CSV
csv = do x <- record `sepEndBy` many1 (oneOf "\n\r")
         eof
         pure x

quotedField :: Parser Value
quotedField = do
  char '"'
  innerStr <- many $ noneOf "\""
  char '"'
  pure $ String $ T.pack ( "\"" ++ innerStr ++ "\"")

unquotedField :: Parser Value
unquotedField = do
  field <- many $ noneOf ","
  case decode @Value $ BLU.fromString field of
    Just x -> pure $ x
    Nothing -> unexpected "can't parse json field"


record :: Parser [Value]
record = (try quotedField <|> unquotedField) `sepBy` (char ',')
