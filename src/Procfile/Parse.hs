module Procfile.Parse where

import           Control.Applicative
import           Control.Monad
import           Text.Parser.LookAhead
import           Text.Trifecta         hiding (spaces)

import           Procfile.Types        (App (App), Procfile)

procfile :: Parser Procfile
procfile = proc `sepEndBy` (void newline <|> eof)

proc :: Parser App
proc = do
    n <- name
    _ <- spaces
    c <- cmd
    return $ App n c

name :: Parser String
name = some (alphaNum <|> char '_') <* char ':'
  <?> "name"

cmd :: Parser String
cmd = manyTill anyChar (eof <|> lookAhead (void newline))
  <?> "cmd"

spaces :: CharParsing m => m ()
spaces = skipMany (char ' ') <?> "white space excluding newline"
