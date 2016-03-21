module Procfile.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Text.Parser.LookAhead
import           Text.Trifecta hiding (spaces)

import           Procfile.Types        (App (App), Procfile)

procfile :: Parser Procfile
procfile = proc `sepEndBy` (void newline <|> eof)

proc :: Parser App
proc = do
    n <- name <* spaces
    envs <- many (try env) <* spaces
    c <- cmd <* spaces
    envs' <- try $ many (try env) <* spaces
    a <- try args <* spaces
    envs'' <- try $ many (try env) <* spaces
    return $ App n c a (envs <> envs' <> envs'')

name :: Parser String
name = some (alphaNum <|> char '_') <* char ':'
  <?> "name"

cmd :: Parser String
cmd = many (noneOf " \n")
  <?> "cmd"

env :: Parser (String,String)
env = (,) <$> key <*> (eq' >> value)
    <?> "env"
  where
    key = some (upper <|> char '_') <?> "env key"
    eq' = char '=' <?> "env eq"
    value = val <?> "env val"

val :: Parser String
val = stringLiteral <|> stringLiteral' <|> manyTill1 (noneOf " \n") (void space <|> eof)
  <?> "val"

args :: Parser [String]
args = manyTill (strip arg) (void (lookAhead (try env)) <|> eof <|> void (lookAhead newline))
  <?> "args"

arg :: Parser String
arg = some (noneOf " =\n") <* notFollowedBy (char '=')
  <?> "arg"

strip :: CharParsing f => f a -> f a
strip p = spaces *> p <* spaces

manyTill1 :: (Monad m, Alternative m) => m a -> m end -> m [a]
manyTill1 p end = do
  x <- p
  xs <- manyTill p end
  return (x:xs)

spaces :: CharParsing m => m ()
spaces = skipMany (char ' ') <?> "white space excluding newline"
