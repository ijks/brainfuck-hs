module Program
    ( Command(..)
    , Program
    -- , program
    , parse
    ) where

import Control.Arrow (left)

import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.String

data Command
    = GoLeft
    | GoRight
    | Increment
    | Decrement
    | PutChar
    | GetChar
    | Loop Program
    deriving (Eq, Show)

type Program = [Command]

command :: Parser Command
command = choice
    [ GoLeft <$ char '<'
    , GoRight <$ char '>'
    , Increment <$ char '+'
    , Decrement <$ char '-'
    , PutChar <$ char '.'
    , GetChar <$ char ','
    , loop
    ]

loop :: Parser Command
loop = Loop <$> between (char '[') (char ']') program

comment :: Parser ()
comment = skipMany (noneOf ['<', '>', '+', '-', '.', ',', '[', ']'])

program :: Parser Program
program = many $ between comment comment command

parse :: String -> Either String Program
parse input = left parseErrorPretty $ Megaparsec.parse program "" input
