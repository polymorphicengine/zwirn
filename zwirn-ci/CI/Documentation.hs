module CI.Documentation where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Text.Lazy as T (pack)
import Data.Text.Lazy.Encoding as T
import System.Directory.OsPath
import System.File.OsPath as F
import System.OsPath hiding (unpack)
import Zwirn.Language.Builtin.Prelude
import Zwirn.Language.Environment
import Zwirn.Language.Pretty

generateDocumentation :: IO ()
generateDocumentation = do
  curr <- getCurrentDirectory
  path <- (curr <>) <$> encodeUtf "/zwirn-docs.md"
  decoded <- decodeUtf path
  print ("Generating documentation in: " ++ decoded)
  F.writeFile path (T.encodeUtf8 $ T.pack documentation)

documentation :: String
documentation =
  "# Zwirn\n\n"
    ++ intercalate
      "\n\n"
      [ documentSection "Core Functions" coreFunctions,
        documentSection "Signals" signals,
        documentSection "Randomness" randomFunctions,
        documentSection "Manipulating Time" timeFunctions,
        documentSection "Manipulating Structure" structureFunctions,
        documentSection "Conditionals" conditionalFunctions,
        documentSection "Cords / Layers" cordFunctions,
        documentSection "Functions on Maps" mapFunctions,
        documentSection "Stateful Functions" mapFunctions
      ]

documentSection :: String -> Map.Map Text AnnotatedExpression -> String
documentSection header ma = "## " ++ header ++ "\n\n" ++ intercalate "\n\n" (map documentOne as)
  where
    as = Map.toList ma

documentOne :: (Text, AnnotatedExpression) -> String
documentOne (name, Annotated _ ty (Just desc)) = "```" ++ unpack name ++ " :: " ++ ppscheme ty ++ "```\n\n" ++ unpack desc
documentOne (name, Annotated _ ty Nothing) = "```" ++ unpack name ++ " :: " ++ ppscheme ty ++ "```"
