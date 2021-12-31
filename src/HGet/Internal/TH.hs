module HGet.Internal.TH where

import Control.Lens (DefName (MethodName), FieldNamer, LensRules, declareLensesWith)
import Control.Lens.Internal.FieldTH (LensRules (..), makeFieldOptics)
import Data.Char (isUpper, toLower)
import Data.Maybe (maybeToList)
import Language.Haskell.TH (mkName, nameBase, DecsQ, Name)

simpleRule :: LensRules
simpleRule =
  LensRules
    { _simpleLenses = True,
      _generateSigs = True,
      _generateClasses = True,
      _allowIsos = False,
      _allowUpdates = True,
      _lazyPatterns = False,
      _classyLenses = const Nothing,
      _fieldToDef = simpleNamer
    }

simpleNamer :: FieldNamer
simpleNamer _ _ field = maybeToList $ do
  let fieldName = dropWhile (not . isUpper) (nameBase field)
  method <- computeMethod fieldName
  let cls = "Has" ++ fieldName
  return (MethodName (mkName cls) (mkName method))
  where
    computeMethod (x : xs) | isUpper x = Just (toLower x : xs)
    computeMethod _ = Nothing

simpleFieldRules :: DecsQ -> DecsQ
simpleFieldRules = declareLensesWith simpleRule

makeSimple :: Name -> DecsQ
makeSimple = makeFieldOptics simpleRule