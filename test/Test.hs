{-# LANGUAGE CPP #-}

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import qualified Test.LSP as LSP
import qualified Test.SrcLoc as SrcLoc
#if defined(wasm32_HOST_ARCH)
import qualified Test.WASM as WASM
#endif
import Test.Tasty
import Test.Tasty.Options
import qualified Test.Model as Model
import qualified Test.ModelMonad as ModelMonad
import qualified Test.Indexer.Invariants as IndexerInvariants

-- Define the custom option
newtype AlsPathOption = AlsPathOption FilePath
  deriving (Show, Typeable)

instance IsOption AlsPathOption where
  defaultValue = AlsPathOption "als"
  parseValue = Just . AlsPathOption
  optionName = return "als-path"
  optionHelp = return "Path to the als executable"

main :: IO ()
main = do
  let opts = [Option (Proxy :: Proxy AlsPathOption)]
      ingredients = includingOptions opts : defaultIngredients
  defaultMainWithIngredients ingredients =<< tests

tests :: IO TestTree
tests = do
  indexerInvariantsTest <- IndexerInvariants.tests
  return $ askOption $ \(AlsPathOption alsPath) ->
    testGroup
      "Tests"
      [ SrcLoc.tests,
        LSP.tests alsPath,
        Model.tests,
        ModelMonad.tests,
        indexerInvariantsTest
#if defined(wasm32_HOST_ARCH)
      , WASM.tests alsPath
#endif
      ]