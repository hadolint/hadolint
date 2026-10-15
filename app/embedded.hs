-- | Hadolint Embedded
--
-- A build of Hadolint compiled to a WASM artifact and embedded in a JavaScript
-- host. The exports are plain functions: the host passes the Dockerfile in and
-- gets the JSON report back. Nothing here reaches into the DOM, so the module
-- can be driven from any JavaScript environment that can provide the JSFFI
-- imports, in a browser or in Node.
--
-- The online linter at hadolint.github.io is one such host — it reads the
-- textarea itself and feeds the value to @lint@.

import Data.Default
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC.Wasm.Prim
import qualified Hadolint
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Formatter.Json as FormatJson
import qualified Language.Docker as Docker
import Language.Docker.Parser (DockerfileError, Error)
import Language.Docker.Syntax (Dockerfile)


main :: IO ()
main = error "ignore this"


-- JavaScript exports --

-- | Lint a Dockerfile and return the JSON report — the same document
-- @hadolint -f json@ writes. Nothing here performs IO; the JSFFI marshalling
-- just requires the exports to be in IO.
foreign export javascript "lint" lint :: JSString -> IO JSString
lint input =
  pure . toJSString . Text.unpack . FormatJson.render $
    [ doLint (Docker.parseText (jsStringToText input)) ]

-- | The version string, for hosts that want to display it.
foreign export javascript "version" version :: IO JSString
version = pure (toJSString Hadolint.getVersion)


-- Internal functions --

doLint :: Either Error Dockerfile -> Format.Result Text.Text DockerfileError
doLint res =
  case res of
    Left err -> Format.Result file ( Seq.singleton err ) mempty
    Right ast -> Format.Result file mempty ( analyze ast )
  where
    file = Text.pack "Dockerfile"
    analyze ast = Seq.unstableSort ( Hadolint.analyze def ast )

jsStringToText :: JSString -> Text.Text
jsStringToText = Text.pack . fromJSString
