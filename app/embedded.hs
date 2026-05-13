-- | Hadolint Embedded
--
-- This is a special version of Hadolint designed to be compiled to a WASM artifact
-- and embedded in a website.
-- It will read the content of the Dockerfile from a textarea element with id
-- `dockerfile__code` and output the linting result as JSON string to stdout in
-- the WASM runtime environment.

import Data.Default
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC.Wasm.Prim
import qualified Hadolint
import qualified Hadolint.Formatter.Format as Format
import qualified Language.Docker as Docker
import Language.Docker.Parser (DockerfileError, Error)
import Language.Docker.Syntax (Dockerfile)


main :: IO ()
main = error "ignore this"


-- JavaScript imports --
foreign import javascript unsafe "document.getElementById($1)"
  js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "document.getElementById($1).value"
  js_document_getElementValue :: JSString -> IO JSString

foreign import javascript unsafe "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "wrapper"
  asEventListener :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.textContent = $2;"
  js_setText :: JSVal -> JSString -> IO ()


-- JavaScript exports --
foreign export javascript "setup" setup :: IO ()
setup :: IO ()
setup = do
  lintInput <- js_document_getElementById (toJSString "lint")
  lintInputCallback <- asEventListener onLint
  js_addEventListener lintInput (toJSString "click") lintInputCallback

  versionOutput <- js_document_getElementById (toJSString "version")
  js_setText versionOutput $ toJSString Hadolint.getVersion

foreign export javascript "lint" lint :: IO ()
lint :: IO ()
lint = do
  dftext <- js_document_getElementValue (toJSString "dockerfile__code")
  showResult $ doLint . Docker.parseText $ jsStringToText dftext


-- Internal functions --

-- event handler - just ignores the event object
onLint :: JSVal -> IO ()
onLint _ = lint

doLint :: Either Error Dockerfile -> Format.Result Text.Text DockerfileError
doLint res =
  case res of
    Left err -> Format.Result file ( Seq.singleton err ) mempty
    Right ast -> Format.Result file mempty ( analyze ast )
  where
    file = Text.pack "Dockerfile"
    analyze ast = Seq.unstableSort ( Hadolint.analyze def ast )

showResult :: Format.Result Text.Text DockerfileError -> IO ()
showResult result = do
  Hadolint.printResults Hadolint.Json True Nothing [ result ]

jsStringToText :: JSString -> Text.Text
jsStringToText = Text.pack . fromJSString
