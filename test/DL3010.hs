module DL3010 (tests) where

import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "COPY rules" $ do
    it "use add" $ ruleCatches "DL3010" "COPY packaged-app.tar /usr/src/app"
    it "use not add" $ ruleCatchesNot "DL3010" "COPY package.json /usr/src/app"
    it "ignore: copy from previous stage" $ ruleCatchesNot "DL3010" "COPY --from=builder /usr/local/share/some.tar /opt/some.tar"
