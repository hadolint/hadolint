module Hadolint.Rule.DL3064 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import Hadolint.Shell (ParsedShell)
import Language.Docker.Syntax


rule :: Rule ParsedShell
rule = dl3064 <> onbuild dl3064
{-# INLINEABLE rule #-}

dl3064 :: Rule ParsedShell
dl3064 = simpleRule code severity message check
  where
    code = "DL3064"
    severity = DLWarningC
    message = "Potentially sensitive data should not be used in the `ARG` or `ENV` commands"

    check (Arg name _) =
      Text.toUpper name `notElem` knownSensitiveNames
        && none (`Text.isInfixOf` Text.toLower name) suspiciousSubstrings
    check (Env pairs) =
      none (\s -> (Text.toUpper . fst) s `elem` knownSensitiveNames) pairs
        && none (\s -> any ((s `Text.isInfixOf`) . Text.toLower . fst) pairs) suspiciousSubstrings
    check _ = True
{-# INLINEABLE dl3064 #-}

none :: (a -> Bool) -> [a] -> Bool
none p xs = not ( any p xs )

knownSensitiveNames :: [Text.Text]
knownSensitiveNames =
  [
    "ACCESS_TOKEN",
    "APPLICATION_KEY",
    "APP_SECRET",
    "AUTH_TOKEN",
    "AWS_ACCESS_KEY_ID",
    "AWS_SECRET_ACCESS_KEY",
    "BITTREX_API_KEY",
    "BITTREX_API_SECRET",
    "CF_PASSWORD",
    "CF_USERNAME",
    "CIRCLE_TOKEN",
    "CI_DEPLOY_PASSWORD",
    "CI_DEPLOY_USER",
    "DOCKERHUB_PASSWORD",
    "DOCKER_EMAIL",
    "DOCKER_PASSWORD",
    "DOCKER_USERNAME",
    "FACEBOOK_ACCESS_TOKEN",
    "FACEBOOK_APP_ID",
    "FACEBOOK_APP_SECRET",
    "FIREBASE_API_TOKEN",
    "FIREBASE_TOKEN",
    "FOSSA_API_KEY",
    "GH_ENTERPRISE_TOKEN",
    "GH_TOKEN",
    "GITHUB_ENTERPRISE_TOKEN",
    "GITHUB_TOKEN",
    "HEROKU_API_KEY",
    "HEROKU_API_USER",
    "NPM_AUTH_TOKEN",
    "NPM_TOKEN",
    "OKTA_AUTHN_GROUPID",
    "OKTA_CLIENT_ORGURL",
    "OKTA_CLIENT_TOKEN",
    "OKTA_OAUTH2_CLIENTID",
    "OKTA_OAUTH2_CLIENTSECRET",
    "OPENAI_API_KEY",
    "OS_PASSWORD",
    "OS_USERNAME",
    "POSTGRES_PASSWORD",
    "SLACK_TOKEN",
    "STRIPE_API_KEY",
    "STRIPE_DEVICE_NAME",
    "TRAVIS_OS_NAME",
    "TRAVIS_SECURE_ENV_VARS",
    "TRAVIS_SUDO",
    "VAULT_CLIENT_KEY",
    "VAULT_TOKEN"
  ]

suspiciousSubstrings :: [Text.Text]
suspiciousSubstrings =
  [
    "api_key",
    "client_key",
    "password",
    "private",
    "secret",
    "token",
    "username"
  ]
