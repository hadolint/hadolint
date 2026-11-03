module Hadolint.Rule.DL3064 (rule) where

import Data.Text qualified as Text
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

    check (Arg name _) = not (isSensitive name)
    check (Env pairs) = not (any (isSensitive . fst) pairs)
    check _ = True
{-# INLINEABLE dl3064 #-}

isSensitive :: Text.Text -> Bool
isSensitive name =
  not (isFileReference name)
    && ( Text.toUpper name `elem` knownSensitiveNames
           || any (`Text.isInfixOf` Text.toLower name) suspiciousSubstrings
       )

-- | Variables ending in @_FILE@ hold the path to a secret (e.g. Docker
-- secrets such as @POSTGRES_PASSWORD_FILE@), not the secret itself.
isFileReference :: Text.Text -> Bool
isFileReference name = "_FILE" `Text.isSuffixOf` Text.toUpper name

knownSensitiveNames :: [Text.Text]
knownSensitiveNames =
  [ "ACCESS_TOKEN",
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
    "OKTA_CLIENT_TOKEN",
    "OKTA_OAUTH2_CLIENTSECRET",
    "OPENAI_API_KEY",
    "OS_PASSWORD",
    "OS_USERNAME",
    "POSTGRES_PASSWORD",
    "SLACK_TOKEN",
    "STRIPE_API_KEY",
    "VAULT_CLIENT_KEY",
    "VAULT_TOKEN"
  ]

suspiciousSubstrings :: [Text.Text]
suspiciousSubstrings =
  [ "api_key",
    "client_key",
    "password",
    "private_key",
    "privatekey",
    "secret",
    "token",
    "username"
  ]
