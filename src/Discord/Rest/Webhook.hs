{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Webhook API interactions
module Discord.Rest.Webhook
  ( WebhookRequest(..)
  , CreateWebhookOpts(..)
  , ModifyWebhookOpts(..)
  ) where

import Data.Aeson
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Discord.Rest.Prelude
import Discord.Types

instance Request (WebhookRequest a) where
  majorRoute = webhookMajorRoute
  jsonRequest = webhookJsonRequest

-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data WebhookRequest a where
  CreateWebhook :: ChannelId -> CreateWebhookOpts -> WebhookRequest Webhook
  GetChannelWebhooks :: ChannelId -> WebhookRequest [Webhook]
  GetGuildWebhooks :: GuildId -> WebhookRequest [Webhook]
  GetWebhook :: WebhookId -> WebhookRequest Webhook
  GetWebhookWithToken :: WebhookId -> T.Text -> WebhookRequest Webhook
  ModifyWebhook :: WebhookId -> ModifyWebhookOpts
                                      -> WebhookRequest Webhook
  ModifyWebhookWithToken :: WebhookId -> T.Text -> ModifyWebhookOpts
                                      -> WebhookRequest Webhook
  DeleteWebhook :: WebhookId -> WebhookRequest Webhook
  DeleteWebhookWithToken :: WebhookId -> T.Text -> WebhookRequest Webhook

data ModifyWebhookOpts = ModifyWebhookOpts
  { modifyWebhookOptsName          :: Maybe T.Text
  , modifyWebhookOptsAvatar        :: Maybe T.Text
  , modifyWebhookOptsChannelId     :: Maybe ChannelId
  }

instance ToJSON ModifyWebhookOpts where
  toJSON ModifyWebhookOpts{..} = object [(name, val) | (name, Just val) <-
                         [("channel_id",   toJSON <$> modifyWebhookOptsChannelId),
                          ("name",   toJSON <$> modifyWebhookOptsName),
                          ("avatar",  toJSON <$> modifyWebhookOptsAvatar) ] ]

data CreateWebhookOpts = CreateWebhookOpts
  { createWebhookOptsName          :: T.Text
  , createWebhookOptsAvatar        :: Maybe T.Text
  }

instance ToJSON CreateWebhookOpts where
  toJSON CreateWebhookOpts{..} = object [(name, val) | (name, Just val) <-
                         [("name",   toJSON <$> Just createWebhookOptsName),
                          ("avatar",  toJSON <$> createWebhookOptsAvatar) ] ]

webhookMajorRoute :: WebhookRequest a -> String
webhookMajorRoute ch = case ch of
  (CreateWebhook c _) ->            "aaaaaahook " <> show c
  (GetChannelWebhooks c) ->         "aaaaaahook " <> show c
  (GetGuildWebhooks g) ->           "aaaaaahook " <> show g
  (GetWebhook w) ->                 "aaaaaahook " <> show w
  (GetWebhookWithToken w _) ->      "getwebhook " <> show w
  (ModifyWebhook w _) ->            "modifyhook " <> show w
  (ModifyWebhookWithToken w _ _) -> "modifyhook " <> show w
  (DeleteWebhook w) ->              "deletehook " <> show w
  (DeleteWebhookWithToken w _) ->   "deletehook " <> show w

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

webhookJsonRequest :: WebhookRequest r -> JsonRequest
webhookJsonRequest ch = case ch of
  (CreateWebhook channel patch) ->
    let body = pure (R.ReqBodyJson patch)
    in Post (baseUrl /: "channels" // channel /: "webhooks") body  mempty

  (GetChannelWebhooks c) ->
    Get (baseUrl /: "channels" // c /: "webhooks")  mempty

  (GetGuildWebhooks g) -> 
    Get (baseUrl /: "guilds" // g /: "webhooks")  mempty

  (GetWebhook w) ->
    Get (baseUrl /: "webhooks" // w)  mempty

  (GetWebhookWithToken w t) ->
    Get (baseUrl /: "webhooks" // w // t)  mempty

  (ModifyWebhook w patch) ->
    Patch (baseUrl /: "webhooks" // w) (R.ReqBodyJson patch)  mempty

  (ModifyWebhookWithToken w t p) ->
    Patch (baseUrl /: "webhooks" // w // t) (R.ReqBodyJson p)  mempty

  (DeleteWebhook w) ->
    Delete (baseUrl /: "webhooks" // w)  mempty

  (DeleteWebhookWithToken w t) ->
    Delete (baseUrl /: "webhooks" // w // t)  mempty