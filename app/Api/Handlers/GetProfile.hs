module Handlers.GetProfile
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Database.Queries.Profile
import Types (WebMonad)

newtype Response =
  Response Profile

instance ToJSON Response where
  toJSON (Response Profile {..}) =
    object
      [ "user" .=
        object
          [ "id" .= profileId
          , "first_name" .= profileUserFirstName
          , "last_name" .= profileUserLastName
          , "email" .= profileUserEmail
          ]
      , "company" .=
        object ["id" .= profileCompanyId, "name" .= profileCompanyName]
      ]

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  maybeProfile <- lift $ getProfile (userId user)
  case maybeProfile of
    Just profile -> json $ Response profile
    Nothing -> status notFound404
