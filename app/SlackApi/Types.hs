module Types where

import qualified RIO.Text.Lazy as LT

import Web.Scotty.Trans

import Common.App (AppT)

type WebHandler = ActionT LT.Text AppT

type WebRoutes = ScottyT LT.Text AppT
