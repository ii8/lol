{-# OPTIONS_GHC -fno-warn-orphans #-}

module Import.Charge (ChargeId(..)) where

import Import.Base
import Web.Stripe.Charge

instance PersistField ChargeId where
    toPersistValue (ChargeId s) = PersistText s
    fromPersistValue s = case fromPersistValueText s of
        Right p -> Right $ ChargeId p
        Left p -> Left p

instance PersistFieldSql ChargeId where
    sqlType _ = SqlString
