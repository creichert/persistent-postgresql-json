
-- | A simple json/jsonb persistent type

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.Persist.Postgresql.Json
       (
         Jsonb(..)
       , Json(..)
       , (@>.)
       , (<@.)
       ) where

import           Data.Aeson
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding   as TE
import           Data.Time            ()
import           Database.Persist
import           Database.Persist.Sql


-- | Postgresql JSON type
newtype Json
  = Json A.Value
    deriving (
               Show
             , Eq
             , FromJSON
             , ToJSON )


instance PersistField Json where
  toPersistValue v = PersistText $ TE.decodeUtf8 $ BSL.toStrict $ A.encode v
  fromPersistValue (PersistByteString v)
    = case A.decode (BSL.fromStrict v) of
        Nothing -> Left "Invalid JSON"
        Just j  -> Right j
  fromPersistValue _ = Left "Not PersistText"

instance PersistFieldSql Json where
   sqlType _ = SqlOther "JSON"

-- | Postgresql JSONB type
newtype Jsonb
  = Jsonb A.Value
    deriving (
               Show
             , Eq
             , FromJSON
             , ToJSON )

instance PersistField Jsonb where
  toPersistValue v = PersistText $ TE.decodeUtf8 $ BSL.toStrict $ A.encode v
  fromPersistValue (PersistByteString v)
    = case A.decode (BSL.fromStrict v) of
        Nothing -> Left "Invalid jsonb"
        Just j  -> Right j
  fromPersistValue _ = Left "Invalid PersistValue for JSONB. PersistByteString required."

instance PersistFieldSql Jsonb where
   sqlType _ = SqlOther "JSONB"

infixr 6 @>., <@.

-- | Does the left JSON value contain within it the right value?
--
-- > '{"a":1, "b":2}'::jsonb @> '{"b":2}'::jsonb
--
-- See tests for example usage
(@>.) :: EntityField record Jsonb -> A.Value -> Filter record
(@>.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "@>"
    jval = Left $ Jsonb val

-- | Does the right JSON value contain within it the left value?
--
-- > '{"a":1, "b":2}'::jsonb <@ '{"b":2}'::jsonb
--
-- See tests for example usage
(<@.) :: EntityField record Jsonb -> A.Value -> Filter record
(<@.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "<@"
    jval = Left $ Jsonb val


{-

FIX Some operators cannot yet be defined due to having a ? which
is used in persistent's query interpolation syntax.

(?.) :: EntityField record Jsonb -> A.Value -> Filter record
(?.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter " ? "
    jval = Left $ Jsonb val


(?|.) :: EntityField record Jsonb -> [A.Value] -> Filter record
(?|.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "\?| "
    jval = Right $ fmap Jsonb val


(?&.) :: EntityField record Jsonb -> [A.Value] -> Filter record
(?&.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter " \"?&\" "
    jval = Right $ fmap Jsonb val


-- * BackendSpecificUpdate not supported in persistent-postgresql.

-- postgresql 9.5 only
f -. a = Update f a (BackendSpecificUpdate "-")

-}


{- Some helpful orphan instances

instance PersistField A.Value where
    toPersistValue (A.Object obj) =
      PersistMap . fmap (\(k,v) -> (k, toPersistValue v)) . M.toList $ obj
    toPersistValue (A.Array arr)  = PersistList . fmap toPersistValue . V.toList $ arr
    toPersistValue (A.String text)= PersistText text
    toPersistValue (A.Number num) = toPersistValue num
    toPersistValue (A.Bool bool)  = PersistBool bool
    toPersistValue (A.Null)       = PersistNull

    fromPersistValue (PersistText text)     = Right $ A.String text
    fromPersistValue (PersistByteString bs) = Right . A.String . E.decodeUtf8 $ bs
    fromPersistValue (PersistInt64 int) = Right $ A.Number $ fromIntegral int
    fromPersistValue (PersistDouble doub) = Right . A.Number $ fromRational $ toRational doub
    fromPersistValue (PersistBool bool)     = Right $ A.Bool bool
    fromPersistValue (PersistDay day)       = Right . A.String . pack . show $ day
    fromPersistValue (PersistTimeOfDay time)= Right . A.String . pack . show $ time
    fromPersistValue (PersistUTCTime utc)   = Right . A.String . pack . show $ utc
    fromPersistValue (PersistNull)          = Right A.Null
    fromPersistValue (PersistMap listPairs) =
      let parsePair (k,v) = case fromPersistValue v of
                              Right s -> Right (k,s)
                              Left m  -> Left m
      in (A.Object . M.fromList) `liftM` mapM parsePair listPairs
    -- fromPersistValue (PersistInt64 int) = Right . A.Number . fromIntegral $ int
    -- fromPersistValue (PersistInt64 int)     = Right . A.Number . N.I . fromIntegral $ int
    -- fromPersistValue (PersistObjectId bs)   = Right . String . E.decodeUtf8 $ bs
    --- fromPersistValue (PersistList vals)     = (Array . V.fromList) `liftM` (mapM (fromPersistValue) vals)



instance PersistField Scientific where
    toPersistValue = PersistRational . toRational
    fromPersistValue (PersistRational r) = Right $ fromRational r
    fromPersistValue (PersistDouble d) = Right $ fromFloatDigits d
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ T.pack
        "PersistField Scientific: Expected Scientific, received: " <> T.pack (show x)

instance PersistFieldSql Scientific where
  sqlType _ = SqlNumeric 32 20

-}
