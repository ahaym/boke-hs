{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.BokeHS.CDS where

import Data.Text
import GHC.TypeLits
import GHC.Records
import Data.Aeson

import Graphics.BokeHS.Prim

data Key (name :: Symbol) where
    Key :: Key name

fieldName :: KnownSymbol n => Key n -> Field
fieldName = Field . pack . symbolVal

class (ToJSON value, KnownSymbol name) => HasColumn row name value | row name -> value where
    getValue :: Key name -> row -> value
    default getValue :: (HasField name row value) => Key name -> row -> value
    getValue _ = getField @name

instance {-# OVERLAPPABLE #-} (ToJSON v, KnownSymbol n, HasField n r v) 
    => HasColumn r n v

data Name r where
    Name :: (ToJSON v, KnownSymbol n, HasColumn r n v) => Key n -> Name r

toColumn :: forall r. [r] -> Name r -> (Field, Value)
toColumn xs (Name k) = (fieldName k, toJSON (toJSON . getValue k <$> xs))

allTheStuff :: [r] -> [Name r] -> [(Field, Value)]
allTheStuff rows cols = toColumn rows <$> cols
