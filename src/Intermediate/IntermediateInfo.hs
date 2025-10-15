{-# LANGUAGE InstanceSigs #-}
module IntermediateInfo
  ( IntermediateInfo(..)
  , EntityInfo(..)
  , entitiesFromInfo
  )
where

import           Env
import qualified Data.Map                      as M

import           Txt
import           AbsFlow

data IntermediateInfo = IntermediateInfo
  { entity_info :: M.Map EntityName EntityInfo
  , entity_txt :: M.Map EntityName Txt
  }
 deriving (Eq, Show)

instance Semigroup IntermediateInfo where
  (<>) :: IntermediateInfo -> IntermediateInfo -> IntermediateInfo
  (<>) a b =
    IntermediateInfo
      { entity_info = M.unionWith (<>) (entity_info a) (entity_info b)
      , entity_txt = M.union (entity_txt a) (entity_txt b) }

instance Monoid IntermediateInfo where
  mempty :: IntermediateInfo
  mempty = IntermediateInfo {entity_info = M.empty, entity_txt = M.empty}



newtype EntityInfo = EntityInfo { width :: (Width,Width) }
  deriving (Eq, Show)

instance Semigroup EntityInfo where
  (<>) :: EntityInfo -> EntityInfo -> EntityInfo
  (<>) a b =
    EntityInfo
      { width = let (wla,wra) = width a
                    (wlb,wrb) = width b
                in (max wla wlb, max wra wrb)
      }

instance Monoid EntityInfo where
  mempty = EntityInfo {width = (0,0) }


entitiesFromInfo :: IntermediateInfo -> [EntityName]
entitiesFromInfo = M.keys . entity_info
