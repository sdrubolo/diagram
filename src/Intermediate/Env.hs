module Env
  ( involvedEntities
  , addEntity
  , nextEntity
  , prevEntity
  , empty
  , addToActiveEntities
  , deleteFromActiveEntities
  , getActiveEntities
  , Env(..)
  , IntermediateT(..)
  , EntityData(..)
  , Entities
  )
where

import qualified Data.Vector                   as V
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State.Lazy
import           Control.Monad.Except

import           Font
import           Debug.Trace
import           Txt

empty = EntityData V.empty M.empty S.empty

type EntityName = String
type Entities = V.Vector EntityName

data EntityData = EntityData
    { all_entities :: Entities
    , entity_to_index :: M.Map EntityName Int
    , scope :: S.Set EntityName }

data Env a = Env
  { font :: FontTable a, entities :: EntityData }

type IntermediateT a  = ExceptT String (State (Env a))

-- need to improve this part
addEntity :: EntityName -> IntermediateT a ()
addEntity entity = modify
  (\env ->
    let entities_data = entities env
        entity_list   = all_entities entities_data
        list_length   = V.length entity_list
        indexing      = entity_to_index entities_data
    in  case entity_list V.!? M.findWithDefault (list_length + 1) entity indexing of
          Nothing -> env
            { entities = entities_data { all_entities    = V.snoc entity_list entity
                                       , entity_to_index = M.insert entity list_length indexing
                                       }
            }
          Just _ -> env
  )

modifyScope :: (t -> S.Set EntityName -> S.Set EntityName) -> t -> IntermediateT a ()
modifyScope fn entity = modify
  (\env ->
    let entities_data  = entities env
        activeEntities = scope entities_data
    in  env { entities = entities_data { scope = fn entity activeEntities } }
  )

addToActiveEntities :: EntityName -> IntermediateT a ()
addToActiveEntities = modifyScope S.insert

deleteFromActiveEntities :: EntityName -> IntermediateT a ()
deleteFromActiveEntities = modifyScope S.delete

getActiveEntities :: IntermediateT a [EntityName]
getActiveEntities = do
  entityScope <- scope <$> gets entities
  return $ S.toList entityScope

involvedEntities :: [EntityName] -> IntermediateT a Entities
involvedEntities entities_names = do
  entity_info <- gets entities
  let indexingMap            = entity_to_index entity_info
      available_entities     = all_entities entity_info
      (min_index, max_index) = foldr (updateMaxAndMin indexingMap) (V.length available_entities + 1, 0) entities_names
  return $ V.unsafeSlice min_index (max_index - min_index + 1) available_entities
 where
  updateMaxAndMin indexing entity val@(min_v, max_v) = case M.lookup entity indexing of
    Nothing -> val
    Just a  -> (min a min_v, max a max_v)

nextEntity :: EntityName -> IntermediateT a (Maybe String)
nextEntity entity = do
  entity_info <- gets entities
  let available_entities = all_entities entity_info
      indexing           = M.lookup entity (entity_to_index entity_info)
  return $ indexing >>= (available_entities V.!?) . (1 +)

prevEntity :: EntityName -> IntermediateT a (Maybe String)
prevEntity entity = do
  entity_info <- gets entities
  let available_entities = all_entities entity_info
      indexing           = M.lookup entity (entity_to_index entity_info)
  return $ indexing >>= \v -> available_entities V.!? (v - 1)
