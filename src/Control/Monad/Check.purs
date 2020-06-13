module Steiner.Control.Monad.Check where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, Reader, ReaderT, local, runReaderT)
import Data.Lens (Lens', Traversal', set)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Steiner.Control.Monad.Unify (UnifyState, UnifyT, runUnifyT)
import Steiner.Data.Lens (newtypeIso)
import Steiner.Language.Type (Type)

-- |
-- Read only environment containing types of variables in scope
--
newtype CheckEnv
  = CheckEnv
  { typeEnv :: Map String Type
  }

derive instance newtypeCheckEnv :: Newtype CheckEnv _

derive newtype instance semigroupCheckEnv :: Semigroup CheckEnv

derive newtype instance monoidCheckEnv :: Monoid CheckEnv

-- Lenses
_typeEnv :: Lens' CheckEnv (Map String Type)
_typeEnv = newtypeIso <<< prop (SProxy :: SProxy "typeEnv")

_atType :: String -> Traversal' CheckEnv (Maybe Type)
_atType name = _typeEnv <<< at name

-- |
-- Run a combination of the UnifyT monad with a CheckEnv Reader
--
runWithUnifyT :: forall m a. UnifyT Type (ReaderT CheckEnv m) a -> m (Tuple a (UnifyState Type))
runWithUnifyT = flip runReaderT mempty <<< runUnifyT

-- |
-- The check monad offers access to types of variables in scope.
--
newtype Check a
  = Check (Reader CheckEnv a)

-- |
-- Run a computation with a new variable in scope
--
withVariable :: forall m a. MonadReader CheckEnv m => String -> Type -> m a -> m a
withVariable name = local <<< set (_atType name) <<< Just

derive instance functorCheck :: Functor Check

derive newtype instance applyCheck :: Apply Check

derive newtype instance applicativeCheck :: Applicative Check

derive newtype instance bindCheck :: Bind Check

derive newtype instance monadCheck :: Monad Check

derive newtype instance monadAskCheck :: MonadAsk CheckEnv Check

derive newtype instance monadReaderCheck :: MonadReader CheckEnv Check
