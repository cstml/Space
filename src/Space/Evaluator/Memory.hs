{-# LANGUAGE TemplateHaskell #-}

module Space.Evaluator.Memory where

import Control.Lens hiding ((:<), (<|))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence
import Space.Evaluator.Stack
import Space.Language
import Space.Language.Empty
import Prettyprinter

data Memory location binder term = Memory
  { _spine :: term
  , _stacks :: Map location (Stack term)
  , _binds :: Map binder term
  }
  deriving (Eq, Show)

makeLenses ''Memory

instance (Ord b, Ord l, Semigroup t) => Semigroup (Memory l b t) where
  m1 <> m2 =
    Memory
      { _spine = m1 ^. spine <> m2 ^. spine
      , _stacks = m1 ^. stacks <> m2 ^. stacks
      , _binds = m1 ^. binds <> m2 ^. binds
      }

instance (Ord b, Ord l, Monoid t) => Monoid (Memory l b t) where
  mempty = Memory mempty mempty mempty

instance (Show l, Show b, Show t, Pretty l, Pretty b, Pretty t ) => Pretty (Memory l b t) where
  pretty mem =
    vsep [ vsep [pretty "Spine:"
                , indent 4 $ pretty (mem ^. spine)
                ]           
         , vsep [ pretty "Stacks:"
                , indent 4 $ (pretty (mem ^. stacks))
                ]
         , vsep [ pretty "Binds:"
                , indent 4 $ (unsafeViaShow (mem ^. stacks))
                ]
         , emptyDoc
         ]
    
instance (Show l,  Show t, Pretty l, Pretty t ) => Pretty (Map l t) where
  pretty m =  Map.foldrWithKey go emptyDoc m 
    where
      go k v doc = vsep [ (pretty k) <> colon <+> (pretty v)
                        , doc
                        ]
