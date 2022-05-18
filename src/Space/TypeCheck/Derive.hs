module Space.TypeCheck.Derive where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.Bifunctor
import Space.Language
import Space.TypeCheck.Derivation
import Space.TypeCheck.Properties

type VarStream = [String]

data TDerivationInfo = TDerivationInfo
  { _numVarStream :: [String]
  , _alphaVarStream :: [Char]
  }
  deriving (Show, Eq)

makeLenses ''TDerivationInfo

derive :: Term -> DeriveM (TDerivation TJudgement)
derive t = runReaderT (derive' t) initTDerivationInfo
 where
  initTDerivationInfo = TDerivationInfo (show <$> [1 ..]) ['a' ..]

  err :: forall a m r. MonadFail m => String -> ReaderT a m r
  err = lift . fail

  implies term cont = do
    -- Get a new variable
    (freshVar, tdInfo) <- asks getANewTVariable

    -- Formulate a basic judgement,
    let judgement = TJudgement (TContext []) term freshVar

    -- Continue recursively,
    contD <- local (const tdInfo) (derive' cont)

    -- Return the term.
    pure $ DSeq judgement contD

  impliesConj term term1 cont constr = do
    -- split the stream and get a freshvariable from the right one
    ((freshVar, lInfo), rInfo) <- asks (first getANewTVariable . splitStream)

    -- calculate the derivation of the push term
    pushD <- local (const lInfo) (derive' term1)

    -- calculate the derivation of the following term
    contD <- local (const rInfo) (derive' cont)

    -- formulate the judgement
    let judgement = TJudgement mempty term freshVar

    -- return the derivation
    pure $ constr judgement pushD contD

  derive' :: Term -> ReaderT TDerivationInfo DeriveM (TDerivation TJudgement)
  derive' term = case term of
    SInteger i cont -> implies term cont
    SChar i cont -> implies term cont
    SVariable v cont -> implies term cont
    SPush pushT loc cont -> impliesConj term pushT cont DPush
    SPop var loc cont -> impliesConj term (SVariable var SEmpty) cont DPop
    SPopT var loc ty cont -> impliesConj term (SVariable var SEmpty) cont DPop
    SEmpty -> pure . DEmpty $ TJudgement mempty SEmpty TEmpty

-- | There are some types which are trivial to infer, like SInteger and SChar.
infer1Trivial :: TJudgement -> [TSubstitution]
infer1Trivial x = undefined

getANewTVariable :: TDerivationInfo -> (SType, TDerivationInfo)
getANewTVariable info =
  ( flip TVariable TEmpty . TVariableAtom $ (head (info ^. alphaVarStream) : head (info ^. numVarStream))
  , info & numVarStream %~ tail
  )

splitStream :: TDerivationInfo -> (TDerivationInfo, TDerivationInfo)
splitStream info =
  ( info & alphaVarStream %~ tail
  , info & alphaVarStream %~ tail . tail
  )
