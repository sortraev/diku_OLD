module EngineImpl where

import Control.Monad (guard, zipWithM, foldM)
import Data.Functor (($>))
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as S
import qualified Data.Map as M

import Types
import Utils (pSpec, args, (.:))



----------------------------
--- EXECUTION ENGINE API ---
----------------------------
execute :: IDB -> [[PSpec]] -> EDB -> Either ErrMsg EDB
execute _ _ _ = Left $ EUnimplemented "Failed attempt at an implementation :("



-- The internals of my execution engine use a different representation of
-- strata. A stratum is a list of tuples of atoms and their clauses.
type Strata  = [Stratum]
type Stratum = [(PSpec, [Clause])]



-- The type I use to represent EDB's internally.
-- Using a Map makes lookup and merging of EDB's easier.
type EDBM = M.Map PSpec ETable

-- an instance is a mapping of variables to data (these both have type Term,
-- but is essentially a map of TVar to TData)
type Instance = [(Term, Term)]
        -- options = edb `edbGetExtens` atom :: [Row] -- ETable
        -- instances atom = mapMaybe (makeInstance atom) options



-------------------------------------------------
--- MY FAILED EXECUTION ENGINE IMPLEMENTATION ---
-------------------------------------------------
execute_ :: IDB -> [[PSpec]] -> EDB -> Either ErrMsg EDB
execute_ (IDB preds clauses) strata edb =
  
  if preds /= concat strata then Left $ EUser "Mismatch between input\
                                              \ predicates and strata!"
  -- internalize strata and EDB, then run evalStrata.
  else 
    M.toList <$> foldM (flip evalStratum) edb' strata' -- evalStrata strata' edb'
    
  where
    strata' = map toStratum strata
    edb'    = M.fromListWith S.union edb

    toStratum    = map (\p -> (p, clausesFor p))
    clausesFor p = filter (`isClauseFor` p) clauses
    isClauseFor (Clause a _ _) p = pSpec a == p


evalStratum :: Stratum -> EDBM -> Either ErrMsg EDBM
evalStratum stratum edb =
  if edb' == edb then return edb' -- nothing's changed this iteration? return! :)
  else evalStratum stratum edb'

  where
    contributions = foldM (\edb' (p, cs) -> contribution p cs edb') edb stratum

    contribution :: PSpec -> [Clause] -> EDBM -> Maybe EDBM
    contribution _ [] edb = Just edb
    contribution p (c@(Clause head _ _):cs) edb = do
      contribution p cs $ edbUnions $ mapMaybe (applyClause edb c) insts

      where insts  = mapMaybe (makeInstance head) extens
            extens = edb `extensFor` p

    applyClause :: EDBM -> Clause -> Instance -> Maybe EDBM
    applyClause edb (Clause head posrefs tests) inst =
      mapM (matchAtom edb inst) posrefs >> -- match and verify pos references.
        mapM (checkTest edb inst) tests $> -- match and verify tests.
          edbSingletonFromAtom head        -- add contribution to an EDBM.


    -- An atom is attempted instantiated with a row of data by
    -- a pairwise match-up of atom arguments to row entries.
    makeInstance :: Atom -> Row -> Maybe Instance
    makeInstance (Atom name args) row = zipWithM matchup args row
      where matchup (TVar _)   d  = Just (TVar name, TData d)
            matchup (TData d1) d2 =
              if d1 == d2 then Just (TVar name, TData d2)
                          else Nothing

    checkTest :: EDBM -> Instance -> Test -> Maybe ()
    checkTest edb inst (TNot atom) = matchAtom edb inst atom $> ()
    checkTest _ inst (TEq  t1 t2) = (guard .: (==)) (matchTerm inst t1)
                                                    (matchTerm inst t2)
    checkTest _ inst (TNeq t1 t2) = (guard .: (/=)) (matchTerm inst t1)
                                                    (matchTerm inst t2)

    matchTerm :: Instance -> Term -> Maybe Term
    matchTerm _    dat@(TData _) = Just dat
    matchTerm inst var@(TVar  _) = lookup var inst

    matchAtom :: EDBM -> Instance -> Atom -> Maybe Atom
    matchAtom edb inst (Atom name args) =
      mapM (matchTerm inst) args >>= \args' ->
        if edbInstExists edb (Atom name args')
        then Just (Atom name args') else Nothing

    edb' = edb `edbUnion` fromMaybe edb contributions



-------------------------------
--- EDBM SPECIFIC UTILITIES ---
-------------------------------
edbInstExists :: EDBM -> Atom -> Bool
edbInstExists edb atom = args' `S.member` etable
  where etable = M.findWithDefault S.empty (pSpec atom) edb

        args'  = [dat | (TData dat) <- args atom]

edbSingletonFromAtom :: Atom -> EDBM
edbSingletonFromAtom atom@(Atom _ args) =
  M.singleton (pSpec atom) $ S.singleton [dat | (TData dat) <- args]

edbUnion :: EDBM -> EDBM -> EDBM
edbUnion = M.unionWith S.union

edbUnions :: [EDBM] -> EDBM
edbUnions = M.unionsWith S.union

extensFor :: EDBM -> PSpec -> [Row]
extensFor = S.toList .:  flip (M.findWithDefault S.empty)-- extensFor'
