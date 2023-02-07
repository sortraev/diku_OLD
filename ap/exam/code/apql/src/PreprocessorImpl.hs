module PreprocessorImpl where

import Data.List

import Types
import Utils



-- The simplified Clause type I use in stratification,
-- since here, a lot of information can be safely ignored.
-- A SimpleClause is a clause head and the positive/negative
-- references to other atoms made by this particular atom.
type SimpleClause = (PSpec, ([PSpec], [PSpec]))



----------------------
--- STRATIFICATION ---
----------------------
stratify :: IDB -> [PSpec] -> Either ErrMsg [[PSpec]]
stratify (IDB ips clauses) eps =
  if ips `disjoint` eps then
     makeStrata (map simpleClause clauses) ips []
  else Left $ EUser $ "Cannot stratify: Overlap in ex- and intensionals."

  where
    simpleClause (Clause atom posRefs tests) =
      (pSpec atom, (map pSpec posRefs, negRefs tests))
    negRefs tests = [pSpec a | TNot a <- tests]


    -- Since we can ignore a lot of information during stratification,
    -- makeStrata and makeStratum use the SimpleClause type I have defined in Util.hs.
    makeStrata :: [SimpleClause] -> [PSpec] -> [[PSpec]]
               -> Either ErrMsg [[PSpec]]
    makeStrata _ [] placed = return placed
    makeStrata clauses unplaced placed =
      makeStratum clauses unplaced [] >>= \i ->
        makeStrata clauses (unplaced \\ i) (placed ++ [i])


    makeStratum :: [SimpleClause] -> [PSpec] -> [PSpec]
                -> Either ErrMsg [PSpec]
    makeStratum clauses stratum removed =
      if stratum' == [] then Left $ EUser "Cannot stratify: empty stratum."
      else if stratum' == stratum then return stratum'
      else makeStratum clauses' stratum' removed'

      where
        stratum' = filter keep stratum
        clauses' = filter ((`elem` stratum') . fst) clauses
        removed' = removed ++ (stratum \\ stratum')

        -- keep p if it has no neg references to atoms in its stratum,
        -- and no pos references to atoms removed in this iteration.
        keep p = neg `disjoint` stratum && pos `disjoint` removed
          where (pos, neg) = foldl combine ([], [])
                               [snd c | c <- clauses, fst c == p]
    


----------------------
--- CLAUSIFICATION ---
----------------------
clausify :: Program -> Either ErrMsg IDB
clausify program = makeIDB <$> makeClauses (transform program)
  where
    makeIDB clauses = IDB (pSpecs clauses) clauses
    pSpecs = nub . map (\(Clause atom _ _) -> pSpec atom)

    makeClauses :: Program -> Either ErrMsg [Clause]
    makeClauses = mapM (\(Rule atom cond) ->
      verifyClause $ uncurry (Clause atom) $ makeClause cond)

    makeClause :: Cond -> ([Atom], [Test])
    makeClause (CAnd c1 c2)       = makeClause c1 `combine` makeClause c2
    makeClause (CAtom a)          = ([a], [])
    makeClause (CNot (CAtom a))   = ([], [TNot a])
    makeClause (CEq t1 t2)        = ([], [TEq  t1 t2])
    makeClause (CNot (CEq t1 t2)) = ([], [TNeq t1 t2])
    makeClause _                  = ([], [])


    verifyClause :: Clause -> Either ErrMsg Clause
    verifyClause clause@(Clause (Atom _ head_args) atoms ts) =
      if need `subsetOf` have
      then return clause
      else Left $ EUser $ "Cannot verify clause. Missing variables: "
                             ++ show (need \\ have)
      where vars terms = [var | var@TVar{} <- concat terms]
            have = vars $ map args atoms
            need = vars $ head_args : [[a, b] | TNeq a b <- ts] ++
                                      [[a, b] | TEq  a b <- ts]



------------------------------
--- PROGRAM TRANSFORMATION ---
------------------------------
-- AND distribution can uncover new opportunities for "not flipping", and
-- possibly vice versa, so to be safe, I repeat the transformation until
-- no more transformations apply. Perhaps rule splitting should not be
transform :: Program -> Program
transform prog = if prog' == prog then prog' else transform prog'
  where
    prog' = splitORs $ distribAnds $ flipNots prog


    -- "Flip" logical negations in conditions for all rules in program.
    flipNots :: Program -> Program
    flipNots = map (\(Rule a cond) -> Rule a $ flipNot cond)

    -- Distribute AND conditions for all rules in program.
    distribAnds :: Program -> Program
    distribAnds = map (\(Rule a cond) -> Rule a $ distribAnd cond)

    -- Split rules of the form "atom if c1 or c2", where c1 and c2 and conditions,
    -- into multiple rules - do so recursively for all rules in program. Also,
    -- remove any rules of the form "atom if false".
    splitORs :: Program -> Program
    splitORs = concatMap splitOR

    flipNot :: Cond -> Cond
    flipNot (CNot (CAnd c1 c2)) = COr  (CNot (flipNot c1)) (CNot (flipNot c2))
    flipNot (CNot (COr  c1 c2)) = CAnd (CNot (flipNot c1)) (CNot (flipNot c2))
    flipNot (CNot (CNot c)) = flipNot c
    flipNot (CAnd c1 c2) = CAnd (flipNot c1) (flipNot c2)
    flipNot (COr  c1 c2) = COr  (flipNot c1) (flipNot c2)
    flipNot c = c

    distribAnd :: Cond -> Cond
    distribAnd (CAnd _ (CNot CTrue)) = CNot CTrue
    distribAnd (CAnd (CNot CTrue) _) = CNot CTrue
    distribAnd (CAnd c1 CTrue) = distribAnd c1
    distribAnd (CAnd CTrue c2) = distribAnd c2
    distribAnd (CAnd c1 (COr c2 c3)) = COr (CAnd c1 c2) (CAnd c1 c3)
    distribAnd (CAnd (COr c1 c2) c3) = COr (CAnd c3 c1) (CAnd c3 c2)
    distribAnd (CAnd c1 c2) = CAnd (distribAnd c1) (distribAnd c2)
    distribAnd (COr  c1 c2) = COr  (distribAnd c1) (distribAnd c2)
    distribAnd (CNot c)     = CNot (distribAnd c)
    distribAnd c = c

    splitOR :: Rule -> Program
    splitOR (Rule atom (COr c1 c2)) =
      splitOR (Rule atom c1) ++ splitOR (Rule atom c2)
    splitOR (Rule _ (CNot CTrue)) = []
    splitOR rule = [rule]
