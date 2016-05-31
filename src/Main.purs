module Main where

import Prelude
import Data.Map as M
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

type Unknown = Int

type Binder = Int

data Tm
  = Var Binder
  | Fun Binder Tm
  | App Tm Tm
  | Int Int

infix 4 App as $$

derive instance genericTm :: Generic Tm

instance showTm :: Show Tm where
  show = gShow

data Ty
  = TyVar Binder
  | TyFun Ty Ty
  | TyInt

infixl 4 TyFun as ~>

derive instance genericTy :: Generic Ty

instance showTy :: Show Ty where
  show = gShow

lam :: (Tm -> Tm) -> Tm
lam f = Fun b e'
  where
    b = maxVar e + 1
    e = f (Var 0)
    e' = f (Var b)

    maxVar (Fun bi _) = bi
    maxVar (App e1 e2) = max (maxVar e1) (maxVar e2)
    maxVar _ = 0

type Subst = Map Binder Ty

applyS :: Subst -> Ty -> Ty
applyS s = go
  where
    go (TyVar b) = TyVar b `fromMaybe` M.lookup b s
    go (t1 ~> t2) = go t1 ~> go t2
    go other = other

combine :: Subst -> Subst -> Subst
combine s1 s2 = map (applyS s1) s2 <> s1

occursIn :: Binder -> Ty -> Boolean
occursIn b (TyVar b1) = b == b1
occursIn b (t1 ~> t2) = occursIn b t1 || occursIn b t2
occursIn _ _          = false

unify :: Ty -> Ty -> Subst
unify TyInt TyInt = M.empty
unify (TyVar b) t | b `occursIn` t = unsafeCrashWith "Occurs check failed!"
                  | otherwise = M.singleton b t
unify t (TyVar b) = unify (TyVar b) t
unify (t1 ~> t2) (t3 ~> t4) = unify t1 t3 `combine` unify t2 t4
unify _ _ = unsafeCrashWith "Unification failed"

type Fresh = Binder

w :: Tm -> Ty
w = _.ty <<< go 0 M.empty
  where
  go :: Fresh
     -> Subst
     -> Tm
     -> { subst :: Subst, ty :: Ty, supply :: Fresh }
  go supply _   (Int i) = { subst: M.empty :: Subst, ty: TyInt, supply }
  go supply env (Var b) = { subst: M.empty :: Subst, ty, supply }
    where
    ty = unsafePartial (fromJust (M.lookup b env))
  go fr env (App e1 e2) = { subst: v `combine` res2.subst `combine` res1.subst
                          , ty: applyS v b
                          , supply: res2.supply
                          }
    where
      res1 = go (fr + 1) env e1
      res2 = go res1.supply (res1.subst `combine` env) e2
      b = TyVar fr
      v = unify (applyS res2.subst res1.ty) (res2.ty ~> b)
  go fr env (Fun x e1) = { subst: res.subst
                         , ty: applyS res.subst b ~> res.ty
                         , supply: res.supply
                         }
    where
    b = TyVar fr
    res = go (fr + 1) (M.delete x env <> M.singleton x b) e1

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
