{-# LANGUAGE TemplateHaskell #-}

module Compiler.CodeGen
  ( codeGen
  ) where

import           Compiler.Ast
import           Compiler.Lens
import           Control.Monad.Except (throwError)
import           Control.Monad.Identity (runIdentity)
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Writer as Writer
import           Data.FileEmbed (embedStringFile)
import           Data.Foldable (sequenceA_)
import qualified Data.List as List
import           Data.Traversable (for)


-- constants
shebang :: String
shebang = "#!/usr/bin/env node"

lib :: String
lib = $(embedStringFile "res/lib.js")


-- generator util
type Generator = RWS.RWS () String Int

class Gen a where
  gen :: a -> Generator ()

write :: String -> Generator ()
write = RWS.tell

paren :: Generator a -> Generator a
paren g = write "(" *> g <* write ")"

brace :: Generator a -> Generator a
brace g = write "{" *> g <* write "}"

commaSep :: Gen a => [a] -> Generator ()
commaSep = sequenceA_ . List.intersperse (write ",") . map gen

fun :: [Var] -> Generator a -> Generator a
fun vars g = paren $ paren (commaSep vars) *> write "=>" *> g

scope :: Generator a -> Generator a
scope g = paren (write "()=>" *> brace g) <* paren (pure ())

tmpVar :: Generator Var
tmpVar = do
  i <- RWS.get
  RWS.put $ i + 1
  pure $ Var $ "_t" ++ show i

constVar :: Var -> Generator a -> Generator a
constVar var g = write "const " *> gen var *> write "=" *> g <* write ";"


-- pattern matching
data Match = Match Var MatchKind
           deriving Show
data MatchKind = MNew
               | MLit Lit
               deriving Show

data Act = Act Var Pat
         deriving Show

data Arm' = Arm' Pat Var
          deriving Show

matchChildren :: Applicative f => (Match -> f Match) -> Match -> f Match
matchChildren _ (Match var kind) = Match var <$> ch kind
  where ch m@MLit{} = pure m
        ch m@MNew   = pure m

matchPreorder :: Monad m => (Match -> m Match) -> Match -> m Match
matchPreorder = preorder matchChildren

patVars :: Pat -> [Var]
patVars Node{ nKind = kind } = case kind of
  PVar v -> [v]
  PLit{} -> []
  PIgn   -> []

matchPatVars :: Match -> Pat -> Writer.Writer [Var] ()
matchPatVars (Match var _) Node{ nKind = pat } = case pat of
  PLit{} -> pure ()
  PVar{} -> Writer.tell [var]
  PIgn   -> pure ()

nextAct :: Match -> Pat -> Either Act ()
nextAct _ Node{ nKind = PVar{} } = pure ()
nextAct _ Node{ nKind = PIgn } = pure ()
nextAct (Match var MNew) pat = throwError $ Act var pat
nextAct (Match _ (MLit mLit)) Node{ nKind = PLit pLit }
  | mLit == pLit = pure ()
  | otherwise = error "incompatible match and pattern"

replace :: Act -> Match -> Match
replace (Act aVar Node{ nKind = pat }) = runIdentity . matchPreorder rep
  where rep m@(Match mVar _)
          | mVar == aVar = pure $ Match mVar mKind
          | otherwise    = pure m
        mKind = case pat of
          PLit lit -> MLit lit
          PVar{}   -> error "invalid replacement pattern"
          PIgn     -> error "invalid replacement pattern"

matchSuccCompat :: Match -> Arm' -> Bool
matchSuccCompat (Match _ kind) (Arm' Node{ nKind = pat } _) = comp kind pat
  where comp MNew _            = True
        comp _ PVar{}          = True
        comp _ PIgn            = True
        comp (MLit m) (PLit p) = m == p

matchFailCompat :: Match -> Arm' -> Bool
matchFailCompat (Match _ kind) (Arm' Node{ nKind = pat } _) = comp kind pat
  where comp MNew _            = True
        comp _ PVar{}          = True
        comp _ PIgn            = True
        comp (MLit m) (PLit p) = m /= p

genAct :: Act -> Generator () -> Generator () -> Generator ()
genAct (Act var Node{ nKind = pat }) t f = case pat of
  PLit lit -> do
    write "if"
    paren $ do
      gen var
      write "==="
      gen lit
    brace t
    write "else"
    brace f
  PVar{} -> error "invalid act pattern"
  PIgn   -> error "invalid act pattern"

genMatch :: Var -> [Arm] -> Generator ()
genMatch var arms = fun [var] $ brace $ do
  arms' <- for arms $ \(Arm pat expr) -> do
    fn <- tmpVar
    constVar fn $ fun (patVars pat) $ gen expr
    pure $ Arm' pat fn
  match arms' $ Match var MNew
  where match :: [Arm'] -> Match -> Generator ()
        match [] _ = write "throw new Error('pattern fail');"
        match arms'@(Arm' pat fn : _) mat = case nextAct mat pat of
          Left act -> do
            let mat' = replace act mat
                ts = filter (matchSuccCompat mat') arms'
                fs = filter (matchFailCompat mat') arms'
            genAct act (match ts mat') (match fs mat)
          Right () -> do
            let vars = Writer.execWriter $ matchPatVars mat pat
            write "return "
            gen fn
            paren $ commaSep vars
            write ";"


-- expr code generation
instance Gen a => Gen (Node a) where
  gen Node{ nKind = a } = gen a

instance Gen Var where
  gen (Var v) = write $ '_' : v

instance Gen Lit where
  gen kind = case kind of
    LInt i -> write $ show i
    LStr s -> write $ show s

instance Gen ExprKind where
  gen kind = case kind of
    ELit lit -> gen lit
    EVar var -> gen var
    EApp f a -> do
      gen f
      paren $ gen a
    EAbs arms -> do
      case arms of
        [Arm Node{ nKind = PVar arg } expr] -> fun [arg] $ gen expr
        [Arm Node{ nKind = PIgn } expr] -> do
          arg <- tmpVar
          fun [arg] $ gen expr
        _ -> do
          arg <- tmpVar
          genMatch arg arms
    ESeq a b -> paren $ do
      gen a
      write ","
      gen b
    ELet var val expr -> do
      fun [var] $ gen expr
      paren $ gen val
    EDef var val expr -> scope $ do
      constVar var $ gen val
      write "return "
      gen expr


genAst :: Ast -> Generator ()
genAst (Ast expr) = do
  write shebang
  write "\n"
  write lib
  write "const main = "
  gen expr
  write ";\nconsole.log(main);"

codeGen :: Ast -> String
codeGen ast = snd $ RWS.evalRWS (genAst ast) () 0
