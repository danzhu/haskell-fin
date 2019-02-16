module Compiler
    ( compile
    ) where

import           Compiler.CodeGen (codeGen)
import           Compiler.Data (SrcPos (SrcPos))
import           Compiler.Parser (ParserError, parseAst)
import           Compiler.TypeCheck (TypeError (TypeErr), typeAst)
import           Control.Arrow (left)
import           Data.List.NonEmpty as NonEmpty
import           Data.Set as Set
import           Data.Void (Void)
import           Debug.Trace
import           Text.Megaparsec (ErrorFancy (ErrorFail),
                                  ParseError (FancyError),
                                  ParseErrorBundle (ParseErrorBundle),
                                  PosState (PosState), bundleErrors,
                                  bundlePosState, defaultTabWidth,
                                  errorBundlePretty, initialPos, pstateInput,
                                  pstateLinePrefix, pstateOffset,
                                  pstateSourcePos, pstateTabWidth)

data CompilerError = ParserError ParserError
                   | TypeError (PosState String) TypeError

instance Show CompilerError where
  show (ParserError bundle) = errorBundlePretty bundle
  show (TypeError state (TypeErr (SrcPos off) kind)) =
    let msg = show kind
        err = FancyError off $ Set.singleton $ ErrorFail msg
        bundle = ParseErrorBundle
          { bundleErrors = NonEmpty.fromList [err]
          , bundlePosState = state }
          :: ParseErrorBundle String Void
    in errorBundlePretty bundle

compile :: String -> String -> Either CompilerError String
compile path src = do
  let state = PosState
        { pstateInput = src
        , pstateOffset = 0
        , pstateSourcePos = initialPos path
        , pstateTabWidth = defaultTabWidth
        , pstateLinePrefix = "" }
  ast <- left ParserError $ parseAst path src
  typed <- left (TypeError state) $ typeAst ast
  traceShowM typed
  let asm = codeGen typed
  traceM asm
  pure asm
