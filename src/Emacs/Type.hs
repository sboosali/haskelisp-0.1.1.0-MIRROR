{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.Type where

import Prelude(Show(..))
import Protolude hiding (show)
import Data.IORef
import GHC.Ptr
import Foreign.C.Types
import Foreign.StablePtr
import Control.Monad.Reader
import Data.Data hiding(typeOf)

data PState = PState
  { symbolMap :: IORef (Map Text GlobalEmacsValue)
  }

data Ctx = Ctx
  { pstateStablePtr :: StablePtr PState
  , pstate :: PState
  , emacsEnv :: EmacsEnv
  }

type EmacsM =
  ReaderT Ctx IO

-- nil について
--
-- emacs 内部では nil は文字列で表現できないシンボルとして定義されている。
-- globals.h
--
-- #define Qnil builtin_lisp_symbol (0)
--
-- type-of では取得できない。nil は型ではない？かな。多分 type_of に渡
-- すとエラーになる。
--
-- haskell では不便なので ENil という型を導入する。
data EmacsType = ESymbol
               | EInteger
               | EFunction
               | EString
               | ECons
               | ENil
  deriving (Show, Eq, Data)

-- これは emacs 側での正式な名前である必要がある。typeOf が依存してい
-- る。nil は若干嘘が入っている。nil という型は存在しない。
emacsTypeSymbolName :: EmacsType -> Text
emacsTypeSymbolName ESymbol   = "symbol"
emacsTypeSymbolName EInteger  = "integer"
emacsTypeSymbolName EFunction = "function"
emacsTypeSymbolName EString   = "string"
emacsTypeSymbolName ECons     = "cons"
emacsTypeSymbolName ENil      = "nil" -- lie

emacsTypes :: [EmacsType]
emacsTypes = fromConstr <$> dataTypeConstrs (dataTypeOf ESymbol)


type EmacsModule = Ptr () -> IO CInt

newtype EmacsEnv   = EmacsEnv (Ptr ())
  deriving (Storable)

newtype EmacsValue = EmacsValue (Ptr ())
  deriving (Storable)

newtype GlobalEmacsValue = GlobalEmacsValue (Ptr ())
  deriving (Storable)

castGlobalToEmacsValue :: GlobalEmacsValue -> EmacsValue
castGlobalToEmacsValue (GlobalEmacsValue p) =
  EmacsValue p

-- EmacsValue Opaque Type :w
--
-- これは導入するべきなのか？
-- 少なくともこれにラップする際は確実に保証できるときのみに
newtype EmacsSymbol   = EmacsSymbol   EmacsValue
newtype EmacsKeyword  = EmacsKeyword  EmacsValue
newtype EmacsCons     = EmacsCons     EmacsValue
newtype EmacsFunction = EmacsFunction EmacsValue
newtype EmacsList     = EmacsList     EmacsValue

-- Emacs の値に対する Haskell の型
-- 数値や文字列は素直なんだけど、他
-- Nil は空 [] でいいのかな？
newtype Symbol = Symbol Text
newtype Keyword = Keyword Text
data Cons = Cons EmacsValue EmacsValue

-- 例外機構

data EmacsFuncallExit
  = EmacsFuncallExitReturn
  | EmacsFuncallExitSignal
  | EmacsFuncallExitThrow
  deriving (Show,Eq,Enum)

data EmacsException
  = EmacsException EmacsFuncallExit EmacsValue EmacsValue

instance Show EmacsException where
  show (EmacsException funcallExit _ _) =
    "EmacsException(" <> show funcallExit <> ")"

instance Exception EmacsException

-- 関数定義のため必要な型

type EFunctionStub
  = EmacsEnv
  -> CPtrdiff
  -> Ptr (Ptr ())
  -> StablePtr PState
  -> IO EmacsValue

data InteractiveForm = InteractiveNoArgs

newtype Doc   = Doc Text   -- ドキュメント(関数など)
newtype Arity = Arity Int  -- 関数アリティ
