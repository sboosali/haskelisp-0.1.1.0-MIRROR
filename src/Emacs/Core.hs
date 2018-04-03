{-# LANGUAGE ForeignFunctionInterface,UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Core (
    module Emacs.Internal,
    defmodule,
    -- mk
    mkCons,
    -- funcall
    ToEmacsValue(..),
    ToEmacsSymbol(..),
    ToEmacsFunction(..),
    funcall1, funcall2, funcall3,
    mkFunctionFromCallable,
    Callable(..),
    --
    car,
    cdr,
    --
    evalString,
    provide,
    message,
    print,
    ) where

import Prelude()
import Protolude hiding (mkInteger,print,Symbol)
import Foreign.C.Types
import Foreign.StablePtr
import Emacs.Type
import Emacs.Internal

-- emacsModuleInit に渡す関数
defmodule :: Text -> EmacsM a -> EmacsModule
defmodule name mod ert = do
  env <- getEmacsEnvFromRT ert
  errorHandle env $ do
    ctx <- initCtx env
    runEmacsM ctx $ mod >> funcall1 "provide" (Symbol name)
  return 0

-- 関数の引数に ToEmacsValue を受け取るようにすると便利なんだけど、問
-- 題はその引数の実際の値を取得するめに EmacsM の中で実行する必要があ
-- り、引数の実行で例外が発生するかもしれない、ということ。
--
-- ある関数の中でEmacsException例外が発生したときにどのタイミングでど
-- こまで進んだかの保証が得られない。
-- ああ、けど IO での例外でも同じことが言えるのか...
class ToEmacsValue h where
  toEv :: h -> EmacsM EmacsValue

-- misc
-- EmacsM EmacsValue はどうなんだ... いいのかな？いいのであれば、
-- EmacsM EmacsSymbol とかも許容するべきかな。
-- いや、やはりないほうがいいかな。
instance ToEmacsValue EmacsValue where
  toEv = pure
-- instance ToEmacsValue (EmacsM EmacsValue) where
--   toEv = identity

-- Integer
instance ToEmacsValue Int where
  toEv = mkInteger

-- String
instance ToEmacsValue Text where
  toEv = mkString

-- Symbol
instance ToEmacsValue EmacsSymbol where
  toEv = pure . asEmacsValue
instance ToEmacsValue Symbol where
  toEv = (asEmacsValue<$>) . toEmacsSymbol

-- Kwyword
instance ToEmacsValue EmacsKeyword where
  toEv = pure . asEmacsValue
instance ToEmacsValue Keyword where
  toEv = (asEmacsValue<$>) . toEmacsKeyword

-- Bool
instance ToEmacsValue Bool where
  toEv True  = mkT
  toEv False = mkNil

-- Nil
instance ToEmacsValue () where
  toEv _ = mkNil

-- List
instance ToEmacsValue EmacsList where
  toEv = pure . asEmacsValue
instance ToEmacsValue h => ToEmacsValue [h] where
  toEv = (asEmacsValue<$>) . toEmacsList

-- Cons
instance ToEmacsValue EmacsCons where
  toEv = pure . asEmacsValue
instance (ToEmacsValue a, ToEmacsValue b) => ToEmacsValue (a, b) where
  toEv = (asEmacsValue<$>) . toEmacsCons

-- Function
-- Can only handle function with no arguments.
-- Use mkFunctionFromCallable for no args.
instance ToEmacsValue EmacsFunction where
  toEv = pure . asEmacsValue
instance (FromEmacsValue a, Callable b) => ToEmacsValue (a -> b) where
  toEv = (asEmacsValue<$>) . toEmacsFunction

-- AsEmacsValue
-- これはderiveしたいところ...
class    AsEmacsValue s             where asEmacsValue :: s -> EmacsValue
instance AsEmacsValue EmacsSymbol   where asEmacsValue (EmacsSymbol ev) = ev
instance AsEmacsValue EmacsKeyword  where asEmacsValue (EmacsKeyword ev) = ev
instance AsEmacsValue EmacsCons     where asEmacsValue (EmacsCons ev) = ev
instance AsEmacsValue EmacsList     where asEmacsValue (EmacsList ev) = ev
instance AsEmacsValue EmacsFunction where asEmacsValue (EmacsFunction ev) = ev

-- それぞれの OpaqueType への変換

-- Symbol
class ToEmacsValue s => ToEmacsSymbol s where
  toEmacsSymbol :: s -> EmacsM EmacsSymbol
instance ToEmacsSymbol EmacsSymbol where
  toEmacsSymbol = pure
instance ToEmacsSymbol Symbol      where
  toEmacsSymbol (Symbol t) = EmacsSymbol <$> intern t

-- Keyword
class ToEmacsValue s => ToEmacsKeyword s where
  toEmacsKeyword :: s -> EmacsM EmacsKeyword
instance ToEmacsKeyword EmacsKeyword where
  toEmacsKeyword = pure
instance ToEmacsKeyword Keyword where
  toEmacsKeyword (Keyword t) = EmacsKeyword <$> intern (":" <> t)

-- Cons
class ToEmacsValue s => ToEmacsCons s where
  toEmacsCons :: s -> EmacsM EmacsCons
instance ToEmacsCons EmacsCons where
  toEmacsCons = pure
instance (ToEmacsValue a, ToEmacsValue b) => ToEmacsCons (a, b) where
  toEmacsCons (a,b) = do
    av <- toEv a
    bv <- toEv b
    mkCons av bv

-- List
class ToEmacsValue s => ToEmacsList s where
  toEmacsList :: s -> EmacsM EmacsList
instance ToEmacsList EmacsList where
  toEmacsList = pure
instance ToEmacsValue x => ToEmacsList [x] where
  toEmacsList xs = EmacsList <$> (join $ mkList <$> mapM toEv xs)

-- Function
-- tricky
-- 無引数関数は明示的にやる必要ある。
class (Callable s,ToEmacsValue s) => ToEmacsFunction s where
  toEmacsFunction :: s -> EmacsM EmacsFunction

instance ToEmacsFunction EmacsFunction where
  toEmacsFunction = pure

instance (FromEmacsValue a, Callable b) => ToEmacsFunction (a -> b) where
  toEmacsFunction f = EmacsFunction <$> mkFunctionFromCallable f

-- Function call Utilities
funcall1
  :: ToEmacsValue a
  => Text
  -> a
  -> EmacsM EmacsValue
funcall1 fname ev0 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0]

funcall2
  :: (ToEmacsValue a, ToEmacsValue b)
  => Text
  -> a
  -> b
  -> EmacsM EmacsValue
funcall2 fname ev0 ev1 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1]

funcall3
  :: (ToEmacsValue a, ToEmacsValue b, ToEmacsValue c)
  => Text
  -> a
  -> b
  -> c
  -> EmacsM EmacsValue
funcall3 fname ev0 ev1 ev2 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1, toEv ev2]

-- Emacs -> Haskell
-- 変換に失敗する場合は例外を飛ばすように
--
-- 現状 EmacsValue を返している関数を、 h を返すようにするのも便利かも
-- しれないが、明示的な型指定する必要が増えるかも。。。
class FromEmacsValue h where
  fromEv :: EmacsValue -> EmacsM h

instance FromEmacsValue Int where
  fromEv = extractInteger

instance FromEmacsValue Text where
  fromEv = extractString

instance FromEmacsValue EmacsValue where
  fromEv = pure

-- TODO: これいいのか？チェック必要ないか？
instance FromEmacsValue EmacsFunction where
  fromEv = pure . EmacsFunction


-- 多相的な関数は駄目らしい(具体的な関数ならokらしい)
-- TODO: optional, rest 引数に対応する。
class Callable a where
    call :: a -> [EmacsValue] -> EmacsM (Either Text EmacsValue)
    arity :: a -> Int

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable a where
    call a [] = Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (IO a) where
    call a [] = do
      v <- liftIO a
      Right <$> toEv v
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (EmacsM a) where
    call am [] = do
      a <- am
      Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} (FromEmacsValue a, Callable b) => Callable (a -> b) where
  call f (e:es) = do
    av <- fromEv e
    call (f av) es
  call _ [] = pure $ Left "Too less arguments"
  arity f = arity (f undefined) + 1

-- 多相的な関数は怒られるはず。
mkFunctionFromCallable :: Callable f => f -> EmacsM EmacsValue
mkFunctionFromCallable f = do
  let a = arity f
  mkFunction func a a ""
  where
    func :: [EmacsValue] -> EmacsM EmacsValue
    func es = do
      res <- call f es
      case res of
        Right ev -> return ev
        Left _   -> undefined

evalString :: Text -> EmacsM EmacsValue
evalString t =
  funcall1 "eval" =<< (car =<< funcall1 "read-from-string" t)

provide :: Text -> EmacsM ()
provide feature =
  void $ funcall1 "provide" (Symbol feature)

message :: Text -> EmacsM ()
message t =
  void $ funcall1 "message" t

print :: ToEmacsValue v => v -> EmacsM ()
print ev =
  void $ funcall1 "print" ev

mkCons
  :: (ToEmacsValue a, ToEmacsValue b)
  => a
  -> b
  -> EmacsM EmacsCons
mkCons a b =
  EmacsCons <$> funcall2 "cons" a b

car :: EmacsValue -> EmacsM EmacsValue
car = funcall1 "car"

cdr :: EmacsValue -> EmacsM EmacsValue
cdr = funcall1 "cdr"
