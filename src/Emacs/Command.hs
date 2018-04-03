{-# LANGUAGE OverloadedStrings #-}
module Emacs.Command
 ( setCommand
 , defcommand'
 ) where

import Emacs.Core
import Data.Text
import Control.Monad (void)

-- 依存型とか駆使すれば多分 interacvie form と関数の引数の型の整合性が
-- 取れていることが多分保証できる。

-- これは直接は使って欲くないため、' postfix を付けている。
-- EmacsValue を引数として取っているので function 以外のものを渡せてしまう
setCommand :: Text -> InteractiveForm -> EmacsValue -> EmacsM ()
setCommand fname form f = do
  fnameQ <- intern fname
  interactiveFormQ <- intern "interactive-form"
  void $ funcall2 "fset" fnameQ f
  void $ funcall3 "put"  fnameQ interactiveFormQ =<< evalString "'(interactive nil)"

-- TODO: interacitve-form の携帯によってarity は決まる？かな
defcommand'
  :: Text
  -> Doc
  -> InteractiveForm
  -> Arity
  -> ([EmacsValue] -> EmacsM EmacsValue)
  -> EmacsM ()
defcommand' fname (Doc doc) form (Arity arity) f =
  setCommand fname form =<< mkFunction f arity arity doc
