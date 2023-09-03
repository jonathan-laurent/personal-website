module AsciiMath.Ast where

-------------------------------------------------------------------------------

newtype Ast = Ast [Expr] deriving (Show)

data Expr =
  Lit String
  | Text String
  | Special String
  | Sub Expr Expr
  | Sup Expr Expr
  | Fun String [Expr]
  | Space Int
  | Group Ast
  deriving (Show)

instance Semigroup Ast where
  (Ast xs) <> (Ast ys) = Ast (xs ++ ys)

instance Monoid Ast where
  mempty = Ast []

-------------------------------------------------------------------------------

-- Operations on AST

modifyAst_ :: (Expr -> Expr) -> (Expr -> Expr) -> Ast -> Ast
modifyAst_ f ftop (Ast es) = Ast (map (ftop . aux) es)
  where
    aux e = f $
      case e of
        Sub e1 e2  -> Sub (aux e1) (aux e2)
        Sup e1 e2  -> Sup (aux e1) (aux e2)
        Fun s es   -> Fun s (map aux es)
        Group a    -> Group (modifyAst_ f ftop a)
        t          -> t

modifyAst :: (Expr -> Expr) -> Ast -> Ast
modifyAst f    = modifyAst_ f id

modifyAstTop :: (Expr -> Expr) -> Ast -> Ast
modifyAstTop = modifyAst_ id

-------------------------------------------------------------------------------

-- Add explicit parens

groupParens :: String -> Ast -> Ast
groupParens f =
  modifyAst subsup . modifyAst doubleParens . modifyAstTop appendedParens
  where
    appendedParens (Group a) = Fun f [Group a]
    appendedParens e = e

    doubleParens (Group (Ast [Group a])) = Fun f [Group a]
    doubleParens t = t

    subsup (Sub (Group a) e) = Sub (Fun f [Group a]) e
    subsup (Sup (Group a) e) = Sup (Fun f [Group a]) e
    subsup t = t

-------------------------------------------------------------------------------