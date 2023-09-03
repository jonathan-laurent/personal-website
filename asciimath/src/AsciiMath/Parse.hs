{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AsciiMath.Parse (parseAst) where

import qualified AsciiMath.Ast   as Ast
import qualified AsciiMath.Spec  as Spec
import qualified AsciiMath.Lexer as Lexer

import AsciiMath.Lexer
import AsciiMath.Spec

import Text.Parsec.Combinator
import Text.Parsec.Prim
import GHC.Exts (sortWith, groupWith)

-------------------------------------------------------------------------------

-- Handling operators precedence

supSubPrec :: Precedence
supSubPrec = 5

opsByPrec :: Spec -> [[Token]]
opsByPrec spec =
  map (map fst) $
  groupWith snd $
  sortWith snd $
  [(Binop f, ar) | (_, f, ar) <- Spec.binops spec] ++
  [(Hat, supSubPrec), (Underscore, supSubPrec)]

-------------------------------------------------------------------------------

-- Building the AST

ast :: Spec -> Parsec [Token] st Ast.Ast
ast spec = fmap Ast.Ast (many (expr spec))

expr :: Spec -> Parsec [Token] st Ast.Expr
expr spec =
  parseOp (opsByPrec spec)
  where
    parseOp [] = term spec
    parseOp (ops : opss) =
      parseOp opss `chainl1` transformer ops

term :: Spec -> Parsec [Token] st Ast.Expr
term spec =
  choice [group, delimiters, function, special, lit, semSpace, text]
  where
    group = tok ParO *> fmap Ast.Group (ast spec) <* tok ParC
    function = do
      (s, a) <- fun
      args <- count a (term spec)
      return (Ast.Fun s args)

    delimiters = choice
      [ delimiter f | (op, cl, f) <- Spec.delimiters spec, op /= cl ]

    delimiter f = do
      tok (DelO f)
      e <- ast spec
      tok (DelC f)
      return $ Ast.Fun f [Ast.Group e]

transformer :: [Token] -> Parsec [Token] st (Ast.Expr -> Ast.Expr -> Ast.Expr)
transformer ops = choice (map parser ops)
  where
    parser op = tok op >> return (res op)

    res Hat e1 e2 = Ast.Sup e1 e2
    res Underscore e1 e2 = Ast.Sub e1 e2
    res (Binop f) e1 e2 = Ast.Fun f [e1, e2]
    res _ _ _ = error "Such a token does not represent an operator"

parseAst spec str = do
  toks <- Lexer.lex spec str
  Ast.groupParens "parens" <$> parse (ast spec) "" toks

-------------------------------------------------------------------------------

-- Boilerplate: reading the tokens stream

parseT :: (Token -> Maybe a) -> Parsec [Token] st a
parseT = tokenPrim showT nextT
  where
    showT = show
    nextT pos _ _ = pos

tok t    = parseT (\c -> if c == t then Just () else Nothing)
semSpace = parseT (\case {Space n -> Just (Ast.Space n) ; _ -> Nothing})
_binop   = parseT (\case {Binop s -> Just s ; _ -> Nothing})
fun      = parseT (\case {Fun s a -> Just (s, a) ; _ -> Nothing})
lit      = parseT (\case {Lit s -> Just (Ast.Lit s) ; _ -> Nothing})
special  = parseT (\case {Special s -> Just (Ast.Special s) ; _ -> Nothing})
text     = parseT (\case {String s -> Just (Ast.Text s) ; _ -> Nothing})

-------------------------------------------------------------------------------