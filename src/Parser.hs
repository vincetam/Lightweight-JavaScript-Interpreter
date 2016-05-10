module Parser (
  execJsBlk,
  parseFromFile,
  fileP
) where

import qualified Data.List as List
import Text.ParserCombinators.Parsec

import Ast

execJsBlk input = parse stmtsP "" input

fileP :: GenParser Char st Statement
fileP = do
  spaces
  doc <- docP
  eof
  prog <- case execJsBlk doc of
    Right stmt -> return stmt
    Left msg -> return $ Log (Val (StrVal ("Parse Error: " ++ (show msg))))
  return prog

docP :: GenParser Char st String
docP = do
  jsBlks <- jscriptP `sepEndBy` commentP
  return $ List.intercalate "" jsBlks

jscriptP :: GenParser Char st String
jscriptP = do
  js <- manyTill anyChar $ lookAhead (commentP <|> eof)
  return js

commentP :: GenParser Char st ()
commentP = do
  c <- try(string "/*" >> manyTill anyChar (string "*/") >>  spaces)
    <|> try(string "//" >> manyTill anyChar newline >> spaces)
  return c

stmtsP :: GenParser Char st Statement
stmtsP = do
  m <- try(stmtsP') <|> noneP
  return m

stmtsP' :: GenParser Char st Statement
stmtsP' = do
  e <- stmtP
  rest <- optionMaybe stmtsP'
  return $ case rest of
    Nothing -> e
    Just e' -> case e of
      Skip -> e'
      _ -> Sequence e e'

noneP :: GenParser Char st Statement
noneP = do
  spaces
  return Skip

restSeqP :: GenParser Char st Statement
restSeqP = do
  char ';'
  stmtsP

stmtP :: GenParser Char st Statement
stmtP = do
  spaces
  s <-  compoundStmtP
    <|> try(ifelseP)
    <|> try(ifP)
    <|> try(whileP)
    <|> try(dowhileP)
    <|> try(forP)
    <|> try(logP)
    <|> try(returnP)
    <|> try(funcdefP)
    <|> try(funcappP)
    <|> try(blockP)
    <|> assignP
    <?> "comments, compound statements, 'if', 'while', or assignment"
  spaces
  many $ char ';'
  spaces
  return s

compoundStmtP :: GenParser Char st Statement
compoundStmtP = do { 
  string "{"; s <- stmtsP; string "}";
  return s
}

funcdefP :: GenParser Char st Statement
funcdefP = do {
  string "function"; fid <- identifierP; char '('; parms <- identifierP `sepBy` (char ','); char ')'; s <- compoundStmtP;
  return $ FuncDef fid parms s
}

identifierP :: GenParser Char st Identifier
identifierP = do {
  spaces;
  identifier <- many1 (letter <|> digit <|> char '_' <|> char '$');
  spaces;
  return identifier
}

funcappP :: GenParser Char st Statement
funcappP = do {
  f <- funcP;
  return $ FuncApp f
}

blockP :: GenParser Char st Statement
blockP = do {
  string "(";
  spaces; string "function"; spaces; string "("; spaces; string ")";
  s <- compoundStmtP;
  string ")";
  string "("; spaces; string ")";
  return $ Block s
}

ifelseP :: GenParser Char st Statement
ifelseP = do {
  string "if"; spaces; string "("; e <- exprP; string ")";
  s1 <- stmtP;
  string "else";
  s2 <- stmtP;
  return $ IfElse e s1 s2
}

ifP :: GenParser Char st Statement
ifP = do {
  string "if"; spaces; string "("; e <- exprP; string ")";
  s <- stmtP;
  return $ If e s
}

whileP :: GenParser Char st Statement
whileP = do {
  string "while"; spaces; string "("; e <- exprP; string ")";
  s <- stmtP;
  return $ While e s
}

dowhileP :: GenParser Char st Statement
dowhileP = do {
  string "do";
  s <- stmtP;
  string "while"; spaces; string "("; e <- exprP; string ")";
  return $ DoWhile s e
}

forP :: GenParser Char st Statement
forP = do {
  string "for"; spaces; string "("; f0 <- stmtP; f1 <- exprP; char ';'; f2 <- stmtP; string ")";
  s <- stmtP;
  return $ For f0 f1 f2 s
}

logP :: GenParser Char st Statement
logP = do {
  string "console"; spaces; string "."; spaces; string "log"; spaces; string "("; e <- exprP; string ")";
  return $ Log e
}

returnP :: GenParser Char st Statement
returnP = do {
  string "return"; spaces; e <- exprP;
  return $ Return e
}

assignP :: GenParser Char st Statement
assignP = do
  s <- try(cpdAssignP) <|> try(incAssign) <|> stdAssignP <?> "assignment expression"
  return s

stdAssignP :: GenParser Char st Statement
stdAssignP = do {
  optionMaybe $ string "var"; spaces; var <- varP; spaces; string "="; spaces; e <- exprP;
  return $ Assign var e
}

cpdAssignP :: GenParser Char st Statement
cpdAssignP = do
  optionMaybe $ string "var"
  spaces
  var <- varP
  spaces
  op <- string "+=" 
    <|> string "-="
    <|> string "*="
    <|> string "/="
    <?> "assignment operator (+=, -=, *=, /=)"
  spaces
  val <- exprP
  return $ case op of 
    "+=" -> Assign var (Bop Plus var val)
    "-=" -> Assign var (Bop Minus var val)
    "*=" -> Assign var (Bop Times var val)
    "/=" -> Assign var (Bop Divide var val)

incAssign :: GenParser Char st Statement
incAssign = do
  optionMaybe $ string "var"
  spaces
  var <- varP
  spaces
  op <- string "++" <|> string "--"
  return $ case op of
    "++" -> Assign var (Bop Plus var (Val (IntVal 1)))
    "--" -> Assign var (Bop Minus var (Val (IntVal 1)))

exprP :: GenParser Char st Expression
exprP = do
  spaces
  exp <- chainl1 parseTermL0 parseOpL0
  spaces
  return exp

parseTermL0 :: GenParser Char st Expression
parseTermL0 = chainl1 parseTermL1 parseOpL1

parseOpL0 :: GenParser Char st (Expression -> Expression -> Expression)
parseOpL0 = do
  spaces
  op <- try (string "&&") <|> try (string "||") <?> "binary operator"
  spaces
  return $ Bop $ transOp op

parseTermL1 :: GenParser Char st Expression
parseTermL1 = chainl1 parseTermL2 parseOpL2

parseOpL1 :: GenParser Char st (Expression -> Expression -> Expression)
parseOpL1 = do
  spaces
  op <- try (string "===")
    <|> try (string "!==")
    <|> try (string "==")
    <|> try (string "!=")
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <?> "binary operator"
  spaces
  return $ Bop $ transOp op

parseTermL2 :: GenParser Char st Expression
parseTermL2 = chainl1 parseTermL3 parseOpL3

parseOpL2 :: GenParser Char st (Expression -> Expression -> Expression)
parseOpL2 = do
  spaces
  ch <- string "+" <|> string "-" <?> "binary operator"
  spaces
  return $ Bop $ transOp ch

parseTermL3 :: GenParser Char st Expression
parseTermL3 = do
  spaces
  e <- unaOpP 
    <|> try(valP)
    <|> try(funcP)
    <|> varP
    <|> parenP
    <?> "value"
  spaces
  return e

unaOpP = do
  ch <- string "!" <?> "unary operator"
  e <- parseTermL3
  return $ Uop (transUnaOp ch) e

transUnaOp s = case s of
  "!"  -> Not
  o    -> error $ "Unexpected operator " ++ o

funcP = do {
  fid <- identifierP; char '('; parms <- exprP `sepBy` (char ','); char ')';
  return $ Func fid parms
}

varP = do
  var <- attrVarP
  return var

attrVarP = do
  e <- chainl1 parseObjAttr parseDotOp
  return e

parseObjAttr = chainl1 parseAry parseBktOp

parseAry = do
  spaces
  var <- many1 (letter <|> digit <|> char '_' <|> char '$')
  spaces
  optional $ string "]"
  spaces
  return $ Var $ StdVar var

parseBktOp = do
  spaces
  ch <- try(string "][") <|> string "[" <?> "bracket notation"
  spaces
  return $ Bop Idx

parseDotOp = do
  spaces
  ch <- string "." <?> "dot notation"
  spaces
  return $ Bop Dot

valP = do
  v <- valP'
  return $ Val v

valP' = do
  spaces
  v <-  try(nullP) 
    <|> try(nanP)
    <|> boolP
    <|> numberP
    <|> stringP1
    <|> stringP2
    <|> arrayP
    <|> objP
  spaces
  return v

nullP = do
  string "null"
  return NullVal

nanP = do
  string "NaN"
  return NaNVal

boolP = do
  bStr <- string "true" <|> string "false" <|> string "skip"
  return $ case bStr of
    "true" -> BoolVal True
    "false" -> BoolVal False
    "skip" -> BoolVal False -- Treating the command 'skip' as a synonym for false, for ease of parsing

numberP = do
  bSign <- optionMaybe $ string "-"
  spaces
  bInt <- many1 digit
  return $ case bSign of
    Just sign -> IntVal $ read $ sign ++ bInt
    Nothing -> IntVal $ read bInt
-- Does not allow single quotes within strings
stringP1 = do {
  char '\''; s <- many $ noneOf "\'"; char '\'';
  return $ StrVal s
}

-- Does not allow double quotes within strings
stringP2 = do {
  char '"'; s <- many $ noneOf "\""; char '"';
  return $ StrVal s
}

arrayP = do {
  char '['; arr <- valP' `sepBy` (char ','); char ']';
  return $ AryVal arr
}

objP = do {
  char '{'; arr <- attrPair `sepBy` (char ','); char '}';
  return $ ObjVal arr
}

attrPair = do {
  spaces; k <- many1 letter; spaces; char ':'; v <- valP'; spaces;
  return $ (k, v)
}

parenP = do {
  string "("; e <- exprP; string ")";
  return e
}

parseOpL3 :: GenParser Char st (Expression -> Expression -> Expression)
parseOpL3 = do
  spaces
  ch <- string "*"
    <|> string "/"
    <?> "binary operator"
  spaces
  return $ Bop $ transOp ch

transOp s = case s of
  "+"  -> Plus
  "-"  -> Minus
  "*"  -> Times
  "/"  -> Divide
  ">=" -> Ge
  ">"  -> Gt
  "<=" -> Le
  "<"  -> Lt
  "==" -> Eq
  "==="-> Seq
  "!=" -> Neq
  "!=="-> Sneq
  "&&" -> And
  "||" -> Or
  o    -> error $ "Unexpected operator " ++ o
