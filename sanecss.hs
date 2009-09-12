import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.List
import Control.Monad.Reader
import qualified Data.Map as Map

indent = "  "

data Declaration = Property :- Value
type Property = String

data Value = Value String | Var Identifier
type Identifier = String

data Rule = Rule { selectors :: [Selector],
                   extensions :: [Identifier],
                   declarations :: [Declaration] }
type Selector = String

data Extension = Extension { parents :: [Identifier],
                             decs :: [Declaration] }

instance Show Declaration where
    show (p :- v) = indent ++ p ++ ": " ++ show v ++ ";\n"
instance Show Value where
    show (Value x) = x
    show (Var i) = '$' : i
instance Show Rule where
    show r = intercalate ", " (selectors r) 
             ++ " {\n"
             ++ concat (map show $ declarations r) 
             ++ "}\n\n"
instance Show Extension where
    show e = "@" 
             ++ show (parents e) ++ "{\n" 
             ++ concat (map show $ decs e)
             ++ "}\n\n"


type VariableMap = Map.Map Identifier Value
type ExtensionMap = Map.Map Identifier Extension

resolveValue :: VariableMap -> Value -> Value
resolveValue varMap (Value x) = Value x
resolveValue varMap (Var i) = case Map.lookup i varMap of
                                Just v -> resolveValue varMap v --TODO: guard against infinite resolving
                                _ -> error $ "The variable " ++ i ++ " is undefined"

resolveValues :: VariableMap -> [Value] -> [Value]
resolveValues varMap = map (resolveValue varMap)

resolveDeclarations :: VariableMap -> [Declaration] -> [Declaration]
resolveDeclarations varMap = map (\(p :- v) -> p :- resolveValue varMap v)

resolveRule :: (VariableMap, ExtensionMap) -> Rule -> Rule
resolveRule (varMap, eM) r = r {declarations = resolveDeclarations varMap ds}
    where
      ds = (decs $ resolveExtension (varMap, eM) $ Extension (extensions r) [])
           ++ (declarations r)

resolveExtension (varMap, extensionMap) e = 
    e' {decs = resolveDeclarations varMap (decs e')}
       where
         e' = expandExtension extensionMap e
              where
                expandExtension eM e@(Extension [] ds) = e
                expandExtension eM (Extension (e:es) ds) = expandExtension eM $
                                                           Extension es (eds++ds)
                    where
                      eds = case Map.lookup e eM of
                              Just x -> decs $ expandExtension eM x
                              _ -> []


data Expression = V Identifier Value | E Identifier Extension | R Rule

resolveExpressions :: [Expression] -> [Rule]
resolveExpressions es = runReader (resolveExpressionsWith es) (Map.empty, Map.empty)

resolveExpressionsWith [] = return []
resolveExpressionsWith ((V i v):es) = do
  (vM, eM) <- ask
  value <- return $ resolveValue vM v
  local (\(v,e) -> (Map.insert i value v, e)) (resolveExpressionsWith es)
resolveExpressionsWith ((E i e):es) = do
  maps <- ask
  extension <- return $ resolveExtension maps e
  local (\(v,e) -> (v, Map.insert i extension e)) (resolveExpressionsWith es)
resolveExpressionsWith ((R r):es) = do
  maps <- ask
  rule <- return $ resolveRule maps r
  rest <- resolveExpressionsWith es
  return $ rule : rest



testMap = Map.fromList [("darkred", Value "#f90000"), ("background", Var "darkred"), ("blue", Value "#f0f0ff"), ("foreground", Var "yellow"), ("yellow", Value "yellow")]
testDeclarations = ["color" :- Var "darkred",
                    "background-color" :- Var "blue",
                    "border-color" :- Var "foreground"]
testRule = Rule ["div.fancy", "a:hover"] ["thick"] testDeclarations
testExpressions = [V "darkred" $ Value "#f90000",
                   V "blue" $ Value "#f0f0ff",
                   V "foreground" $ Var "blue",
                   E "thick" $ Extension [] ["border-width" :- Value "10px",
                                             "border-color" :- Var "darkred"],
                   R testRule]

testExtnMap = Map.fromList[("thick", Extension [] ["border-width" :- Value "10px"]),
                           ("red", Extension [] ["color" :- Value "red"]),
                           ("thinknred", Extension ["thick", "red"] ["font-weight" :- Value "strong"])]

------------------------------------------------------------
-- Parser                                                 --
------------------------------------------------------------

whitespace :: Parser ()
whitespace = do
  many (oneOf " \n\t" <?> "whitespace")
  return ()

parseProperty :: Parser Property
parseProperty = do
  many1 (letter <|> char '-') <?> "property"

parseDeclaration :: Parser Declaration
parseDeclaration = do
  property <- parseProperty
  whitespace
  char ':'
  whitespace
  value <- parseValue
  return $ property :- value
  
parseIdentifier :: Parser Identifier
parseIdentifier = do
  many1 letter <?> "identifier"

parseValue :: Parser Value
parseValue = do
  parseVariable <|> parseStaticValue
      where
        parseVariable = do
               char '$'
               identifier <- parseIdentifier
               char ';'
               return $ Var identifier
        parseStaticValue = do
               value <- many (noneOf ";")
               char ';'
               return $ Value value

parseSelector :: Parser Selector
parseSelector = do
  many1 (alphaNum <|> oneOf " >#.*+[]=~|^$():-") <?> "selector"

parseSelectors :: Parser [Selector]
parseSelectors = do
  parseSelector `sepBy` (char ',' >> whitespace)

parseRule :: Parser Rule
parseRule = do
  selectors <- parseSelectors
  whitespace >> char '{' >> whitespace
  declarations <- parseDeclaration `sepEndBy` whitespace
  char '}'
  return $ Rule selectors [] declarations

test = putStr $ concatMap show $ resolveExpressions testExpressions


run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

tRule = "\
\h1, h2, #container h3, a:hover { \
\  color: $darkred; \ 
\  font-family: 'Helvetica'; \
\}"
