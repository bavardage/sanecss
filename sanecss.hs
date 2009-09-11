import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.List
import Control.Monad.Reader
import qualified Data.Map as Map

data PseudoRule = PseudoRule [PseudoRule] [Declaration]
data Rule = Rule { pseudorules :: [Identifier],
                   selectors :: [Selector], 
                   declarations :: [Declaration] }
data Variable = Variable String
data Declaration = Property ::: Value
data Value = Static String | Dynamic Identifier


type Identifier = String
type Selector = String

type Property = String

indent = "   "

instance Show Variable where --for debugging
    show (Variable v) = "= " ++ v

instance Show Declaration where
    show (p ::: v) = p ++ ": " ++ show v

instance Show Value where
    show (Static v) = v
    show (Dynamic i) = "DYNAMIC!!$" ++ i

instance Show Rule where
    show r = showSelectors r ++ " {\n" ++ showDeclarations r ++ "\n}"
        where
          showSelectors = intercalate ", " . selectors
          showDeclarations = intercalate ";\n" . map ((indent ++) . show) . declarations

type VariableMap = Map.Map Identifier Variable
type PseudoRuleMap = Map.Map Identifier PseudoRule

data SaneCSSItem = V (Identifier, Variable) | P (Identifier, PseudoRule) | R Rule
type SaneCSS = [SaneCSSItem]

evalSaneCss x = runReader 


testPseudoRule1 = PseudoRule [] ["monkeys":::Static "like food"]
testRule1 = Rule ["ape"] ["div.foo", "a:hover"] ["test":::Static "10px", "font":::Dynamic "foo"]
testVariable1 = Variable "arial"

testSaneCss = [P ("ape", testPseudoRule1), V ("foo", testVariable1), R testRule1]