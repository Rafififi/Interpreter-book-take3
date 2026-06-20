module AST where
import Token
import qualified Data.Map as M

type VarMap = M.Map Expression Expression

emptyMap :: VarMap
emptyMap = M.empty

define :: Expression -> Expression -> Bool -> (VarMap, VarMap) -> (VarMap, VarMap)
define name expr True (global, local)  = (global, M.insert name expr local)
define name expr False (global, local) = (M.insert name expr global, local)

get :: Expression -> (VarMap, VarMap)-> Either String Expression
get name (global, local)= case M.lookup name local of
                        Just v  -> Right v
                        Nothing ->  case M.lookup name global of
                          Nothing -> Left $ "Undefined Variable: " <> show name <> "."
                          Just v  -> Right v

updateKey :: Expression -> Expression -> (VarMap, VarMap )-> Either String Environment
updateKey name new (global, local) =  case M.lookup name local of
    Just _    -> Right $ Environment new $ define name new True (global, local)
    Nothing   -> case M.lookup name global of 
      Just _  -> Right $ Environment new $ define name new False (global, local)
      Nothing -> Left $ "Undefined Variable: " <> show name <> "."

merge :: (VarMap, VarMap) -> VarMap
merge (global, local) = local `M.union` global

data Environment = Environment Expression (VarMap, VarMap)
  deriving (Eq)

instance Show Environment where
  show (Environment expr _) = show expr

data Expression = Literal  TokenType
                | Call Expression [Expression]
                | Grouping Expression
                | Variable Expression
                | Assign   Expression Expression
                | Unary {
                    unOp    :: TokenType
                  , unRight :: Expression
                  }
                | Binary {
                    binLeft  :: Expression
                  , binOp    :: TokenType
                  , binRight :: Expression
                  }
                | Logical Expression TokenType Expression
                | Print Expression
                | Var   Expression Expression
                | Block [Expression]
                | If    Expression Expression 
                | IfElse Expression Expression Expression
                | While Expression Expression
                | For (Maybe Expression) (Maybe Expression) (Maybe Expression) Expression
                | Fun Expression [Expression] Expression
                | Ret (Maybe Expression)
                deriving (Eq)

instance Show Expression where
  show (Call callee args)          = show callee <> "(" <> show args <> ")"
  show (Literal token)             = show token
  show (Grouping expr)             = "(" <> show expr <> ")"
  show (Variable token)            = show token
  show (Assign token expr)         = show token <> " = " <> show expr
  show (Unary op right)            = "(" <> show op <> " "  <> show right <> ")"
  show (Binary l op r)             = "(" <> show l <> " " <> show op <> " "  <> show r <> ")"
  show (Logical l op r)            = show l <> " " <> show op <> " " <> show r
  show (Print expr)                = "Print: " <> show expr
  show (Var token expr)            = "VAR: " <> show token <> " = " <> show expr
  show (Block statements)          = "{ " <> show (map show statements) <> " }"
  show (If cond e1)                = "if: " <> show cond <> "{" <> show e1 <> "}"
  show (IfElse cond e1 e2)         = "if: " <> show cond <> "{" <> show e1 <> "} " <> "else {"<>show e2 <> "}"
  show (While cond body)           = "While: (" <> show cond <>  "){" <> show body <> "}"
  show (For decl cond change body) = "For: (" <> show decl <> "; " <> show cond <> show change <>  "){" <> show body <> "}"
  show (Fun name params body)      = "Fn: " <> show name <> "(" <> show params <> "){" <> show body <> "}"
  show (Ret val)                   = "Return: " <> show val

