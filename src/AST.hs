{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AST where
import Token
import qualified Data.Map as M

type VarMap = M.Map Expression Expression

emptyMap :: VarMap
emptyMap = M.empty

define :: Expression -> Expression ->  [VarMap] -> [VarMap]
define _ _ []  = []
define name expr (local:xs)  = M.insert name expr local : xs


get :: Expression -> [VarMap] -> Either String Expression
get name [] = Left $ "Undefined Variable: " <> show name <> "."
get name (local:xs) = case M.lookup name local of
                        Just v  -> Right v
                        Nothing -> get name xs

nilLiteral :: Expression
nilLiteral = Literal $ TokenInfo 0 0 NIL

updateKey :: Expression -> Expression -> [VarMap] -> Either String [VarMap]
updateKey name _ [] = Left $ "Undefined Variable: " <> show name <> "."
updateKey name new (local:xs) =  case M.lookup name local of
    Just _    -> Right $ define name new [local]
    Nothing   -> case updateKey name new xs of
              Left err -> Left err
              Right newMap -> Right $ local : newMap

merge :: (VarMap, VarMap) -> VarMap
merge (global, local) = local `M.union` global

data Context = Context { expression :: Expression 
                       , varMap :: [VarMap]
                       } deriving (Eq)

instance Show Context where
  show (Context expr _) = show expr

data Expression = Literal  TokenInfo
                | Call Expression [Expression]
                | Grouping Expression
                | Variable Expression
                | Assign   Expression Expression
                | Unary {
                    unOp    :: TokenInfo
                  , unRight :: Expression
                  }
                | Binary {
                    binLeft  :: Expression
                  , binOp    :: TokenInfo
                  , binRight :: Expression
                  }
                | Logical Expression TokenInfo Expression
                | Print Expression
                | Var   Expression Expression
                | Block [Expression]
                | If    Expression Expression 
                | IfElse Expression Expression Expression
                | While Expression Expression
                | For (Maybe Expression) (Maybe Expression) (Maybe Expression) Expression
                | Fun Expression [Expression] Expression
                | Ret (Maybe Expression)
                | ArgsList [Expression] Expression
                deriving (Eq, Ord)

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
  show (ArgsList name args)        = "name: " <> show name <> " Args: "  <> show args

class Error a where
  err :: a -> String

instance Error Expression where
  err (Call callee args)          = show callee <> "(" <> show args <> ")"
  err (Literal token)             = show token
  err (Grouping expr)             = "(" <> show expr <> ")"
  err (Variable token)            = show token
  err (Assign token expr)         = show token <> " = " <> show expr
  err (Unary op right)            = "(" <> show op <> " "  <> show right <> ")"
  err (Binary l op r)             = "(" <> show l <> " " <> show op <> " "  <> show r <> ")"
  err (Logical l op r)            = show l <> " " <> show op <> " " <> show r
  err (Print expr)                = "Print: " <> show expr
  err (Var token expr)            = "VAR: " <> show token <> " = " <> show expr
  err (Block statements)          = "{ " <> show (map show statements) <> " }"
  err (If cond e1)                = "if: " <> show cond <> "{" <> show e1 <> "}"
  err (IfElse cond e1 e2)         = "if: " <> show cond <> "{" <> show e1 <> "} " <> "else {"<>show e2 <> "}"
  err (While cond body)           = "While: (" <> show cond <>  "){" <> show body <> "}"
  err (For decl cond change body) = "For: (" <> show decl <> "; " <> show cond <> show change <>  "){" <> show body <> "}"
  err (Fun name params body)      = "Fn: " <> show name <> "(" <> show params <> "){" <> show body <> "}"
  err (Ret val)                   = "Return: " <> show val
  err (ArgsList name args)        = "name: " <> show name <> " Args: "  <> show args
