{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter(interpret) where
import GHC.IO (unsafePerformIO)
import AST (Expression (Literal, Grouping, Unary, Binary, Print, Var, Assign, Variable, Block, If, IfElse, Logical, While, Fun, Call, ArgsList, Ret), Context (Context), get, emptyMap, VarMap, define, updateKey)
import Token (TokenType(IDENTIFIER, STRING, NUMBER, MINUS, NIL, BOOLEAN, BANG, SLASH, STAR, PLUS, GREATER, GREATEREQUAL, LESS, LESSEQUAL, EQUALEQUAL, BANGEQUAL, OR, AND), TokenInfo (TokenInfo), showLocation)

printUnsafe :: Show a => a -> ()
printUnsafe x = unsafePerformIO $ print x

nilLiteral :: Expression
nilLiteral = Literal $ TokenInfo 0 0 NIL

evaluateBlock :: [Expression] -> [VarMap] -> NewContext Context
evaluateBlock [] vars = Success (Context nilLiteral (tail vars)) 
evaluateBlock (expr:xs) vars = case evaluate $ Context expr vars of
  Fail err -> Fail err
  Success (Context _ vars) -> evaluateBlock xs vars
  Return (Context _ vars) ret -> Return (Context nilLiteral (tail vars)) ret


evaluateList :: [Expression] -> [VarMap] -> NewContext Context
evaluateList [] vars = Success (Context nilLiteral vars) 
evaluateList (expr:xs) vars = case evaluate $ Context expr vars of
  Fail err -> Fail err
  Success (Context _ vars) -> evaluateBlock xs vars
  Return (Context _ vars) ret -> Return (Context nilLiteral vars) ret


evaluate :: Context -> NewContext Context
evaluate (Context (Literal x) vars) = case x of
  str@(TokenInfo _ _ (STRING _))         -> Success (Context (Literal str) vars) 
  num@(TokenInfo _ _ (NUMBER _))         -> Success (Context (Literal num) vars)
  bool@(TokenInfo _ _ (BOOLEAN _))       -> Success (Context (Literal bool) vars)
  nil@(TokenInfo _ _ NIL)                -> Success (Context (Literal nil) vars)
  name@(TokenInfo _ _ (IDENTIFIER _))      -> case get (Literal name) vars of
                                            Left err -> Fail err
                                            Right res -> Success (Context res vars) 
  _                                      -> Fail "Called evaluate on literal but this is not a literal"
evaluate (Context (Grouping x) vars) = evaluate (Context x vars)
evaluate (Context (Unary (TokenInfo r c MINUS) (Literal (TokenInfo _ _ (NUMBER x)))) vars) = Success $ Context (Literal $ TokenInfo r c (NUMBER $ -x)) vars
evaluate (Context (Unary (TokenInfo r c BANG) expr) vars) = Success $ Context (Literal $ TokenInfo r c (BOOLEAN $ isTruthy expr)) vars
evaluate (Context (Var name@(Literal (TokenInfo _ _ (IDENTIFIER _))) expr) vars) = case evaluate $ Context expr vars of
                                        Fail err                           -> Fail err
                                        Success (Context res vars)         -> Success (Context nilLiteral $ define name res vars)
                                        Return (Context _ vars) Nothing    -> Return (Context nilLiteral $ define name nilLiteral vars) Nothing
                                        Return (Context _ vars) (Just ret) -> Return (Context nilLiteral $ define name ret vars) (Just ret)
evaluate (Context (Variable name@(Literal (TokenInfo _ _ (IDENTIFIER _)))) vars) = Success (Context nilLiteral $ define name nilLiteral vars)
evaluate (Context (Assign (Variable name@(Literal (TokenInfo _ _ (IDENTIFIER _)))) expr) vars) = case evaluate $ Context expr vars of
                                        Fail err                           -> Fail err
                                        Success (Context res vars)         -> assignToContext name res vars
                                        Return (Context _ vars) Nothing    -> successToReturn $ assignToContext name nilLiteral vars
                                        Return (Context _ vars) (Just ret) -> successToReturn $ assignToContext name ret vars
              where assignToContext name val vars = case updateKey name val vars of
                                                                    Left str -> Fail str
                                                                    Right vars -> Success $ Context nilLiteral vars
                    successToReturn (Success (Context ret vars)) = Return (Context ret vars) (Just ret)
                    successToReturn  x = x
evaluate (Context (Print expr) vars) = case evaluate $ Context expr vars of
                                        Fail err -> Fail err
                                        Success (Context res vars) -> do 
                                                                        printUnsafe res `seq`
                                                                          Success (Context (Literal $ TokenInfo 0 0 NIL) vars)
                                        Return (Context _ vars) ret -> do
                                                                        printUnsafe ret `seq`
                                                                          Return (Context (Literal $ TokenInfo 0 0 NIL) vars) ret
evaluate (Context (Logical left (TokenInfo _ _ op)right) vars) = do
  let leftRes = case evaluate $ Context left vars of
                  Fail err                           -> Left err
                  Success (Context x vars)           -> Right (x, vars)
                  Return (Context _ _ ) Nothing      -> Left "return value of function was null, can't do binary opertaion with that"
                  Return (Context _ vars) (Just ret) -> Right (ret, vars)
  case leftRes of 
    Left err -> Fail err
    Right (res, vars) -> case op of
                          OR  -> if isTruthy res then Success (Context res vars) else evaluate $ Context right vars
                          AND -> if not $ isTruthy res then Success (Context (Literal $ TokenInfo 0 0 (BOOLEAN False)) vars) else evaluate $ Context right vars
                          _   -> Fail "Logical operation can only be done with AND/OR"

evaluate (Context (Binary left op right) vars) = do
  let leftRes = case evaluate $ Context left vars of
                  Fail err                           -> Left err
                  Success (Context x vars)           -> Right (x, vars)
                  Return (Context _ _ ) Nothing      -> Left "return value of function was null, can't do binary opertaion with that"
                  Return (Context _ vars) (Just ret) -> Right (ret, vars)
  let rightRes = case leftRes of
                  Left _ -> Left "left has failed do not evaluate right"
                  Right (_ , vars) -> case evaluate $ Context right vars of
                                        Fail err                           -> Left err
                                        Success (Context x vars)           -> Right (x, vars)
                                        Return (Context _ _ ) Nothing      -> Left "return value of function was null, can't do binary opertaion with that"
                                        Return (Context _ vars) (Just ret) -> Right (ret, vars)
  case (leftRes, rightRes) of
    (Left leftErr, _) -> Fail $ "left of binary opertaion failed Left: " <> show leftErr
    (_ , Left rightErr) -> Fail $ "right of binary opertaion failed Right: " <> rightErr
    (Right (Literal (TokenInfo r c (STRING x)), _), Right (Literal (TokenInfo _ _ (STRING y)), vars)) -> case op of
                                                                                  (TokenInfo _ _ PLUS) -> Success $ Context (Literal $ TokenInfo r c (STRING (x<>y))) vars 
                                                                                  (TokenInfo _ _ EQUALEQUAL) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x==y)) vars 
                                                                                  (TokenInfo _ _ BANGEQUAL) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x/=y)) vars 
                                                                                  tok -> Fail $ "Binary operation failed can not be applied to 2 strings " <> showLocation tok
    (Right (Literal (TokenInfo r c (NUMBER x)), _), Right (Literal tok@(TokenInfo _ _ (NUMBER y)), vars)) -> case op of
                                                                                  (TokenInfo _ _ MINUS) -> Success $ Context (Literal $ TokenInfo r c $ NUMBER (x-y)) vars 
                                                                                  (TokenInfo _ _ SLASH) -> if y /= 0 then 
                                                                                                              Success $ Context (Literal $ TokenInfo r c $ NUMBER (x/y)) vars 
                                                                                                          else Fail $ "Division by zero " <> showLocation tok
                                                                                  (TokenInfo _ _ STAR) -> Success $ Context (Literal $ TokenInfo r c $ NUMBER (x*y)) vars 
                                                                                  (TokenInfo _ _ PLUS) -> Success $ Context (Literal $ TokenInfo r c $ NUMBER (x+y)) vars 
                                                                                  (TokenInfo _ _ GREATER) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x>y)) vars 
                                                                                  (TokenInfo _ _ GREATEREQUAL) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x>=y)) vars 
                                                                                  (TokenInfo _ _ LESS) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x<y)) vars 
                                                                                  (TokenInfo _ _ LESSEQUAL) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x<=y)) vars 
                                                                                  (TokenInfo _ _ EQUALEQUAL) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x==y)) vars 
                                                                                  (TokenInfo _ _ BANGEQUAL) -> Success $ Context (Literal $ TokenInfo r c $ BOOLEAN (x/=y)) vars 
                                                                                  tok -> Fail $ "Binary operation failed can not be applied to 2 numbers " <> showLocation tok
    (Right x, Right y) -> case op of 
                            (TokenInfo _ _ EQUALEQUAL) -> Success $ Context (Literal $ TokenInfo 0 0 $ BOOLEAN (x==y)) vars 
                            (TokenInfo _ _ BANGEQUAL) -> Success $ Context (Literal $ TokenInfo 0 0 $ BOOLEAN (x/=y)) vars 
                            tok -> Fail $ "Binary operation failed can not be applied 2 things " <> showLocation tok
                          
    

evaluate (Context (Block exprs) vars) = evaluateBlock exprs $ emptyMap : vars
evaluate (Context (If cond stat) vars) = case evaluate $ Context cond vars of
                                          Fail err -> Fail err
                                          Success (Context res vars) -> if isTruthy res then evaluate $ Context stat vars else Success (Context nilLiteral vars)
                                          Return (Context _ vars) Nothing -> Return (Context nilLiteral vars) Nothing
                                          Return (Context _ vars) (Just ret) -> if isTruthy ret then evaluate $ Context stat vars else Return (Context nilLiteral vars) (Just ret)
evaluate (Context (IfElse cond ifStat elseStat) vars) = case evaluate $ Context cond vars of
                                          Fail err -> Fail err
                                          Success (Context res vars) ->  evaluate $ Context (ifElse res ifStat elseStat) vars 
                                          Return (Context _ vars) Nothing -> Return (Context nilLiteral vars) Nothing
                                          Return (Context _ vars) (Just ret) -> evaluate $ Context (ifElse ret ifStat elseStat) vars
                                          where ifElse res ifStat elseStat = if isTruthy res then ifStat else elseStat
evaluate (Context (While cond whileBlock) vars) = case evaluate $ Context cond vars of
                                          Fail err                           -> Fail err
                                          Success (Context res vars)         -> if not $ isTruthy res then Success $ Context nilLiteral vars else evaluateWhile whileBlock vars
                                          Return (Context _ vars) Nothing    -> Return (Context nilLiteral vars) Nothing
                                          Return (Context _ vars) (Just ret) -> if not $ isTruthy ret then Return (Context nilLiteral vars) (Just ret) else evaluateWhile whileBlock vars
    where evaluateWhile whileBlock vars = case evaluate $ Context whileBlock vars of
                                            Fail err -> Fail err
                                            Success (Context _ vars) -> evaluate (Context (While cond whileBlock) vars)
                                            Return (Context _ vars) Nothing -> Return (Context nilLiteral vars) Nothing
                                            Return (Context _ vars) (Just ret) -> Return (Context nilLiteral vars) (Just ret)
evaluate (Context (Call func@(Literal funcTok@(TokenInfo _ _ (IDENTIFIER _))) variables) vars) = 
  case get func vars of
    Left err                           -> Fail $ err <> show vars
    Right (ArgsList args (Block body)) -> if length variables /= length args  
                                          then Fail $ "Function call has " <> show (length variables) <> " arguments instead of " <> show (length args) <> " Location: " <> showLocation funcTok
                                          else case foldl argToName (Right $ emptyMap : vars) (zip args variables) of
                                              Left err -> Fail err
                                              Right vars -> case evaluateList body vars of
                                                              Fail err                           -> Fail err
                                                              Success x                          -> Success x
                                                              Return (Context _ vars) Nothing    -> Success (Context nilLiteral vars)
                                                              Return (Context _ vars) (Just ret) -> Success (Context ret vars)
    Right _ -> Fail $ "call to function does not have valid function "<> show func <> " Location " <> showLocation funcTok
    where argToName (Left acc) (_ , _) = Left acc
          argToName (Right acc) (argName, argCall) = case evaluate $ Context argCall acc of
                                                Fail err                            -> Left err
                                                Success (Context res vars)          -> Right $ define argName res vars
                                                Return (Context _ vars) Nothing     -> Right $ define argName nilLiteral vars
                                                Return (Context _ vars) (Just ret)  -> Right $ define argName ret vars
evaluate (Context (Fun name@(Literal (TokenInfo _ _ (IDENTIFIER _))) variables body) vars) =
                                                          let varsWithFunc = define name (ArgsList variables body) vars
                                                          in Success (Context nilLiteral varsWithFunc)
evaluate (Context (Ret (Just expr)) vars) = case evaluate $ Context expr vars of
                                              Fail err -> Fail err
                                              Success (Context res vars) -> Return (Context nilLiteral vars) $ Just res
                                              Return (Context _ vars) ret -> Return (Context nilLiteral vars) ret
evaluate (Context (Ret Nothing) vars) = Return (Context nilLiteral vars) Nothing
evaluate _ = Fail "This has not been implemented yet"
                                                
isTruthy :: Expression -> Bool
isTruthy (Literal (TokenInfo _ _ NIL)) = False
isTruthy (Literal (TokenInfo _ _ (BOOLEAN False))) = False
isTruthy _ = True
 
interpret' :: [Expression] -> [VarMap] -> String 
interpret' [] _ = ""
interpret' (expression:xs) vars = do
  case evaluate $ Context expression vars  of
    Fail err                  -> err
    Success (Context _ vars)  -> interpret' xs vars
    Return (Context _ vars) _ -> interpret' xs vars
 
interpret :: [Expression] -> String
interpret expressions = interpret' expressions [emptyMap]

data NewContext a = Fail String
                | Success a
                | Return a (Maybe Expression)
