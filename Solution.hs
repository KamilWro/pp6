-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko} gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module Solution (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes
import Data.Map as Map
import Data.Maybe as Maybe

type Environment a = Map String a
type Error p = (p,ErrorKind) 

-- Wartość zwracana
data Value p
  = VInt Integer
  | VBool Bool
  | VUnit
  | VPair (Value p) (Value p)
  | VList (Value p) (Value p)
  | VNil
  | VArrow Var (Expr p) (Environment (Value p))
  deriving (Eq)

-- Rodzaje błedów
data ErrorKind
  = EUndefinedVariable Var 
  | ETypeMismatch Type Type
  | EFunMismatch Type Type 
  | EIfMismatch Type Type
  | EMatchMismatch Type Type
  | EListMismatch Type Type
  | EUndefinedFunction
  | EPairSigMismatch Type
  | EListSigMismatch Type  
  
-- Wypisywanie błędu
instance Show ErrorKind where
  show (EUndefinedVariable x) =
    "Undefined variable " ++ show x ++ "."
  show (ETypeMismatch t1 t2)  =
    "Type mismatch: exprected " ++ show t1 ++ " but received " ++ show t2 ++ "."
  show (EFunMismatch t1 t2)  =
    "Unexprected return value. Expected " ++ show t1 ++ " but received " ++ show t2 ++ "."
  show (EIfMismatch t1 t2)    =
    "Type mismatch in the branches of an if: " ++ show t1 ++ " and " ++ show t2 ++ "."
  show (EMatchMismatch t1 t2)    =
    "Type mismatch in the branches of an match: " ++ show t1 ++ " and " ++ show t2 ++ "."    
  show (EListMismatch t1 t2)    =
    "The list must be of one type: Expected " ++ show t1 ++ " but received " ++ show t2 ++ "."
  show EUndefinedFunction =
    "Undefined function."
  show (EPairSigMismatch t) =
    "Type mismatch: exprected Pair but received " ++ show t ++ "."
  show (EListSigMismatch t) =
    "Type mismatch: exprected List but received " ++ show t ++ "."  
    
     
-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck fs vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck functions vars expr= 
  case checkFunctions of
    []    ->
      case checkMain of
        Right TInt                    -> Ok
        Left (p,errorMessage)         -> Error p $ show errorMessage
        Right t                       -> Error (getData expr) (show $ ETypeMismatch TInt t) 
    ((Just (p,errorMessage)):_)       -> Error p $ show errorMessage
  where
    gamma  = Map.fromList [(x,TInt)|x<-vars]
    func   = Map.fromList [(name,TArrow argT resT)|(FunctionDef p name _ argT resT _)<-functions]
    checkFunctions= Prelude.filter(Maybe.isJust)(Prelude.map (ftypecheck func) functions)
    checkMain = infer_type (union gamma func) expr
  
-- Funkcja sprawdzająca typy funkcji
ftypecheck :: Environment Type-> FunctionDef p -> Maybe (Error p)
ftypecheck gamma (FunctionDef _ _ fArg fArgT fResT fBody) = 
  case checkFunction of
    Right t        -> if t==fResT then Nothing else Just ((getData fBody),EFunMismatch fResT t)
    Left error     -> Just error
  where
    checkFunction= infer_type (Map.insert fArg fArgT gamma) fBody

-- Funkcja porównująca typ zwracany z oczekiwanym
check :: Environment Type -> Expr p -> Type -> Either (Error p) ()
check gamma expr expT=
  do
    it <- infer_type gamma expr
    if (expT==it) then return () else Left ((getData expr),ETypeMismatch expT it)
      
-- Funkcja sprawdzająca typ wyrażenia
infer_type:: Environment Type -> Expr p -> Either (Error p) Type
infer_type gamma (EVar p var) = 
  case Map.lookup var gamma of
    Just x  -> Right x
    Nothing -> Left (p,EUndefinedVariable var)
infer_type _ (ENum _ _)  = return TInt
infer_type _ (EBool _ _) = return TBool
infer_type _ (EUnit _ ) = return TUnit
infer_type gamma (EUnary _ operator expr)=
  do
    check gamma expr t
    return tr
  where
    (t,tr)
      | operator==UNot =
        (TBool,TBool)
      | operator==UNeg =
        (TInt,TInt)
 
infer_type gamma (EBinary _ operator expr1 expr2)=
  do
    check gamma expr1 t1
    check gamma expr2 t2
    return tr
  where
    (t1,t2,tr) 
      | elem operator [BAdd, BSub, BMul, BDiv, BMod ] =
          (TInt,TInt,TInt)
      | elem operator [BEq,BNeq, BLt, BGt, BLe, BGe] =
          (TInt,TInt,TBool)
      | elem operator [BAnd, BOr] =
          (TBool,TBool,TBool) 

infer_type gamma (ELet _ var expr1 expr2) =
  do
    x <- infer_type gamma expr1
    let gamma2= Map.insert var x gamma
    infer_type gamma2 expr2  

infer_type gamma (EIf p expr1 expr2 expr3) =
  do
    check gamma expr1 TBool
    x<-type_expr expr2
    y<-type_expr expr3
    if x==y then return y else Left (p, EIfMismatch x y)
  where
    type_expr expr=infer_type gamma expr

infer_type gamma (EPair _ expr1 expr2) =
  do
    x<-type_expr expr1
    y<-type_expr expr2
    return $ TPair x y
  where
    type_expr expr=infer_type gamma expr
    
infer_type gamma (EFst _ expr) =
  do
    x<-type_expr
    case x of
      (TPair t1 _) -> return t1
      t1        -> Left (getData expr,EPairSigMismatch t1 )
  where
    type_expr=infer_type gamma expr
      
infer_type gamma (ESnd _ expr) =
  do
    x<-type_expr
    case x of
      (TPair _ t2) -> return t2
      t2        -> Left (getData expr,EPairSigMismatch t2 )
  where
    type_expr=infer_type gamma expr
    
infer_type _ (ENil p t) = 
  case t of
    TList t1  -> return t
    t1       -> Left (p,EListSigMismatch t1 )
    
infer_type gamma (ECons p (expr1) (expr2)) =
  do
    x<-type_expr expr2
    t1 <- type_expr expr1
    case x of
      TList t2 ->
        do
          if t1==t2 then return $ TList t1 else Left (p,EListMismatch t1 t2)
      t2       -> Left (getData expr2,ETypeMismatch (TList t1) t2 )
  where
    type_expr expr=infer_type gamma expr
    
infer_type gamma (EMatchL p expr1 expr2 (var1,var2,expr3)) =
  do
    x<-type_expr expr1
    case x of
      TList t1  -> isList t1
      t1        -> Left((getData expr1),EListSigMismatch t1)
  where
    isList t= 
      do
        t2<-type_expr expr2
        t3<- (let gamma2 = Map.insert var2 (TList t) (Map.insert var1 t gamma) in infer_type gamma2 expr3)
        if t3==t2 then return t2 else Left(p,EMatchMismatch t2 t3)
    type_expr expr=infer_type gamma expr

infer_type gamma (EFn p var t expr)  =
  do
    x<-type_expr 
    return $ TArrow  t x
  where
    type_expr=infer_type (Map.insert var t gamma) expr

infer_type gamma (EApp p expr1 expr2)=
  do
    x<-type_expr expr1
    y<-type_expr expr2
    case x of
      TArrow argT resT  -> 
        do
          check gamma expr2 argT
          return resT
      _                 -> Left (p,EUndefinedFunction)
  where
    type_expr expr=infer_type gamma expr 
  
   
 
-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval fs input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że definicje funckcji fs oraz wyrażenie e są dobrze
-- typowane, tzn. typecheck fs (map fst input) e = Ok
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval functions list expr=
  case res of
    Just (VInt x)  -> Value x
    _              -> RuntimeError
  where 
    vars= Map.fromList [(var,VInt value)|(var,value)<-list] 
    func = Map.fromList [(name,VArrow arg body func)|(FunctionDef p name arg _ _ body)<-functions]
    res= calc (union vars func) expr
    
-- Funkcja obliczająca wyrazenie
calc :: Environment (Value p)-> Expr p -> Maybe (Value p)

calc gamma (EVar _ var)    = Map.lookup var gamma
calc _ (ENum _ x)          = return (VInt x)
calc _ (EBool _ x)         = return (VBool x)
calc _ (EUnit _ )          = return VUnit
calc gamma (EPair _ expr1 expr2)=
  do
    v1<-res_expr expr1
    v2<-res_expr expr2
    return $ VPair v1 v2
  where
    res_expr expr = calc gamma expr
calc gamma (EFst p expr)=
  do
    (VPair v1 _)<-res_expr
    return v1    
  where 
    res_expr= calc gamma expr
calc gamma (ESnd _ expr)=
  do
    (VPair _ v2)<-res_expr
    return v2
  where 
    res_expr= calc gamma expr
calc gamma (ENil _ _ )= return VNil
calc gamma (ECons _ expr1 expr2) =
  do
    v1<-res_expr expr1
    v2<-res_expr expr2
    return$ VList v1 v2
  where
    res_expr expr = calc gamma expr
calc gamma (EUnary _ operator expr) 
  | operator==UNeg =
    do
      (VInt x)<-res_expr
      return $ VInt (-x)
  | operator==UNot =
    do
      (VBool x)<-res_expr
      return $ VBool (not x)
  where
    res_expr = calc gamma expr
calc gamma (EBinary _ operator expr1 expr2)
  | elem operator [BEq,BNeq, BLt, BGt, BLe, BGe]=
    do
      (VInt x)<- res_expr expr1
      (VInt y)<- res_expr expr2
      return $ VBool(comparison x y)
  | elem operator [BAnd, BOr]=
    do
      (VBool x) <- res_expr expr1
      (VBool y) <- res_expr expr2
      return $ VBool(boolean x y)
  | elem operator [BAdd, BSub, BMul,BDiv, BMod ] =
    do
      (VInt x)<- res_expr expr1
      (VInt y)<- res_expr expr2
      if y==0 && (elem operator [BDiv, BMod ]) then Nothing else return $ VInt(arithmetic x y)
  where
    res_expr expr= calc gamma expr
    comparison x y
      | operator==BGe = x>=y
      | operator==BLe = x<=y
      | operator==BGt = x>y
      | operator==BLt = x<y
      | operator==BNeq= x/=y
      | operator==BEq = x==y
    arithmetic x y
      | operator==BMod = x`mod`y
      | operator==BMul = x*y
      | operator==BDiv = x`div`y
      | operator==BSub = x-y
      | operator==BAdd = x+y
    boolean x y
      | operator==BOr  = x||y
      | operator==BAnd = x&&y
      
calc gamma (ELet _ var expr1 expr2) =
  do 
    x<- calc gamma expr1
    calc (gamma2 x) expr2
  where 
    gamma2 x= Map.insert var x gamma
    
calc gamma (EIf _ expr1 expr2 expr3) =
  do
    (VBool x) <- res_expr expr1
    if x==True then res_expr expr2 else res_expr expr3 
  where
    res_expr expr= calc gamma expr

calc gamma (EMatchL _ expr1 expr2 (var1, var2, expr3)) =
  do
    x<-res_expr expr1
    case x of
      VNil  -> res_expr expr2
      _     -> 
        do
          let (VList t1 t2)=x
          let gamma2 =  Map.insert var2 t2 (Map.insert var1 t1 gamma)
          calc gamma2 expr3
  where
    res_expr expr = calc gamma expr
calc gamma (EFn _ var _ expr) = return $ VArrow var expr gamma
calc gamma (EApp _ expr1 expr2 ) =
  do
    VArrow var expr lGamma <-calc gamma expr1
    x<- calc gamma expr2
    calc (Map.insert var x lGamma) expr
