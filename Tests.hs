-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
module Tests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  -- Programy poprawnie otypowane. Podstawowe operacje
  [ Test { testName    = "positivIntTest",
           testProgram = SrcString "(fn (x:unit)-> 123) ()",
           testAnswer  = Eval [] (Value 123)
         },
    Test { testName    = "emptyListTest",
           testProgram = SrcString "fun f(x:int list): int = 1 in f([]:int list)",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "listTest",
           testProgram = SrcString " fun f(x:bool list): int = 2 in f([true,false]:bool list)",
           testAnswer  = Eval [] (Value 2)
         },         
    Test { testName    = "negativIntTest",
           testProgram = SrcString "(fn (x:unit)-> -1) ()",
           testAnswer  = Eval [] (Value (-1))
         },
    Test { testName    = "addTest",
           testProgram = SrcString "(fn (x:unit)-> 2+3) ()",
           testAnswer  = Eval [] (Value 5)
         },
    Test { testName    = "subTest",
           testProgram = SrcString "fun f(x:unit): int = 2-2 in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "mulTest",
           testProgram = SrcString "fun f(x:unit): int = 2*8 in f()",
           testAnswer  = Eval [] (Value 16)
         },
    Test { testName    = "divTest",
           testProgram = SrcString "fun f(x:unit): int = 7 div 2 in f()",
           testAnswer  = Eval [] (Value 3)
         },         
    Test { testName    = "modTest",
           testProgram = SrcString "(fn (x:unit)-> 8 mod 3) ()",
           testAnswer  = Eval [] (Value 2)
         },
    Test { testName    = "letTest",
           testProgram = SrcString "fun f(x:unit): int = let y=1 in y in f() ",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "letLambdaTest",
           testProgram = SrcString "(fn (x:unit)-> let x=1 in 43-x) () ",
           testAnswer  = Eval [] (Value 42)
         },         
    Test { testName    = "envInputTest",
           testProgram = SrcString "input x in x",
           testAnswer  = Eval [2] (Value 2)
         },
    Test { testName    = "envFuncTest",
           testProgram = SrcString "fun f(x:int): int = x in f(1) ",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifTrueTest",
           testProgram = SrcString "fun f(x:unit): int = if true then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifFalseTest",
           testProgram = SrcString "(fn (x:unit)-> if false then 1 else 0) ()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "ifEqTest",
           testProgram = SrcString "fun f(x:unit): int = if 2=2 then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifNeqTest",
           testProgram = SrcString "(fn (x:unit)-> if 2<>2 then 1 else 0) ()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "ifLtTest",
           testProgram = SrcString "fun f(x:unit): int = if 2<3 then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifGtTest",
           testProgram = SrcString "fun f(x:unit): int = if 2>3 then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "ifLeAndTrueTest",
           testProgram = SrcString "(fn (x:unit)-> if (1<=2) and (2<=2) then 1 else 0) ()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifGeAndTrueTest",
           testProgram = SrcString "fun f(x:unit): int = if (3>=3) and (3>=2) then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifAndFalseTrueTest",
           testProgram = SrcString "(fn (x:unit)->  if false and true then 1 else 0) ()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "ifAndTrueFalseTest",
           testProgram = SrcString "fun f(x:unit): int = if true and false then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "ifAndTrueTest",
           testProgram = SrcString "fun f(x:unit): int = if true and true then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },     
    Test { testName    = "ifOrTrueFalseTest",
           testProgram = SrcString "fun f(x:unit): int = if true or false then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },    
    Test { testName    = "ifOrFalseTrueTest",
           testProgram = SrcString "fun f(x:unit): int = if false or true then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },    
    Test { testName    = "ifOrFalseTest",
           testProgram = SrcString "fun f(x:unit): int = if false or false then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 0)
         },    
    Test { testName    = "ifOrTrueTest",
           testProgram = SrcString "(fn (x:unit)-> if true or true then 1 else 0 ) ()",
           testAnswer  = Eval [] (Value 1)
         }, 
    Test { testName    = "ifNotTest",
           testProgram = SrcString "fun f(x:unit): int = if not true then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "fstTest",
           testProgram = SrcString "fun f(x:unit): int*int = (1,2) in fst(f())",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "sndTest",
           testProgram = SrcString "fun f(x:unit): int*int = (1,0) in snd(f())",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "lambdaTest",
           testProgram = SrcString "(fn(x:int)->x+5) 10",
           testAnswer  = Eval [] (Value 15)
         }                                  
  ]
  ++
  -- Programy poprawnie otypowane. Skrajne przypadki
  [
    Test { testName    = "divTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = 0 div 10 in f()",
           testAnswer  = Eval [] (Value 0)
         },  
    Test { testName    = "letIntTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = let y=1 in let y=2 in let y=42 in y-1 in f()",
           testAnswer  = Eval [] (Value 41)
         },
    Test { testName    = "let_ifBoolTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = let x=(if true then false else true) in if x then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "if_letBoolTrueExtreme",
           testProgram = SrcString "fun f(x:unit): int = if (let x=true in x) then (let x=1 in x)  else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "if_letBoolFalseExtreme",
           testProgram = SrcString "fun f(x:unit): int = if (let x=false in x) then 1  else (let x=0 in x) in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "letVarTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = let x=1 in let y=x in let z=y in x-z in f()",
           testAnswer  = Eval [] (Value 0)
         },
    Test { testName    = "envTestExtreme",
           testProgram = SrcString "input x in let x=42 in x ",
           testAnswer  = Eval [42] (Value 42)
         },
    Test { testName    = "envGeneralTestExtreme",
           testProgram = SrcString "fun f(f:int):int= f in let f=f 22 in (fn(f:int)->f) f",
           testAnswer  = Eval [] (Value 22)
         },         
    Test { testName    = "envFuncTestExtreme",
           testProgram = SrcString "fun f(x:int): int = let x=1 in x input x in f(x) ",
           testAnswer  = Eval [2] (Value 1)
         },
    Test { testName    = "ifFalseTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = if false then 1 div 0 else 1 in f()",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "ifTrueTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = if true then 41 else 1 div 0 in f()",
           testAnswer  = Eval [] (Value 41)
         },
    Test { testName    = "ifTrueTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = if true then 41 else 1 div 0 in f()",
           testAnswer  = Eval [] (Value 41)
         },
    Test { testName    = "ifNotFalseTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = if  not not not true then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 0)
         }, 
    Test { testName    = "ifNotTrueTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = if not not not not true then 1 else 0 in f()",
           testAnswer  = Eval [] (Value 1)
         }, 
    Test { testName    = "ifNeqOddTestExtreme",
           testProgram = SrcString "fun f(x:unit): int = -(-(-1)) in f()",
           testAnswer  = Eval [] (Value (-1))
         },
    Test { testName    = "fstTestExtreme",
           testProgram = SrcString "fst (fst((2,1),0))",
           testAnswer  = Eval [] (Value 2)
         },
    Test { testName    = "sndTestExtreme",
           testProgram = SrcString "snd(0,snd(1,2))",
           testAnswer  = Eval [] (Value 2)
         },
    Test { testName    = "pairTestExtreme",
           testProgram = SrcString "snd (1,fst (if true then 42 else 0,2))",
           testAnswer  = Eval [] (Value 42)
         },
    Test { testName    = "recPairTestExtreme",
           testProgram = SrcString "fun f(x:int *int): (int * int)*(int*int) = ((1,2),x) in snd (fst (f(3,4)))",
           testAnswer  = Eval [] (Value 2)
         },
    Test { testName    = "funVarTestExtreme",
           testProgram = SrcString "fun f(x:int): int = 2 in let f=3 in f",
           testAnswer  = Eval [] (Value 3)
         },
    Test { testName    = "lambdaVarTestExtreme",
           testProgram = SrcString "input x in let f= (fn(y:int)->x+y) in let x=2 in f 5",
           testAnswer  = Eval [5] (Value 10)
         },                      
    Test { testName    = "matchTestExtreme",
           testProgram = SrcString "fun f(x:int):int list= match [1]:int list with | [] -> []:int list | x :: x -> x in 1",
           testAnswer  = Eval [] (Value 1)
         },
    Test { testName    = "lambdaTestExtreme",
           testProgram = SrcString "(fn(x:int)->(fn(y:int)->x-y)5)10",
           testAnswer  = Eval [] (Value 5)
         }       
  ]
  ++
  -- Programy poprawnie otypowane. Przypadki konczace sie bledem
  [ Test { testName    = "divTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = 8 div 0 in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "modTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = 2 mod 0 in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "letTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = let x=(1 div 0) in x*10 in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "letZeroTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = let x=0 in 10 mod x in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "ifTrueTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = if true then 1 div 0 else 1 in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "ifFalseTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = if false then 1 else 1 div 0 in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "ifOrTestRuntimeError",
           testProgram = SrcString "fun f(x:unit): int = if (true) or (1 div 0 = 0) then 1 else 0 in f()",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "fstPairLazyRuntimeError",
           testProgram = SrcString "fst (1,2 div 0)",
           testAnswer  = Eval [] RuntimeError
         },         
    Test { testName    = "sndPairLazyRuntimeError",
           testProgram = SrcString "snd (2 div 0,1)",
           testAnswer  = Eval [] RuntimeError
         },         
    Test { testName    = "sndPairRuntimeError",
           testProgram = SrcString "snd (1,2 div 0)",
           testAnswer  = Eval [] RuntimeError
         },  
    Test { testName    = "fstPairRuntimeError",
           testProgram = SrcString "fst (2 div 0,1)",
           testAnswer  = Eval [] RuntimeError
         },
    Test { testName    = "loopRuntimeError",
           testProgram = (SrcFile "./Tests/loop.pp6") ,
           testAnswer  = Eval [0] RuntimeError
         },
    Test { testName    = "lambdaRuntimeError",
           testProgram = SrcString "(fn(x:int)->2 div x) 0",
           testAnswer  = Eval [] RuntimeError
         }                                           
  ]
  ++
  -- Programy niepoprawnie otypowane. Podstawowe operacje
  [
    Test { testName    = "addRightTestTypeError",
           testProgram = SrcString "(fn(x:unit)-> 2+(2<>3)) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "addLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = (2=2)+2 in f()",
           testAnswer  = TypeError
         },  
    Test { testName    = "subRightTestTypeError",
           testProgram = SrcString "(fn (x:unit)-> 2-([]:int list) )()",
           testAnswer  = TypeError
         },
    Test { testName    = "subLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = (3,4)-2 in f()",
           testAnswer  = TypeError
         },  
    Test { testName    = "mulRightTestTypeError",
           testProgram = SrcString "(fn (x:unit) -> 2 * (2<=2)) ()",
           testAnswer  = TypeError
         },  
    Test { testName    = "mulLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = (2<=2) * 2 in f()",
           testAnswer  = TypeError
         },           
    Test { testName    = "divRightTestTypeError",
           testProgram = SrcString "(fn (x:unit)-> 2 div (2<=2)) ()",
           testAnswer  = TypeError
         },  
    Test { testName    = "divLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = (3,3) div 2 in f()",
           testAnswer  = TypeError
         },  
    Test { testName    = "modRightTestTypeError",
           testProgram = SrcString "(fn (x:unit)-> 2 mod (2<=2) ) ()",
           testAnswer  = TypeError
         },  
    Test { testName    = "modLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = (2<=2) mod 2 in f()",
           testAnswer  = TypeError
         },                    
    Test { testName    = "notTestBoolTypeError",
           testProgram = SrcString "fun f(x:unit): bool = not 1 in 1",
           testAnswer  = TypeError
         },  
    Test { testName    = "notTestIntTypeError",
           testProgram = SrcString "(fn (x:unit)-> not 0) ()",
           testAnswer  = TypeError
         },           
    Test { testName    = "neqTestBoolTypeError",
           testProgram = SrcString "(fn(x:unit) -> -false) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "neqTestIntTypeError",
           testProgram = SrcString "fun f(x:unit): int = -true in f()",
           testAnswer  = TypeError
         },         
    Test { testName    = "varTestTypeError",
           testProgram = SrcString "x",
           testAnswer  = TypeError
         },
    Test { testName    = "ifOrRightTestTypeError",
           testProgram = SrcString "(fn (x:unit) -> if ((1>0) or (3 div 2)) then 1 else 2) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifOrLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if ( 1 or true) then 1 else 2 in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifAndRightTestTypeError",
           testProgram = SrcString "(fn(x:unit) -> if ( true and 1 ) then 1 else 2) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifAndLeftTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if ( 2 and (2=2)) then 1 else 2 in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifNeqTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if (true<>false) then 1 else 2 in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifEqTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if (true=true) then 1 else 2 in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifGtTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if ([1]:int list>[3]:int list) then 1 else 2 in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifLtTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if ((1,1)<(0,0)) then 1 else 2 in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifLeTestTypeError",
           testProgram = SrcString "(fn (x:unit)-> if (true<=0) then 1 else 2) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifGeTestTypeError",
           testProgram = SrcString "(fn (x:unit)-> if (1>=false) then 1 else 2 )()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifThenTestTypeError",
           testProgram = SrcString "(fn (x:unit)-> if 1 then 1 else 2) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "ifThenElseTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = if true then 1 else true in f()",
           testAnswer  = TypeError
         }                               
  ]
  ++
  -- Funkcje niepoprawnie otypowane (nieprawidlowy argument)
  [
    Test { testName    = "funcArgExpUnitGetListTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = 1 in f([]: unit list)",
           testAnswer  = TypeError
         },
    Test { testName    = "funcArgExpIntGetUnitTestTypeError",
           testProgram = SrcString "fun f(x:int): int = 1 in f()",
           testAnswer  = TypeError
         },      
    Test { testName    = "funcArgExpBoolGetListTestTypeError",
           testProgram = SrcString "fun f(x:bool): int = 1 in f([]: bool list)",
           testAnswer  = TypeError
         },  
    Test { testName    = "funcArgExpListGetBoolTestTypeError",
           testProgram = SrcString "fun f(x:bool list): int = 1 in f([[]:bool list]:bool list list)",
           testAnswer  = TypeError
         },                 
    Test { testName    = "funcArgExpPair1TestTypeError",
           testProgram = SrcString "fun f(x:int*(bool*bool)): int = 1 in f((1,(true,1)))",
           testAnswer  = TypeError
         },
    Test { testName    = "funcArgExpPair2TestTypeError",
           testProgram = SrcString "fun f(x:int list*bool list): int = 1 in f(([true,false]:bool list,[1,2]:int list))",
           testAnswer  = TypeError
         }
  ]
  ++
    -- Lambdy niepoprawnie otypowane
  [
    Test { testName    = "lambdaArgExpUnitGetListTestTypeError",
           testProgram = SrcString "(fn(x:unit)-> 1) []: unit list",
           testAnswer  = TypeError
         },
    Test { testName    = "lambdaArgExpIntGetUnitTestTypeError",
           testProgram = SrcString "(fn(x:int) -> 1) ()",
           testAnswer  = TypeError
         },      
    Test { testName    = "lambdaArgExpBoolGetListTestTypeError",
           testProgram = SrcString "(fn(x:bool) -> 1) []: bool list",
           testAnswer  = TypeError
         },  
    Test { testName    = "lambdaArgExpListGetBoolTestTypeError",
           testProgram = SrcString "(fn(x:bool list)-> 1) [[]:bool list]:bool list list",
           testAnswer  = TypeError
         },                 
    Test { testName    = "lambdaArgExpPair1TestTypeError",
           testProgram = SrcString "(fn(x:int*(bool*bool))-> 1)(1,(true,1))",
           testAnswer  = TypeError
         },
    Test { testName    = "lambdaArgExpPair2TestTypeError",
           testProgram = SrcString "(fn(x:int list*bool list) -> 1) ([true,false]:bool list,[1,2]:int list)",
           testAnswer  = TypeError
         },
    Test { testName    = "lambdaExpIntReturnBoolTestTypeError",
           testProgram = SrcString "(fn(x:unit)-> true) ()",
           testAnswer  = TypeError
         },
    Test { testName    = "lambdaVarTestTypeError",
           testProgram = SrcString "let f=fn(x:int)->x+y in 5",
           testAnswer  = TypeError
         },                  
    Test { testName    = "lambdaRecTestTypeError",
           testProgram = SrcString "let f=fn(x:int)-> f x in f 5",
           testAnswer  = TypeError
         }
  ]
  ++
  -- Funkcje niepoprawnie otypowane (niepoprawna zwracana wartosc)                 
  [ 
    Test { testName    = "funcExpIntReturnBoolTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = () in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "funcExpBoolReturnListTestTypeError",
           testProgram = SrcString "fun f(x:unit): bool = [true,false,true]:bool list in if f() then 1 else 0",
           testAnswer  = TypeError
         },                  
    Test { testName    = "funcExpUnitReturnIntTestTypeError",
           testProgram = SrcString "fun f(x:unit): unit = 1 in 1",
           testAnswer  = TypeError
         },
    Test { testName    = "funcExpListReturnIntTestTypeError",
           testProgram = SrcString "fun f(x:unit): bool list = [[]:bool list]:bool list list in 1",
           testAnswer  = TypeError
         },
    Test { testName    = "funcExpPairReturnOtherPairTestTypeError",
           testProgram = SrcString "fun f(x:unit): int*bool = (true,1) in 1",
           testAnswer  = TypeError
         }
  ]
  ++
  -- Programy niepoprawnie otypowane. Listy
  [
    Test { testName    = "listIntTestTypeError",
           testProgram = SrcString "let x= [1,2,3,[]:int list]: int list in 1",
           testAnswer  = TypeError
         },
    Test { testName    = "listBoolTestTypeError",
           testProgram = SrcString "let x= [true,false,[]:bool list,true]: bool list in 1",
           testAnswer  = TypeError
         },  
    Test { testName    = "listListTestTypeError",
           testProgram = SrcString "let x= [[]:int list, []:bool list]: int list list in 1",
           testAnswer  = TypeError
         },
    Test { testName    = "listListUnitTestTypeError",
           testProgram = SrcString "let x= [[]:unit list, [true]:unit list]: unit list list in 1",
           testAnswer  = TypeError
         },  
    Test { testName    = "listListPairTestTypeError",
           testProgram = SrcString "let x= [(1,true),(0,false),(false,2)]: (int * bool) list in 1",
           testAnswer  = TypeError
         }, 
    Test { testName    = "notFiniteListTestTypeError",
           testProgram = SrcFile "./Tests/isntFiniteList.pp6",
           testAnswer  = TypeError
         },
    Test { testName    = "listLambdaTestTypeError",
           testProgram = SrcString "let x= [(fn (x:unit)->1)(),(fn (x:unit)->2)(),(fn (x:unit)->true)()]: int list in 1",
           testAnswer  = TypeError
         },
    Test { testName    = "emptyListTestTypeError",
           testProgram = SrcString " fun f(x:bool list): int = 2 in f([]:int list)",
           testAnswer  = TypeError
         },
    Test { testName    = "wrongListTestTypeError",
           testProgram = SrcString "let x=fn(x:unit)->[]:int in f()+2",
           testAnswer  = TypeError
         }                                
  ]
  ++
  -- Programy niepoprawnie otypowane. Funkcje
  [
    Test { testName    = "funTestTypeError",
           testProgram = SrcString "fibbonacci()",
           testAnswer  = TypeError
         },         
    Test { testName    = "funReturnAndArgTestTypeError",
           testProgram = SrcString "fun undertemited(x : bool) : int = x input x in x+2",
           testAnswer  = TypeError
         },
    Test { testName    = "varFuncTestTypeError",
           testProgram = SrcString "fun f(x:unit): int = y input y in f()",
           testAnswer  = TypeError
         },
    Test { testName    = "funcAndVarTestTypeError",
           testProgram = SrcString "fun f(f:int):int=if f=0 then 0 else f+f(f-1) in f 5",
           testAnswer  = TypeError
         }                    
  ]
  ++
  -- Programy niepoprawnie otypowane.  Dopasowanie
  [
    Test { testName    = "emptyValueTestTypeError",
           testProgram = (SrcFile "./Tests/wrongMatch1.pp6"), 
           testAnswer  = TypeError
         },
    Test { testName    = "listValueTestTypeError",
           testProgram = (SrcFile "./Tests/wrongMatch2.pp6"), 
           testAnswer  = TypeError
         },
    Test { testName    = "listArgTestTypeError",
           testProgram = (SrcFile "./Tests/wrongMatch3.pp6"), 
           testAnswer  = TypeError
         }                          
  ]
  ++
  -- Programy niepoprawnie otypowane. Inne
  [
    Test { testName    = "pairTestTypeError",
           testProgram =  (SrcString "snd (1)"), 
           testAnswer  = TypeError
         },
    Test { testName    = "appTestTypeError",
           testProgram =  (SrcString "2+2 4"), 
           testAnswer  = TypeError
         }             
  ]
  ++
  -- Zlozone programy poprawnie otypowane.
  [
    Test { testName    = "countTestExt" ,
           testProgram =  (SrcFile "./Tests/count.pp6"),
           testAnswer  = (Eval [] (Value 4892058347577175493632))
         },
    Test { testName    = "isFiniteListTestExt" ,
           testProgram =  (SrcFile "./Tests/func.pp6"),
           testAnswer  =  (Eval [1]   (Value 1))
         },
    Test { testName    = "lengthTestExt" ,
           testProgram =  (SrcFile "./Tests/func.pp6"),
           testAnswer  =  (Eval [2]   (Value 3))
         },
    Test { testName    = "emptyListLengthTestExt" ,
           testProgram =  (SrcFile "./Tests/func.pp6"),
           testAnswer  =  (Eval [3]   (Value 0))
         },
    Test { testName    = "lengthWithOverwriteTestExt" ,
           testProgram =  (SrcFile "./Tests/func.pp6"),
           testAnswer  =  (Eval [4]   (Value 3))
         },                                         
    Test { testName    = "getElemTestExt" ,
           testProgram =  (SrcFile "./Tests/func.pp6"),
           testAnswer  =  (Eval [5]   (Value 8)) 
         },       
    Test { testName    = "editListTestExt" ,
           testProgram =  (SrcFile "./Tests/func.pp6"),
           testAnswer  =  (Eval [6]   (Value 5))
         },  
    Test { testName    = "vectorTestExt" ,
           testProgram =  (SrcFile "./Tests/vector.pp6")  , 
           testAnswer  =  (Eval []  (Value 22))
         },                   
    Test { testName    = "fibonnaciTestExt" ,
           testProgram =  (SrcFile "./Tests/fib.pp6") ,  
           testAnswer  =   (Eval [19]   (Value 4181))
         },  
    Test { testName    = "NWDTestExt" ,
           testProgram =  (SrcFile "./Tests/NWD.pp6") ,  
           testAnswer  =  (Eval [1,122,18] (Value 2)) 
         },  
    Test { testName    = "lazyFunTestExt" ,
           testProgram =  (SrcFile "./Tests/NWD.pp6") ,  
           testAnswer  =  (Eval [3,100,100] RuntimeError)
         },
    Test { testName    = "factorialTestExt" ,
           testProgram =  (SrcFile "./Tests/factorial.pp6") ,  
           testAnswer  =  (Eval [5] (Value 120))
         },
    Test { testName    = "maxTestExt" ,
           testProgram =  (SrcFile "./Tests/list.pp6") ,  
           testAnswer  =  (Eval [] (Value 4))
         }  ,            
    Test { testName    = "binaryTestExt" ,
           testProgram =  (SrcFile "./Tests/binary.pp6") ,  
           testAnswer  =  (Eval [-123] (Value 6))
         } ,
    Test { testName    = "allTestExt" ,
           testProgram =  (SrcFile "./Tests/test.pp6") ,  
           testAnswer  =  (Eval [] (Value 2))
         },
    Test { testName    = "lambdaMulTestExt" ,
           testProgram =  (SrcFile "./Tests/calc.pp6") ,  
           testAnswer  =  (Eval [2,5,1] (Value 10))
         },                        
    Test { testName    = "lambdaSubTestExt" ,
           testProgram =  (SrcFile "./Tests/calc.pp6") ,  
           testAnswer  =  (Eval [2,3,10] (Value (-1)))
         },
    Test { testName    = "easyLambdaTestExt" ,
           testProgram =  (SrcFile "./Tests/testLambda.pp6") ,  
           testAnswer  =  (Eval [100,10] (Value 0))
         },                                 
    Test { testName    = "trickyLambdaTestExt" ,
           testProgram =  (SrcFile "./Tests/lambada.pp6") ,  
           testAnswer  =  (Eval [] (Value 2))
         }
  ]
