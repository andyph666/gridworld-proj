(* add line 67 in parser.mly to add if() without else *)
(* add line 245,46 in parser.mly and line 23,24 to ast.ml add string and bool *)

impossible to test : read() & roll() & list() & choose() without an input	

test-arith1 : test basic calculation
OK

test-arith2 : test calculation with operator precedence
FAILED (sth wrong with mod, how about the precedence of mod??)

test-arith3 : test calculation with parenthesis
FAILED (no effect of () )

test-array1 : 1D array assigment and access
FAILED

test-array2 : 2D array assigment and access (not sure if it is written right)
FAILED

test-comment : test the comment
OK

test-decl1 : test variable declaration with initialization
OK

test-decl2 : test variable declaration without initialization
FAILED 

test-fib : test fib, recursion
FAILED(no function and function has not recursion)

test-for1 : test for loop
FAILED(actually no for now)

test-for2 : test for loop and the break
FAILED

test-for3 : test for loop and the continue
FAILED

test-fun1 : test the basic function with return
FAILED

test-fun2 : test the basic function without return
FAILED

test-gcd1 : test gcd, but not using function
OK

test-gcd2 : test gcd, use function
FAILED(no function? can not return?)

test-hello1 : test the string, integer output
OK

test-hello2 : test calculation in print()
OK

test-if1 : test if without else, use expression for judgement
OK

test-if2 : test if without else, use bollean for judgement
OK

test-if3 : test if with else, use expression for judgement
FAILED(something wrong in the python output, one more tab before else)

test-if4 : test if with elif, use expression for judgement
FAILED

test-node : test node and goto
FAILED

test-obj1 : test basic object with integal assignment
FAILED

test-obj2 : test basic object with string assignment
FAILED

test-obj3 : test object in functions
FAILED

test-obj4 : test object in nodes
FAILED

test-ops1 : test binary operators
OK

test-ops2 : test unary operator !
FAILED(no this operators)

test-ops3 : test unary operator ++ and --
FAILED

test-ops4 : test boolean operators 
FAILED

test-scope1 : test the scope of the variables in global
FAILED

test-scope2 : test the scope of the variables in function
FAILED

test-var1 : test integer variable assignment
OK

test-var2 : test string variable assignment
OK

test-var3 : test boolean variable assignment
OK

test-while1 : test while loop
OK

test-while2 : test while loop and break
FAILED

test-while3 : test while loop and continue
FAILED
