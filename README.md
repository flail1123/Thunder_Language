# Thunder Langugage (extention .th)
## How to run: 
```
git clone https://github.com/flail1123/Thunder_Language.git
make && ./interpret <path_to_your_program>
```
for example:
```
git clone https://github.com/flail1123/Thunder_Language.git
make && ./interpret /good/13-nested-functions.th
```
or to read program from keyboard (ctrl+D after finished writing program):
```
git clone https://github.com/flail1123/Thunder_Language.git
make && ./interpret
```


## Description:
An interpreter and type checker for Thunder an imperative, strongly and statically typed, general-purpose programming language.

## Examples:
hello world:
```
# this is a comment
print("Hello World!");
```
fibonaci:
```
/* 
this is also a comment
but a multiline one
*/
int fib(int n){
    if (n == 1 or n == 2){
        return 1;
    }
    int a = 1, b = 1;
    while (n > 2){
        tuple(a, b) = (b, a+b);
        n--;
    }
    return b;
}
print("fib(3)");
```
arrays are passed by pointers:
```
int[] l = new(int, 10);
int[] changeArray(int[] l_temp){
  for (const i : [0..6]){
    l_temp[i] = 10*i;
  }
  return l_temp;
}
changeArray(l);
print(l);#[0,10,20,30,40,50]
```
More examples of working programs are in folder `good` and of not working programs (those that throw errors) in `bad` folder.

## Files' description:

 * `Types.hs` - all used types
 * `Memory.hs` - utility functions for managment of memory state
 * `TypeChecker.hs` - module responsible for statical typing, run in the first phase before interpreter
 * `Interpreter.hs` - module responsible for actually running the interpreter, run in the second phase after type checker
 * `Main.hs` - main file, runs type checker and interpreter
 * `grammar.cf` - BCNF grammar of the language

## Language description:
Program is a list of statements separated by semicolons.

### Types:

There are 4 simple types available (string, int, bool, none) and 2 complex types arrays and tuples.

Every type can be `const`, which makes it unchangable.

Every type has default value:
 * `int` -> `0`
 * `string` -> `""`
 * `bool` -> `False`
 * `none` -> `None`
Arrays and types have default types depending on of what simple types they are made of.

Arrays are the only types passed by pointer.

Array declaration:
```
Type[]...[] VariableName;
```
Tuple declaration:
```
<< Type, Type, ...>> VariableName;
```

### Operators:

Arythmetic operators: +, -, *, //, %, *=, +=, -=, //=, %=

Comarison operators: <, <=, >, >=, ==, !=

Logical operators: and, or, not 

### Statements:

Block: 
```
{
  Statement
}
```
Condition (if else):
```
if ( BoolExpression )
  Block 
else
  Block
```

Condition (if):
```
if ( BoolExpression )
  Block 
```

Loop (for):
```
for ( Type VariableName : Expr ) 
  Block
```

Loop (while):
```
while ( BoolExpression ) 
  Block
```
Function definition:
```
Type FunctionName ( Type Variable, ...) 
  Block
```
