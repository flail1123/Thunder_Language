Thunder Langugage (extention .th)
How to run: 
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


Description:
An interpreter and type checker for Thunder an imperative, strongly and statically typed, general-purpose programming language.

##Files' description:

 * `Types.hs` - all used types
 * `Memory.hs` - pomocnicze funkcje do zarządzania stanem pamięci
 * `TypeChecker.hs` - module responsible for statical typing, run in the first phase before interpreter
 * `Interpreter.hs` - module responsible for actually running the interpreter, run in the second phase after type checker
 * `Main.hs` - main file, runs type checker and interpreter


##Language description:
There are 4 simple types available (string, int, bool, none) and 2 complex types arrays and tuples.

Every type has default value:
 * `int` -> `0`
