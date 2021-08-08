# Python_Interpreter

## Object


The project is built based on UIUC CS-421 MP2 (https://github-dev.cs.illinois.edu/cs421-su21/release.git)


## Running Code
Install Haskell 

To run the interpolator, start GHCi with `stack ghci`. From here, run REPL by calling `main`.

Type Python commands under Python REPL:

`Python>`

Use `exit()` or cmd + c to quit the interpolator.

## Interpolator Functions

### Constants
Recognizable constants include
* integer (both positive and negative)
* double (both positive and negative)
* string (the combination of letters, numbers, marks, puctuation, symbols and spaces, excluding double quote ")

### Variables
Assign a constant to a variable
The variable name should start with a letter or a underscore. 
The latter part of the variable name can be letter, number, or underscore.

```
Python> a = 3

Python> print(a)
3
```

### Basic Operations
#### Arithmetic Opeartions
Arithmetic operations (+, -, * and /) for integer and double values.

#### Bool Operations
Bool operations (and, or and not) for bool values (True and False)

#### Comparison Opeartions
Comparison operations (<, >, <=, >=, /=, ==) for integer and double values.

### Python Built-in Functions
#### abs()
Return the aboslute value of a number.

```
Python> abs(-4)
4
Python> abs(-4.1-9.0)
13.1
```

### bool(x)
Return a Boolean value. x is converted using the standard truth testing procedure. 

```
Python> bool(3.2)
True
Python> bool(-1)
True
Python> bool(0)
False
Python> bool(0.0)
False
Python> bool("ab")
True
Python> bool(True)
True
Python> bool(False or True)
True
Python> bool(not False)
True
```
### chr(i)
Return the string representing a character whoe Unicode code point is the integer i. 

```
Python> chr(97)
'a'
Python> chr(45)
'-'
```
### eval(expression)
Evaluate the value of the expression.

```
Python> eval(1+1)
2
Python> a=4

Python> eval(a+2)
6
```

### print()
Print string or value of an expression given in the argument.

```
Python> a="Test"

Python> print(a)
"Test"
```

### round(number, ndigits)
Return number rounded to ndigits precision after the decimal point.

```
Python> round(-8.021,2)
-8.02
```

## To-do
### Expression statement
When input an expression, such as a variable expression, display its value.
Now it cannot be realized, because an expression statement could be interpolated as a set statement.

### String
Currently, string only allows pure letters. Need to include digitals and some legal characters.

### Integer and double operations
Currently, the arithmetic and comparison operations require the two operators to be of the same type (int or doulbe). Need to extend to accept mixed data types.
