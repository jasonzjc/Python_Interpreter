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
* string

### Variables
Assign a constant to a variable

```
Python> a = 3

Python> print(a)
3
```

### Python Built-in Functions
#### abs()
Return the aboslute value of a number.

```
Python> abs(-4)
4
```

## To-do
### Expression statement
When input an expression, such as a variable expression, display its value.
Now it cannot be realized, because an expression statement could be interpolated as a set statement.

### string
Currently, string only allows pure letters. Need to include digitals and some legal characters.