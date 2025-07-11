# Star Language Compiler

An x86-64 native unoptimizing compiler.

## Usage

- Install Erlang
- Install Gleam

```bash
./compile.sh <source file.star> # will generate executable at the current location
```

Check out examples

## Features

- Data types: *int8*, *int16*, *int32*, *int64*, *bool*
- Variable definition (see *Note on type inference*)
  - **var_int64** := 10
  - **var_int16** *int16* := -5
- Variable assignment
  - **var_int64** = 9
- Pointers:
  - **pointer** := &**var_int16**, type of **pointer** - **int16*
  - **value** := ***pointer**
  - ***pointer** = ***pointer** + 15
- Slices:
  - A slice is a pointer and a length
  - **slice** := *[]int16*(**pointer**, 1), type of **slice** - *[]int16*
  - **first_element** := **slice**[0]
  - **out_of_bounds_error** := **slice**[1]
- Arithmetic: +, -, *, /
- Logic: &&, ||, !
- If statements
- Functions
- Builtins
  - fn **print_int8**(*int8*)
  - fn **print_int16**(*int16*)
  - fn **print_int32**(*int32*)
  - fn **print_int64**(*int64*)
  - fn **print_bool**(*bool*)

### Note on type inference
  Type of variable definition is inferred unless type is specified explicitly, for example:
  ```go
  fn returns_pointer_int8(arg int32) *int8 { ... }

  var_set_int32 int32 := 10
  var_inferred_int32 := var_set_int32 + 5
  var_inferred_int8 := *returns_ponter_int8(var_inferred_int32)
  ```
  However, integer literals are untyped until they appear in an expression with typed context, for example in var_inferred_int32 **5** will be *int32* because it is added to **var_set_int32** of type *int32*.
  
  If the variable is defined without a type to an expression which contains only untyped literals, the type is assumed to be int64.
  ```go
  var_int64_by_default := 10 * (5 / 2) - 3
  you_can_override_of_course int16 := 10 * (5 / 2) - 3
  but_this_errors := var_int64_by_default + you_can_override_of_course // Trying to add int64 and int16
  ```
