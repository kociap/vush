# Types

## Builtin Types
### Vectors
### Matrices
Members in the form matY or matYxZ where Y and Z are in {2, 3, 4} construct a matrix. Each component (i, j) which has a corresponding component (i, j) in the source matrix will be initialized from it. Other components will be initialized from the identity matrix.

## Array Types

## Struct Types

# Literals
## Bool Literals
`true` or `false`
## Integer Literals
Four types of integer literals are available:
 - decimal literals have no prefix and consist of decimal digits `[0-9]`
 - binary literals start with `0b` or `0B` and consist of binary digits `0` and `1`
 - octal literals start with `0o` or `0O` and consist of octal digits `[0-7]`
 - hexadecimal literals start with `0x` or `0X` and consist of hexadecimal digits `[0-9a-fA-F]`

Leading zeros in decimal literals are not allowed.

All integer literals are by default 32bit signed integers (`int`). Use the suffix `u` or `U` to change the type of a literal to 32bit unsigned integer (`uint`).

Integer literals are never negative. The `-` sign before a literal, e.g. `-1`, is actually a unary operator `-` followed by the integer literal.

## Float Literals
All floating point literals are by default 32bit (`float`). Use the suffix `d` or `D` to change the type to 64bit floating point (`double`).

## String Literals
Newlines are not allowed inside string literals.

# Expressions
```
expression -> all expressions
condition -> no expr_init
```

## Initializer expression
```
     initializer -> '.' identifier '=' expression
                    '[' integer_literal ']' '=' expression
                    expression
initializer_list -> initializer ',' initializer_list
                    initializer
                    epsilon
       init_expr -> type '{' initializer_list '}'
```

Initializers are evaluated in the order of appearance.

# Statements
## Switch
```
switch expression {
    case_label => { statements }
    case_label, case_label => { statements }
}
```
`case_label` might be an integer literal or `default`.
Cases within the same comma separated list will execute the same code. Cases do not have fallthrough mechanism.

# Attributes
```
@attribute-name
```

Attributes may take additional parameters in which case they are declared using a function-call-like syntax
```
@attribute-name(parameters)
```

## workgroup
The workgroup attribute may only be used on compute stages.
```
@workgroup(x [, y [, z]])
```

## builtin
```
@builtin(kind)
```

## Interpolation Attributes
@noperspective
@flat
@smooth
@invariant

# Builtins
## Constants

## Variables
Builtin variables are defined by the user with the attribute `@builtin(X)`.

## Functions


# Functions


# Stages
Stages add more parameter types:
sourced parameters
```
type_name parameter_name from source_name
```
and unique to the vertex stage vertex input parameters
```
type_name parameter_name from in
```

## Vertex
All parameters of a vertex stage must be either sourced parameters or vertex input parameters.
## Fragment
The first parameter of a fragment stage might be an ordinary parameter that is of the same type as the return from the previous stage.
All other parameters must be sourced parameters.
## Compute
All parameters of a compute stage must be sourced parameters.
The return type of a compute stage must always be `void`.

# Passes
A pass must have either a vertex stage and optionally a fragment stage or only a compute stage.

# Settings
