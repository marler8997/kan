# The Kan Semantic Language

Kan is a "semantic langauge".  It differs from a traditional "programming language" in that the syntax is not designed to be easy to write or "look pretty" to humans.  Instead, the language focuses on expressive semantics.  That being said, every language requires some syntax, but Kan's is very minimal.  At first, minimal syntax may seem restrictive, however, it ends up allowing the semantics to be freely extended and modified without having to worry about stomping on the syntax.  It's a tradeoff between "verbosity" and "expressiveness". A complex syntax with lots of features can support very terse statements whereas a simple syntax requires more verbosity but allows the semantics to be freely extended without having to consider how its syntax can be represented and interact with all the existing syntax.

# Interpreted vs Compiled

The tools for the language will support both running via an interpreter and native compilation.

# Syntax

The entirety of the language syntax is built from the following primitive constructs.

* Numbers
* Strings
* Symbols/Keywords
* Tuples
* Function Calls


# The Type System

At a minimum, a type is defined as a function that takes any value and returns whether or not it is a valid value of that type.

Types can also implement additional interfaces such as a "range" interface that enumerate's all its values, or a "printing" interface that can print values or even a "representation" interface that determines the binary represenation interface of each value.  Note that these interfaces are optional.

Note that a "type" itself is also a value that can be passed to other types.

Primitive Values
* a number literal
* a string literal
* a set of values

```
// one number
1234

// a set of two numbers
{1234 5678}
```

How to call a function
```
<function-name>(<arguments>)

print(1)
print(1 2)
```

```
// Adds the symbol 'x' to the current symbol table
set(x 1234)
// overrides the current value of x
assign(x 5678)
```

Predefined functions
```

// function definition
// returns an object with the following functions
// bool containsValue(Value value)
//
// createTypeFromValueSet(Value values...)

// example: define the 'bool' type
set(bool createTypeFromValueSet(false true))

assert(bool.containsValue(false))
assert(bool.containsValue(true))
assert(not(bool.containsValue(42)))

foreach(x bool.values (
    print(x)
))

// function definition
numberRange(<start> <limit>)

// example
set(OneThroughTen createTypeFromValueSet(numberRange(1 11)))

assert(not(OneThroughTen.containsValue(0)))
assert(OneThroughTen.containsValue(1))
assert(OneThroughTen.containsValue(10))
assert(not(OneThroughTen.containsValue(11)))

foreach(x OneThroughTen.values (
    print(x)
))

set(foo OneThroughTen.create(1))
assert(equal(typeof(foo) OneThroughTen))
```

# Different Kinds of Functions

#### Syntax Function
A "syntax function" is a function that operates at the "syntax level" taking it's arguments and returning a semantic node. i.e.
```
symbol("abc") // represents the symbol `abc`
symbol(true)  // represents the symbol `true`, allows you to use keywords as symbols
flag(log)     // represent a "named flag" called "log".  flag values can be passed to functions to enable/disable features
```

#### Semantic Functions
These functions operate on semantic data structures.  For example, the `set` function is a "semantic function" adds an entry to the current scope's symbol table, i.e.
```
set(x 25)
// adds x to the current symbol table with the value 25
```

#### RunTime Functions
Functions that operate on "runtime values", however, they can still be executed at compile-time but always after all "semantic functions" have been interpreted.
