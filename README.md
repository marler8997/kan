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

# Semantic Functions

"Semantic Funtions" only execute during semantic analysis and take semantic data structures.  They have access to a special API that is only available at semantic analysis time.

For example, the `set` function is a "semantic function" that adds an entry to the current scope's symbol table, i.e.
```
set(x 25)
// adds x to the current symbol table with the value 25
```

### Semantic Arguments

Semantic functions specify how they like their arguments.  They can be:

1. raw syntax nodes
2. semi-analyzed nodes where non-function symbols are left unresolved (note that semantic functions are still executed for these arguments)
3. fully analyzed nodes

A semantic function can also return a semantic node.  If it does, the analyzer will continue by analyzing the return value.

> NOTE: maybe a semantic function should have the option of having some of it's arguments in different forms, i.e. the first could be "semi-analyzed" and the rest "fully analyzed".

### Implied Arguments

All semantic functions have access to some "implied arguments" such as:

* An array of semantic nodes, one for each input argument (note: this may change depending on how the function has declared it wants its arguments)
* The data structure for the calling scope
* A reference to the analyzer data structure, who will probably have its own methods that can be called such as "analyzeNode".

### Static Data

A semantic call could be interpreted multiple times if previous invocations did not fully complete.  Because of this, a semantic call may want to save/access data from previous invocations.  If so, it can declare static data that persists each time the call is invoked.

> NOTE: internally, this could be implemented by adding a `void*` pointer to a SemanticCall node, which points to a data buffer that can hold all the static data for that call

### Error Handling

A semantic function can also return one of 2 error conditions:

1. analysis failed because of undefined symbol
2. analysis failed and more symbols won't help

The first error condition will cause the analyzer to continue on and come back around if more symbols are added.  If no more symbols are added, it will call the function again in a different state indicating the semantic function should now report missing symbols as errors.  The second error condition will cause the analyzer to fail on the current pass and the semantic function should have already reported its errors.

### Semantic Functions that "Add Symbols"

Semantic functions that can "add symbols" to the current scope must declare this ability.  If they do, they must also indicate when they are done adding symbols to the current scope (even if they haven't been fully analyzed yet).  The reason for this is that whenever a symbol is resolved, the analyzer will not ascend the scope heirarchy until each inner scope can add no more symbols.  This is because the analyzer must always resolve symbols to their most inner scope, and if an inner scope has not added all its symbols then you can never move a symbol resolution to an upper scope because that symbol could be added to an inner scope later.

Note that this feature is enforced by the analyzer. It uses 2 methods.  If a semantic function has declared it will NEVER add symbols to the call scope, then the analyzer will change the type of the scope passed to the function to "readonly" (it won't have an "add" method to add symbols to it).  If a semantic function has declared that it CAN add symbols but already been partially analyzed and returned a state indicating it has already added all it's symbols, then the analyzer will assert an error if the call adds any more symbols on a later pass.