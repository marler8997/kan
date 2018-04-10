# Ideas

A scratchpad for ideas.

#### Modules As Functions

I think the concept of a Module can fit within the definition of a "function".  To make this work, we can think one aspect of functions that isn't normally done in other languages.  In most langauges, all function calls will also use a stack to store arguments and local parameters.  However, this doesn't have to be the case.  A function could pre-allocate one area of memory where all it's arguments and local variables are stored and re-use that for ever call.  This prevents the ability to call the function recursively, and also prevents multiple threads from calling the function simultaneously.  This mechanism also allows an optimization where the location in memory can be statically compiled into the code.  If this is the case, then the code will only work one way, however, the function could still be compiled to support dynamic positions which would mean the code should work both with a stack and a single area of global memory (maybe, I'd have to go into the details more to figure this out).

In Kan, I want to look into allowing functions to be called either via the stack, or using other allocation mechanisms.  In this way, a module can be thought of as a function as well.  It could be a function that only contains static data, or maybe you could even "call" the module where it's top-level variables would be allocated on a stack instead of in a "global variable section".

#### Contextual Symbols

A "context symbol" is a symbol that represents a different value depending on the context it is referenced from.  This allows multiple "things" to share the same symbol.  Say you want the symbol `foo` to be both a function and an integer.
```
set(foo contextSwitch(
    callContext(function(...))
    defaultContext(42)
))
```
This defines `foo` as a "context symbol: that resolves to a function in a `callContext` and the number literal 42 in any other context, i.e.
```
foo(...) // calls the function foo

set(result add(3 foo)) // result will be set to 45
```
