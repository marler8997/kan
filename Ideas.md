# Ideas

A scratchpad for ideas.

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
