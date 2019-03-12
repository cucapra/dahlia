---
id: imports
title: Imports
---

Import statements allow programs to access functions defined in headers for the
target language. Each language backend can choose to emit the equivalent statements
to import constructs into the program scope.

## Structure

An `import` statement is written as follows:

```
import "<module>" {
  def extern <func1>(...);
  ...
}
```

The statement has zero or more external function definitions. The external
function definitions are added to the environment during typechecking.

## Compilation

Each backend is resposible for emitting statements to add specified functions
into the scope.

### C++ backends

An import statement simply compiles to a `#include`:

```
import "<module>" {
  def extern <func1>(...);
  ...
}
```

compiles to:

```C
#include "<module>"
```
