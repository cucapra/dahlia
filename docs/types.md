---
id: types
title: Types
---

At the heart of the Dahlia compiler is the type checker that checks for various
constraints that make programs hardware realizable.

## Booleans

Boolean constants and the result of comparisons are typed as `boolean`.

```dahlia
let x = true // x is boolean
let z = x >= y // y is boolean
```

## Sized Integers

Bitwidth optimizations are important for building fast hardware. Dahlia allows
programs to specify an exact bit width specification for all constants
and variables.

```
// Signed 10 bit integer
let x: bit<10> = -15;

// Unsigned 10 bit integer
let y: ubit<10> = 102;
```

A bare `let`-binding automatically infers bit widths for constants. The
inference is conservative and finds the minimum number bits needed to store
the constant:

```dahlia
let x = 1; // x is bit<1>
```

If the program needs to store a constant with a larger bitwidth (for use in
a future computation), simply add an explicit type annotation:

```dahlia
let x: bit<10> = 1;
```

## Floats & Doubles

Dahlia provides a simple float type:

```dahlia
let f: float = 1.0; // f is float
let f: double = 2.0;
```

**Note**: Automatic inferences always infers the type `double` for floating
point literals.

The calyx backend currently does not support floats and double.

## Fixed point numbers

```dahlia
let ufix<WIDTH,BP>;

// Unsigned fixed point numbers
let ufix<16, 8> = 1.5;
let ufix<8, 3> = 1.25;
let ufix<32, 12> = 125.75;

// Signed fixed point numbers
let fix<16, 8> = -1.5;
```

**Note:** Signed and unsigned fixed point numbers are converted into `ap_fixed` and `ap_ufixed` respectively in the Vivado backend.

**Note:** Arithmetic betweenn different-width fixed point is currently not implemented in the calyx backend.

## Binary Operations

Binary operations over `booleans` and `floats` are straightforward.

```dahlia
let a = true || false // a is boolean
let b = 1 || 2 // Error! Only booleans expressions can be used with boolean operators
let f = 1.0 + 2.0 // f is float
```

Binary operations over sized integers are more subtle. They automatically
infer the largest bitwidth between the operands and uses that for the
result:

```dahlia
let x: bit<10> = 10;
let y: bit<32> = 20;
let z = x + y; // z is bit<32>
```

## Records

Records are custom types in Dahlia

**Note:** Records are not implemented in the **calyx** backend

```
record point {
  x: bit<32>;
  y: bit<32> // Note there is no semi-colon for the last element
}
```

They cannot arrays, however they can contain other records

```
record rect {
  lb: point;
  rt: point
}
```

### Record Access

Records are created only in let bindings.

```
let p: point = { x = 1; y = 2 }; // Note that there is no ; after y = 2

let dist_sq: bit<32> = p.x * p.x + p.y * p.y;
```

They cannnot be mutated, a new instance will have to be created if an operation is performed on a variable.

```
let p: point = { x = 1; y = 2 };

// Illegal
// p.x := p.x + 1;

let np: point = { x = p.x + 1; y = p.y };
```

### Record and Functions

Records can be used in functions

```
record complex {
    real: float;
    imag: float
}

// Performs d = a * b + c
def complex_fma(a: complex, b: complex, c: complex): complex = {
    let dr: float = c.real + (a.real * b.real) - (a.imag * b.imag);
    let di: float = c.imag + (a.real * b.imag) + (a.imag * b.real);
    let d: complex = { real=dr; imag=di };
    return d;
}
```

## Type Casting

### Implicit Casting

Between the same types implicit type casting is allowed.

```
let i: bit<8> = (1 as bit<8>);
let f: bit<16> = (10 as bit<16>);

let x = f + i; // x is of type bit<16>
```

**Note:** The calyx backend requires **explicit casting**.

### Explicit Casting

Explicit type casting is done using `as` expressions.

```dahlia
let i: bit<8> = (1 as bit<8>);
let f: fix<16, 8> = (10 as fix<16, 8>);

let x = f + (i as fix<16, 8>);
```

Keep in mind that not all conversions are safe. Dahlia will warn when a conversion is done to a type with lesser bitwidth.

```dahlia
let x: ubit<16> = 512;
let y: ubit<8> = (x as ubit<8>); // x is too large to fit in y
```

```
[WARN] [2.18] Casting ubit<16> to ubit<8> which may lose precision.
let y: ubit<8> = (x as ubit<8>);
                 ^
```
