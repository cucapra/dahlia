---
id: headers
title: Generating Headers
---

Hardware accelerators are often a part of a larger heterogenous system. The
CLI supports the `-h` option for generating valid headers files for the backend
in question. For example, the Vivado backend will generate a C header file with
function declarations for exposed functions.

To generate a header, simply add the `-h` flag to the normal options:

```bash
fuse -h <srcfile> -b c++
```

>If the backend does not support header file generation, the compiler will
>fail with an error.

The generated header file will contain declarations for all the functions
as well as type alias or records.
