## 2018 July 11

Trying out different designs with vvadd  
For the purposes of this document, BF is banking factor, UF is unroll factor  

### 1. No spcifications is illegal- no unroll is explicit

```
func madd(a: float[1024], b: float[1024], c: float[1024]) {

  for (let i = 0..1023) {
    c[i] := a[i] + b[i];
  }

}
```

```
Invalid array accessor
```

* The rule seems all non unrolled loop array acceses should be explicit.

### 2. unroll 1 can be used to get around

```
func madd(a: float[1024], b: float[1024], c: float[1024]) {

  for (let i = 0..1023) unroll 1 {
    c[i] := a[i] + b[i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {

        for (int i = 0; i <= 1023; i += 1) {
                #pragma HLS UNROLL factor=1
                c[i] = a[i]+b[i];
        }
}
```

* Should we make these two designs to be the same? Probably not, non-unrolled is explicit.

### 2a. Use explicit to get around

```
func madd(a: float[1024], b: float[1024], c: float[1024]) {

  for (let i = 0..1023) {
    c[0][i] := a[0][i] + b[0][i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {

        for (int i = 0; i <= 1023; i += 1) {
                c[0 + 1*(i)] = a[0 + 1*(i)]+b[0 + 1*(i)];
        }
}
```

### 3. banking(1) is default

```
func madd(a: float[1024], b: float[1024], c: float[1024]) bank(1) {

  for (let i = 0..1023) {
    c[i] := a[i] + b[i];
  }

}
```

```
Invalid array accessor
```

### 4. Bank and unroll with defaults

```
func madd(a: float[1024], b: float[1024], c: float[1024]) bank(1) {

  for (let i = 0..1023) unroll 1 {
    c[i] := a[i] + b[i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {

        for (int i = 0; i <= 1023; i += 1) {
                #pragma HLS UNROLL factor=1
                c[i] = a[i]+b[i];
        }
}
```

### 4a. 

```
func madd(a: float[1024] bank(1), b: float[1024] bank(1), c: float[1024] bank(1)) {

  for (let i = 0..1023) {
    c[i] := a[i] + b[i];
  }

}
```

```
Invalid array accessor
```

* The culpit seems to be unroll- Unroll decides what type of array access.

### 4b. 

```
func madd(a: float[1024] bank(1), b: float[1024] bank(1), c: float[1024] bank(1)) {

  for (let i = 0..1023) unroll 1 {
    c[i] := a[i] + b[i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {

        for (int i = 0; i <= 1023; i += 1) {
                #pragma HLS UNROLL factor=1
                c[i] = a[i]+b[i];
        }
}
```

### 5. General design point

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=32
        #pragma HLS ARRAY_PARTITION variable=b factor=32
        #pragma HLS ARRAY_PARTITION variable=c factor=32
        for (int i = 0; i <= 1023; i += 1) {
                #pragma HLS UNROLL factor=32
                c[i] = a[i]+b[i];
        }
}
```

### 6. BF < UF is illegal

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(1)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  }
  
}
```

```
Illegal bank access
```

### 7. BF > UF is legal

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(64)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {

        #pragma HLS ARRAY_PARTITION variable=c factor=64
        for (int i = 0; i <= 1023; i += 1) {
                #pragma HLS UNROLL factor=32
                c[i] = a[i]+b[i];
        }
}
```

This fails on 13th July `Illegal bank access: 1`

* This might be of actual use in HLS, but should we qualify it?

### 8. Multiple access (read) is illegal

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(32)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  }

}
```

```
Illegal bank access: 1
```

### 9. Multiple access (write) is illegal

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024]) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  }

}
```

```
Illegal bank access: 1
```

### 10. Simple array[bank][index] access  

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..511) {
    c[0][i] := a[i] + b[i];
    c[1][i] := a[i] + b[i];
  }

}
```

`Invalid array accessor` `Can't implicitly access array by indexing with type: int`

* Error is unroll using this access

### 10a. Simple array[bank][index] access   

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..511) {
    c[0][i] := a[0][i] + b[0][i];
    c[1][i] := a[0][i] + b[0][i];
  }

}
```

```
Illegal bank access: 0
```

* Because multiple read

### 10b. Simple array[bank][index] access   

```
func madd(a: float[1024] bank(2), b: float[1024] bank(2), c: float[1024] bank(2)) {

  for (let i = 0..511) {
    c[0][i] := a[0][i] + b[0][i];
    c[1][i] := a[0][i] + b[0][i];
  }

}
```

```
Illegal bank access: 0
```

* Is this an illegal access? Yes, still multiple read

### 10c. Simple array[bank][index] access  

```
func madd(a: float[1024] bank(2), b: float[1024] bank(2), c: float[1024] bank(2)) {

  for (let i = 0..511) {
    c[0][i] := a[0][i] + b[0][i];
    c[1][i] := a[1][i] + b[1][i];
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=2
        #pragma HLS ARRAY_PARTITION variable=b factor=2
        #pragma HLS ARRAY_PARTITION variable=c factor=2
        for (int i = 0; i <= 511; i += 1) {
                c[0 + 2*(i)] = a[0 + 2*(i)]+b[0 + 2*(i)];
                c[1 + 2*(i)] = a[1 + 2*(i)]+b[1 + 2*(i)];
        }
}
```

* This is valid because access type is correct and no read multi access

### 10d. Simple array[bank][index] access  

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..511) {
    c[0][i] := a[0][i] + b[0][i];
    c[1][i] := a[1][i] + b[1][i];
  }

}
```

```
Illegal bank access: 1
```

* This error makes sense, but shouldn't either 11 or 11a be valid? The error here is no banks.

### 11. Using array[bank][index] access in unroll, 

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..1023) unroll 2 {
    c[0][0] := a[i] + b[i];
    c[1][0] := a[i] + b[i];
  }

}
```

```
Illegal bank access
```

* Why is it illegal? Some details in error message would help. It is illegal as indexing by i needs to be banked. Up to b we have the same error.

### 11a. Using array[bank][index] access in unroll

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..1023) unroll 2 {
    c[0][i] := a[i] + b[i];
    c[1][i] := a[i] + b[i];
  }

}
```

```
Bank accessor must be static
```

* I feel this should give same error as above?

### 11b. Using array[bank][index] access in unroll

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..1023) unroll 2 {
    c[0][0] := a[0][0] + b[0][0];
    c[1][0] := a[0][0] + b[0][0];
  }

}
```

```
Illegal bank access: 0
```

* a and b second access multiple read.

### 12. Nested for vvadd using one loop index

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 32 {
      c[i] := a[i] + b[i];
    }
  }

}
```

```
Invalid array accessor
```

* Verifying this access is illegal

### 12a. Nested for vvadd using one loop index

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) unroll 32 {
    for (let j = 0..31) {
      c[i] := a[i] + b[i];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=32
        #pragma HLS ARRAY_PARTITION variable=b factor=32
        #pragma HLS ARRAY_PARTITION variable=c factor=32
        for (int i = 0; i <= 31; i += 1) {
                #pragma HLS UNROLL factor=32
                for (int j = 0; j <= 31; j += 1) {
                        c[i] = a[i]+b[i];
                }
        }
}
```

* Neat!! As expected.

### 12b. Nested for vvadd using one loop index

```
func madd(a: float[1024] bank(16), b: float[1024] bank(16), c: float[1024] bank(16)) {   
                               
  for (let i = 0..31) unroll 4 {       
    for (let j = 0..31) {              
      c[2*i+1] := a[2*i+1] + b[2*i+1]; 
    }                                  
  }                                    

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=16
        #pragma HLS ARRAY_PARTITION variable=b factor=16
        #pragma HLS ARRAY_PARTITION variable=c factor=16
        for (int i = 0; i <= 31; i += 1) {
                #pragma HLS UNROLL factor=4
                for (int j = 0; j <= 31; j += 1) {
                        c[2*i+1] = a[2*i+1]+b[2*i+1];
                }
        }
        
}
```

### 13a. Nested for vvadd using both indeces no unroll 

```
func madd(a: float[1024] bank(16), b: float[1024] bank(16), c: float[1024] bank(16)) {

  for (let i = 0..31) {
    for (let j = 0..31) {
      c[0][i] := a[0][i] + b[0][i];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=16
        #pragma HLS ARRAY_PARTITION variable=b factor=16
        #pragma HLS ARRAY_PARTITION variable=c factor=16
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        c[0 + 16*(i)] = a[0 + 16*(i)]+b[0 + 16*(i)];
                }
        }
}
```

### 13b. Nested for vvadd using both indeces no unroll 

```
func madd(a: float[1024] bank(16), b: float[1024] bank(16), c: float[1024] bank(16)) {

  for (let i = 0..31) {
    for (let j = 0..31) {
      c[0][i+j] := a[0][i+j] + b[0][i+j];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=16
        #pragma HLS ARRAY_PARTITION variable=b factor=16
        #pragma HLS ARRAY_PARTITION variable=c factor=16
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        c[0 + 16*(i+j)] = a[0 + 16*(i+j)]+b[0 + 16*(i+j)];
                }
        }
}
```

### 14. Two unrolls

```
func madd(a: float[1024] bank(16), b: float[1024] bank(16), c: float[1024] bank(16)) {

  for (let i = 0..31) unroll 2{
    for (let j = 0..31) unroll 2 {
      c[i+j] := a[i+j] + b[i+j];
    }
  }

}
```

```
Fatal error: exception Failure("Undefined")
```

### 15. One unroll

```
func madd(a: float[1024] bank(16), b: float[1024] bank(16), c: float[1024] bank(16)) {

  for (let i = 0..31) unroll 2 {
    for (let j = 0..31) {
      c[i+j] := a[i+j] + b[i+j];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=16
        #pragma HLS ARRAY_PARTITION variable=b factor=16
        #pragma HLS ARRAY_PARTITION variable=c factor=16
        for (int i = 0; i <= 31; i += 1) {
                #pragma HLS UNROLL factor=2
                for (int j = 0; j <= 31; j += 1) {
                        c[i+j] = a[i+j]+b[i+j];
                }
        }
}
```

* Wasn't this expected to fail?

### 16. One unroll

```
func madd(a: float[1024] bank(16), b: float[1024] bank(16), c: float[1024] bank(16)) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 2 {
      c[i+j] := a[i+j] + b[i+j];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=16
        #pragma HLS ARRAY_PARTITION variable=b factor=16
        #pragma HLS ARRAY_PARTITION variable=c factor=16
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        #pragma HLS UNROLL factor=2
                        c[i+j] = a[i+j]+b[i+j];
                }
        }
}
```

* Which loop is unrolled doesn't matter?

### 17. Arithmetic in loop index

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 32 {
      c[32*i+j] := a[32*i+j] + b[32*i+j];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=32
        #pragma HLS ARRAY_PARTITION variable=b factor=32
        #pragma HLS ARRAY_PARTITION variable=c factor=32
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        #pragma HLS UNROLL factor=32
                        c[32*i+j] = a[32*i+j]+b[32*i+j];
                }
        }
}
```

* Multiplier in loop doesn't matter? Only two unrolls seem to hurt seashell.  

## 2018 July 17

### 1. Roundabout way with unroll 1 is invalid because of invalid index equations  

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[32]) {

  for (let i = 0..31) unroll 1 {
    for (let j = 0..31) unroll 32 {
      c[32*i] := a[32*i+j] + b[32*i+j];
    }
  }

}
```  

`Illegal operation: can't apply operator '+' to index with static and dynamic information and index with static and dynamic information`

### 2. explicit notation for first loop works. 

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[32]) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 8 {
      c[0][32*i] := a[32*i+j] + b[32*i+j];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[32]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=32
        #pragma HLS ARRAY_PARTITION variable=b factor=32
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        #pragma HLS UNROLL factor=8
                        c[0 + 1*(32*i)] = a[32*i+j]+b[32*i+j];
                }
        }
}
```

### 3. unroll in one loop and use it in all arrays works

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 32 {
      c[32*i+j] := a[32*i+j] + b[32*i+j];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=32
        #pragma HLS ARRAY_PARTITION variable=b factor=32
        #pragma HLS ARRAY_PARTITION variable=c factor=32
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        #pragma HLS UNROLL factor=32
                        c[32*i+j] = a[32*i+j]+b[32*i+j];
                }
        }
}
```

### 4. full unroll in one loop, can make it explicit access

```
func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 32 {
      c[j][i] := a[j][i] + b[j][i];
    }
  }

}
```

```
void madd(float a[1024], float b[1024], float c[1024]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=32
        #pragma HLS ARRAY_PARTITION variable=b factor=32
        #pragma HLS ARRAY_PARTITION variable=c factor=32
        for (int i = 0; i <= 31; i += 1) {
                for (int j = 0; j <= 31; j += 1) {
                        #pragma HLS UNROLL factor=32
                        c[j + 32*(i)] = a[j + 32*(i)]+b[j + 32*(i)];
                }
        }
}
```
