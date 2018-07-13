## 2018 July 11

Trying out different designs with vvadd  
For the purposes of this document, BF is banking factor, UF is unroll factor  

### 1. No spcifications

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

### 2. 

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

* Should we make these two designs to be the same?

### 3. 

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

### 4. 

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

* The culpit seems to be unroll

### 5. 

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

### 6. General design point

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

### 7. BF < UF is illegal

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

### 8. BF > UF is legal

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

* This might be of actual use in HLS, but should we qualify it?

### 9. Multiple write is illegal

```
func madd(a: float[1024] bank(32), b: float[1024], c: float[1024]) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  }

}
```

```
Illegal bank access
```

### 10. Using array[bank][index] access in unroll

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

* Why is it illegal? Some details in error message would help.

### 10a. Using array[bank][index] access in unroll

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

### 10b. Using array[bank][index] access in unroll

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

### 11. Simple array[bank][index] access  

```
func madd(a: float[1024], b: float[1024], c: float[1024] bank(2)) {

  for (let i = 0..511) {
    c[0][i] := a[i] + b[i];
    c[1][i] := a[i] + b[i];
  }

}
```

```
Invalid array accessor
```

### 11a. Simple array[bank][index] access   

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

### 11b. Simple array[bank][index] access   

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

* Is this an illegal access?

### 11c. Simple array[bank][index] access  

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

### 11.d. Simple array[bank][index] access  

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

* This error makes sense, but shouldn't either 11 or 11a be valid?

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
