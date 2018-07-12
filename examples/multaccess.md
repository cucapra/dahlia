## 2018 July 12

### 1. Initial design 

```
func mmul(a: float[9] bank(3), b: float[9] bank(3), c: float[3]) {

  for (let i = 0..3)  {
    for (let j = 0..3) unroll 3 {
      c[i] := a[i*3+j] + b[i*3+j];
    }
  }

}
```

```
Invalid array accessor
```

* This is invalid because c is accessed using i. Shouldn't array accessor depend on array type?

### 2. Exploiting vvadd behaviour with unroll to get around this

```
func mmul(a: float[9] bank(3), b: float[9] bank(3), c: float[3]) {

  for (let i = 0..3) unroll 1 {
    for (let j = 0..3) unroll 3 {
      c[i] := a[i*3+j] + b[i*3+j];
    }
  }

}
```

```
void mmul(float a[9], float b[9], float c[3]) {
        #pragma HLS ARRAY_PARTITION variable=a factor=3
        #pragma HLS ARRAY_PARTITION variable=b factor=3

        for (int i = 0; i <= 3; i += 1) {
                #pragma HLS UNROLL factor=1
                for (int j = 0; j <= 3; j += 1) {
                        #pragma HLS UNROLL factor=3
                        c[i] = a[i*3+j]+b[i*3+j];
                }
        }
}
```

* This is the same example as before, but using unroll term gives a workaround.

