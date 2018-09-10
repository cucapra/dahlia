## 2018 July 17

### 1. Attempt 1

```
func mmul(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024]) {

  for (let i = 0..31) {
    for (let j = 0..31) {
      for (let k = 0..31) unroll 32 {
        c[i*32+j] := a[i*32+k] + b[k*32+j] + c[i*32+j];
      }
    }
  }

}
```

gives `Can't implicitly access array by indexing into c with type int`
adding `c[0][]` gives `Illegal bank access: 32` probably due to array b banked cyclically.


### 2. Attempt 2

```
func mmul(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024]) {

  for (let i = 0..31) {
    for (let j = 0..31) {
      for (let k = 0..31) unroll 32 {
        c[0][i*32+j] := a[k][i] + b[k][j] + c[0][i*32+j];
      }
    }
  }

}
```

gives `Illegal bank access 0 on array c`

### 3. Proposed version

```
func mmul(a: float[32]:[32] bank(32), b: float[32] bank(32):[32], c: float[32]:[32]) {

  for (let i = 0..31) {
    for (let j = 0..31) {
      for (let k = 0..31) unroll 32 {
        c[i][j] := a[i][k] * b[k][j] + c[i][j];
      }
    }
  }

}
```

