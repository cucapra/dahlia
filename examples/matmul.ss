func mmul(a: float[N*N], b: float[N*N], c: float[N*N]) {

  for (let i = 0..N) {
    for (let j = 0..N) {
      for (let k = 0..N) unroll N {
        c[i*N+j] := a[i*N+k] + b[k*N+j] + c[i*N+j];
      } 
    } 
  } 

}
