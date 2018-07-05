func mmul(a: float[N*N], b: float[N*N], c: float[N*N]) {

  for (let i = 0..N-1) {
    for (let j = 0..N-1) {
      for (let k = 0..N-1) unroll N {
        c[i*N+j] := a[i*N+k] + b[k*N+j] + c[i*N+j];
      } 
    } 
  } 

}
