func madd(a: float[1024] bank(1024), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) unroll 32 {
    for (let j = 0..31) unroll 32 {
      c[32*i] := a[32*i+j] + b[32*i];
    } 
  } 

}
