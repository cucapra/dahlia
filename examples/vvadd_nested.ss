func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) unroll 4 {
    for (let j = 0..31) unroll 8 {
      c[32*i] := a[32*i+j] + b[32*i];
    } 
  } 

}
