func madd(a: float[1024] bank(32), b: float[1024] bank(32), c: float[1024] bank(32)) {

  for (let i = 0..31) {
    for (let j = 0..31) unroll 32 {
      c[j][i] := a[j][i] + b[j][i];
    } 
  } 

}
