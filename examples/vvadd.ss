func madd(a: float[] bank(32), b: float[] bank(32), c: float[] bank(32)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b[i];
  } 

}
