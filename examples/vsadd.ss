func madd(a: float[] bank(32), b: float, c: float[] bank(32)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b;
  } 

}
