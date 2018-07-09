func madd(a: float[1024] bank(32), b: float, c: float[1024] bank(32)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b;
  } 

}
