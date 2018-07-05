func madd(a: int[] bank(32), b: int, c: int[] bank(32)) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b;
  } 

}
