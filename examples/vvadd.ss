func madd(a: int[4] bank(2), b: int[4] bank(2), c: int[4] bank(2)) {

  for (let i = 0..2) unroll 2 {
    c[i] := a[i] + b[i];
  } 

}
