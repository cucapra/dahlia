func madd(a: int[] bank(2), b: int, c: int[] bank(2)) {

  for (let i = 0..2) unroll 2 {
    c[i] := a[i] + b;
  } 

}
