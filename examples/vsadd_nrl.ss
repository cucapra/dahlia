func madd(a: int[] bank(32), b: int, c: int[] bank(32)) {

  for (let i = 0..1023) {
    c[0][i] := a[0][i] + b;
    c[1][i] := a[1][i] + b;
  } 

}
