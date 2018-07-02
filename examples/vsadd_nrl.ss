func madd(a: int[4] bank(2), b: int, c: int[4] bank(2)) {

  for (let i = 0..2) {
    c[0][i] := a[0][i] + b;
    c[1][i] := a[1][i] + b;
  } 

}
