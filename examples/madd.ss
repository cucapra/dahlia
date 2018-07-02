func madd(a: int[] bank(2), b: int, c: int[] bank(2)) {

  for (let i = 0..2) {
    c[0][i] := a[0][i] + b;
    c[1][i] := a[1][i] + b;
  }

}
