memory a: int[4] bank(2);
memory c: int[4] bank(2);

for (let i = 0..1) {
  c[0][i] := a[0][i] ;
  c[1][i] := a[1][i] ;
}
