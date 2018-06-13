memory a: int[10] bank(5);

for (let i = 0..9) {

  a[i] := i;

  if (i < 9) {
    a[i+1] := i+1;
  }

}
