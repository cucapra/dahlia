func mmul(a: float[9 bank(3)], b: float[9 bank(3)], c: float[3]) {

  for (let i = 0..3) unroll 1 {
    for (let j = 0..3) unroll 3 {
      c[i] := a[i*3+j] + b[i*3+j];
    } 
  } 

}
