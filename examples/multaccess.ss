func mmul(a: float[9] bank(3), b: float[9] bank(3), c: float[3] bank(1)) {

  for (let i = 0..3)  {
    for (let j = 0..3) unroll 3 {
      c[i*3] := a[i*3+j] + b[i*3+j];
    } 
  } 

}
