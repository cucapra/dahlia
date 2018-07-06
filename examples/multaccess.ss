func mmul(a: float[9], b: float[9], c: float[9]) {

  for (let i = 0..3) {
    for (let j = 0..3) {
      c[i*3] := a[i*3+j] + b[i*3+j];
    } 
  } 

}
