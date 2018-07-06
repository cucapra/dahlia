func mmul(a: float[1024], b: float[1024], c: float[1024]) {

  for (let i = 0..1023-1) {
    for (let j = 0..1023-1) {
      for (let k = 0..1023-1) unroll 32 {
        c[i*32+j] := a[i*32+k] + b[k*32+j] + c[i*32+j];
      } 
    } 
  } 

}
