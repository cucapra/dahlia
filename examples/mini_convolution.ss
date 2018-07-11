func mconvo(a: int[3] bank(3), b: int[9], out: int[9]) {

  for (let i = 0..7) {

    let ans = 0;
    for (let j = 0..3) unroll 3 {
      ans := ans + a[j] * b[0][i+j];

    } 

    out[0][i] := ans;

  }

}

