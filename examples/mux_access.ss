func mux_access(a: int[10] bank(5)) {

  mux 5 a_m(a);

  for (let i = 0..4) {

    a_m[i] := 5;


  }


}
