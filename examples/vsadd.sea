/////////////////////////////////////////////
// vsadd
// - This application uses one banked array as input, 
// one banked array as output, regular banking with each 
// unroll accessing different bank and having one scalar value
////////////////////////////////////////////


func madd(a: float[1024 bank(32)], b: float, c: float[1024 bank(32)]) {

  for (let i = 0..1023) unroll 32 {
    c[i] := a[i] + b;
  } 

}
