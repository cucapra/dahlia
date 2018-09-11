open Core
open Seashell
open Compile_utils

let compile filename =
  let prog = In_channel.read_all filename in
  let ast = parse_with_error prog in
  let (ctx, dta) = typecheck_with_error ast in
  emit_code ast ctx dta

let%expect_test "should-compile/vsadd.sea" =
  compile "should-compile/vsadd.sea";
  [%expect {|
    void madd(float a[1024], float b, float c[1024]) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=32
    	#pragma HLS ARRAY_PARTITION variable=c factor=32
    	for (int i = 0; i <= 1023; i += 1) {
    		#pragma HLS UNROLL factor=32
    		c[1*(i)] = a[1*(i)]+b;
    	}
    } |}];
