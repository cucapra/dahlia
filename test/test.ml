(** Use this file to write down end-to-end tests for the compiler toolchain.
    When adding a new file, make sure to add it to [deps] in test/dune. *)
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
    } |}]
;;


let%expect_test "should-compile/array1d_logical.sea" =
  compile "should-compile/array1d_logical.sea";
  [%expect {|
    void madd(float a[1024], float b) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=32
    	for (int i = 0; i <= 1023; i += 1) {
    		#pragma HLS UNROLL factor=32
    		b = a[1*(i)];
    	}
    } |}]
;;

let%expect_test "should-compile/array1d_physical.sea" =
  compile "should-compile/array1d_physical.sea";
  [%expect {|
    void madd(float a[1024], float b, float c) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=32
    	for (int i = 0; i <= 31; i += 1) {
    		#pragma HLS UNROLL factor=1

    		b = a[0 + 32*(i)];
    		c = a[1 + 32*(i)];
    	}
    } |}]
;;

let%expect_test "should-compile/crlf.sea" =
  compile "should-compile/crlf.sea";
  [%expect {|
    void add(float a, float b) {
    	float c = a+b;
    } |}]
;;

let%expect_test "should-compile/float.sea" =
  compile "should-compile/float.sea";
  [%expect {|
    void add_floats(float a, float b) {

    	float x = 2.5;
    	float z = a+b+x;
    } |}]
;;

let%expect_test "should-compile/logical_access.sea" =
  compile "should-compile/logical_access.sea";
  [%expect {|
    void logl_access(int a[240]) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=30
    	for (int i = 0; i <= 9; i += 1) {
    		#pragma HLS UNROLL factor=5
    		for (int j = 0; j <= 3; j += 1) {
    			#pragma HLS UNROLL factor=2
    			for (int k = 0; k <= 6; k += 1) {
    				#pragma HLS UNROLL factor=3
    				a[24*(i)+6*(j)+1*(k)] = 2;
    			}
    		}
    	}
    } |}]
;;

let%expect_test "should-compile/multaccess.sea" =
  compile "should-compile/multaccess.sea";
  [%expect {|
    void mmul(float a[9], float b[9], float c[3]) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=3
    	#pragma HLS ARRAY_PARTITION variable=b factor=3

    	for (int i = 0; i <= 3; i += 1) {
    		#pragma HLS UNROLL factor=1
    		for (int j = 0; j <= 3; j += 1) {
    			#pragma HLS UNROLL factor=3
    			c[1*(i)] = a[1*(i*3+j)]+b[1*(i*3+j)];
    		}
    	}
    } |}]
;;

let%expect_test "should-compile/typedefs.sea" =
  compile "should-compile/typedefs.sea";
  [%expect {|
    typedef int number;
    void add(int a, int b) {

    	int x = a+b;
    	int y = 5;
    	int z = a+b+x+y;
    } |}]
;;

let%expect_test "should-compile/vsadd_nrl.sea" =
  compile "should-compile/vsadd_nrl.sea";
  [%expect {|
    void madd(int a[1024], int b, int c[1024]) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=32
    	#pragma HLS ARRAY_PARTITION variable=c factor=32
    	for (int i = 0; i <= 31; i += 1) {
    		#pragma HLS UNROLL factor=1

    		c[0 + 32*(i)] = a[0 + 32*(i)]+b;
    		c[1 + 32*(i)] = a[1 + 32*(i)]+b;
    	}
    } |}]
;;

let%expect_test "should-compile/vvadd.sea" =
  compile "should-compile/vvadd.sea";
  [%expect {|
    void madd(float a[1024], float b[1024], float c[1024]) {
    	#pragma HLS ARRAY_PARTITION variable=a factor=32
    	#pragma HLS ARRAY_PARTITION variable=b factor=32
    	#pragma HLS ARRAY_PARTITION variable=c factor=32
    	for (int i = 0; i <= 1023; i += 1) {
    		#pragma HLS UNROLL factor=32
    		c[1*(i)] = a[1*(i)]+b[1*(i)];
    	}
    } |}]
;;
