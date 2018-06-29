func add_to_array(a: int[] bank(5), size: int, num: int) {

  for (let i = 0..size) unroll 5 {
    a[i] := a[i] + 5;

  }

}

func add_to_array_twice(a: int[] bank(5), size: int, num: int) {

  add_to_array(a, size, num);
  add_to_array(a, size, num);

}

