int a[6];

a[0] := 8;
a[1] := 5;
a[2] := 3;
a[3] := 1;
a[4] := 9;
a[5] := 2;

for (let i = 0..5) {
    let min_index = i;
    for (let j = i+1..5) {

        if (a[j] < a[min_index]) {
            let min_index = j;
        };
    };

    if (min_index != i) {
        let temp = a[i];
        a[i] := a[min_index];
        a[min_index] := temp;
    };

};




