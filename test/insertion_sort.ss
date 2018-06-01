int a[6];

a[0] := 8;
a[1] := 5;
a[2] := 3;
a[3] := 1;
a[4] := 9;
a[5] := 2;

let sorted_size = 0;

for (let i = 0..5) {

    let potential_swap = i;
    for (let j = 0..sorted_size) {

        if (a[potential_swap] < a[j]) {
            
            let save = a[j];
            a[j] := a[potential_swap];
            a[potential_swap] := save
            
        }

    };
    let sorted_size = sorted_size + 1
}