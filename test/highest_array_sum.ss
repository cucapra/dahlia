int a[5];
int b[5];
int c[5];

int sums[3];

a[0] := 4;
a[1] := 1;
a[2] := 9;
a[3] := 7;
a[4] := 4;

b[0] := 1;
b[1] := 1;
b[2] := 2;
b[3] := 9;
b[4] := 2;

c[0] := 0;
c[1] := 9;
c[2] := 9;
c[3] := 4;
c[4] := 8;

for (let i = 0..4) {

    sums[0] := sums[0] + a[i];
    sums[1] := sums[1] + b[i];
    sums[2] := sums[2] + c[i];

}

let max_index = 0;

for (let i = 0..2) {
    if (sums[i] > sums[max_index]) {
        let max_index = i;
    }
}