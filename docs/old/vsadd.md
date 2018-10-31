### 2018 June 18
## seashell
```
memory a: int[4] bank(2);
memory c: int[4] bank(2);
let b = 10;

for (let i = 0..1) {
  c[0][i] := a[0][i] + b;
  c[1][i] := a[1][i] + b;
}
```

## current C out
```
int main()
{
	int a[2];
	int c[2];
	int  b = 10;
	for (int i = 0; i < 1; i += 1) {
		c[0+2*i] = a[0+2*i] + b;
		c[1+2*i] = a[1+2*i] + b;
	}
	return 0;
}
```

## ideal C out
```
void madd(float A[N*N], float B, float C[N*N])
{
  int i;

  for (i = 0; i < N * N; i++)
      C[i] = A[i] + B;
}
```

## input to HLS flow
```
void madd(float A[N*N], float B, float C[N*N])
{
  int i;

#pragma HLS ARRAY_PARTITION variable=A factor=ELE

  for (i = 0; i < N * N; i++)
#pragma HLS unroll factor=ELE
      C[i] = A[i] + B;
}
```

## intermediate HLS input
```
void madd(float A[N*N], float B, float C[N*N])
{
  int i;

#pragma HLS ARRAY_PARTITION variable=A factor=ELE
#pragma HLS ARRAY_PARTITION variable=C factor=ELE

  for (int i = 0; i < N; i += 1) {
      C[0+N*i] = A[0+N*i] + B;
      C[1+N*i] = A[1+N*i] + B;
  }
}
```
for N=2 passes through HLS flow.
