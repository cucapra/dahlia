#lang racket/base

(require "let-matrix.rkt"
         "image-gen-helpers.rkt"
         racket/list
         pict)

;; Constants defined in the header (modified to make the example smaller)
(define row-size 6)
(define size (* row-size row-size))
(define block-size 2)

#| Original C code
   loopjj:for (jj = 0; jj < row_size; jj += block_size){
        loopkk:for (kk = 0; kk < row_size; kk += block_size){
            loopi:for ( i = 0; i < row_size; ++i){
                loopk:for (k = 0; k < block_size; ++k){
                    i_row = i * row_size;
                    k_row = (k  + kk) * row_size;
                    temp_x = m1[i_row + k + kk];
                    loopj:for (j = 0; j < block_size; ++j){
                        mul = temp_x * m2[k_row + j + jj];
                        prod[i_row + j + jj] += mul;
                    }
                }
            }
        }
}
|#

#|
(define-values (M1 M2 PROD)
  (let/matrix [(m1[1][size #:bank block-size])
               (m2[1][size #:bank block-size])
               (prod[1][size #:bank block-size])]
              (for ([jj (in-range 0 row-size block-size)]
                    [kk (in-range 0 row-size block-size)]
                    [i (in-range row-size)]
                    [k (in-range block-size)]
                    [j (in-range block-size)])
                (let ([i-row (* i row-size)]
                      [k-row (* row-size (+ k kk))])
                  (m1[0][(+ i-row k kk)])
                  (m2[0][(+ k-row j jj)])
                  (prod[0][(+ i-row j jj)])))))
|#

(define-values (M1 M2 PROD)
  (let/matrix [(m1[row-size #:bank block-size][row-size #:bank block-size])
               (m2[row-size #:bank block-size ][row-size #:bank block-size])
               (prod[row-size #:bank block-size][row-size #:bank block-size])]
              (for ([jj (in-range 0 row-size block-size)]
                    [kk (in-range 0 row-size block-size)]
                    [i (in-range row-size)]
                    [k (in-range block-size)]
                    [j (in-range block-size)])
                (let ([i-row (* i row-size)]
                      [k-row (* row-size (+ k kk))])
                  (m1[i][(+ k kk)])
                  (m2[(+ k kk)][(+ j jj)])
                  (prod[i][(+ j jj)])))))


(define elems 64)
(define img-lst
 (zip-with (lambda (m1 m2 prod)
             (vc-append 4 m1 m2 prod))
           (take M1 elems)
           (take M2 elems)
           (take PROD elems)))

(save-pict (scale (smart-layout img-lst) 2) "gemm-blocked.png" 'png)