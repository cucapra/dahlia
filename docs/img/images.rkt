#lang slideshow

(require racket/set)
(require pict)

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

(define creamsicle  (list 255 153 85))
(define s 12)
(define rect (rectangle s s))

(define (mark rec)
  (cc-superimpose
   rec
   (colorize (filled-rectangle (- s 2) (- s 2)) creamsicle)))

(define (bank r c b)
  (+ (modulo r b) (* b (modulo c b))))

(define (colored-grid n b locs)
  (scale
    (for/fold ([column (blank s)])
      ([c (in-range n)])
      (vc-append
        column
        (for/fold ([row (blank s)])
          ([r (in-range n)])
          (hc-append
            row
            (cc-superimpose
              (if (set-member? locs (list r c))
                (mark rect)
                rect)
              (scale (text (number->string (bank r c b))) 0.6))))))
    3.5))

(define (color-row n b locs)
  (for/fold ([row (blank s)])
    ([r (in-range n)])
    (hc-append
      row
      (cc-superimpose
        (if (set-member? locs r)
          (mark rect)
          rect)
        (scale (text (number->string (modulo r b))) 0.6)))))

;; 8 row / 2 bank / 0 stride
(save-pict (scale (color-row 8 2 (set 0 1 2 3)) 3) "row-slice.png" 'png)

;; 8 row / 2 bank / 2 stride
(save-pict (scale (color-row 8 2 (set 0 2 4 6)) 3) "row-slice-stride.png" 'png)

;; 8 row / 2 bank / 3 stride
(save-pict (scale (color-row 8 2 (set 0 3 6)) 3) "row-slice-invalid.png" 'png)

(save-pict
  (colored-grid 4 2 (set '(0 0) '(2 0) '(2 1) '(0 1) '(2 2) '(0 2) '(2 3) '(0 3)))
  "two-dim-slice.png"
  'png)

