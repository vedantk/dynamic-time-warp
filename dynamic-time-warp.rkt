#lang racket

(define (create-2d xlen ylen)
  (build-vector xlen (lambda (_) (make-vector ylen 0))))

(define (matrix-get matrix x y)
  (vector-ref (vector-ref matrix x) y))

(define (matrix-set! matrix x y val)
  (vector-set! (vector-ref matrix x) y val))

(define (distance x y)
  (abs (- x y)))

(define (dynamic-time-warp v1 v2)
  (define-values (v1-len v2-len)
    (values (vector-length v1)
            (vector-length v2)))
  
  (define dtw-matrix (create-2d v1-len v2-len))
  
  (matrix-set! dtw-matrix 0 0 0)
  (for ([i (in-range 1 v1-len)])
    (matrix-set! dtw-matrix i 0 +inf.f))
  (for ([i (in-range 1 v2-len)])
    (matrix-set! dtw-matrix 0 i +inf.f))
  
  (for ([i (in-range 1 v1-len)])
    (for ([j (in-range 1 v2-len)])
      (let ([hs (filter (lambda (k) (< k v1-len))
                        (list (- i 1) i (+ i 1)))]
            [ks (filter (lambda (k) (< k v2-len))
                        (list (- j 1) j (+ j 1)))])
        (when (= (length hs) (length ks))
          (matrix-set! dtw-matrix i j
                       (+ (distance (vector-ref v1 i) (vector-ref v2 j))
                          (apply min (map (lambda (h k)
                                            (matrix-get dtw-matrix h k))
                                          hs
                                          ks))))))))
  
  (matrix-get dtw-matrix (- v1-len 1) (- v2-len 1)))

(define (test)
  (define v1
    (list->vector (map sin (stream->list (in-range 0 10 .05)))))
  (define v2
    (list->vector (map (lambda (k) (* 1.1 (sin k)))
                       (stream->list (in-range 0 10 .06)))))
  
  ; 0.0
  (displayln (dynamic-time-warp v1 v1))
  
  ; 0.059
  (displayln (dynamic-time-warp v1 v2)))