;; 12.scm - Robert Coffey - 2022-12-12

(import (chicken format)
        (chicken io)
        (srfi 1)
        vector-lib)

(define (string->vector str)
  ((compose list->vector string->list) str))

(define grid)

(define height)
(define width)

(define _start)
(define end)

(define (parse-lines! lines)
  (set! grid (list->vector (map string->vector lines)))
  (set! height (vector-length grid))
  (set! width (vector-length (vector-ref grid 0)))
  (define (find-char c)
    (let loop-y ((y 0))
      (if (>= y height)
          #f
          (let* ((row (vector-ref grid y))
                 (x (let loop-x ((x 0))
                      (cond ((>= x width) #f)
                            ((char=? c (vector-ref row x)) x)
                            (else (loop-x (+ x 1)))))))
            (if x (cons y x) (loop-y (+ y 1)))))))
  (set! _start (find-char #\S))
  (set! end (find-char #\E))
  (vector-for-each
   (lambda (y r)
     (vector-map!
      (lambda (x c)
        (cond ((char=? c #\S) 0)
              ((char=? c #\E) 2)
              (else (- (char->integer c) (char->integer #\a)))))
      r))
   grid))

(define (diff pos1 pos2)
  (- (vector-ref (vector-ref grid (car pos1)) (cdr pos1))
     (vector-ref (vector-ref grid (car pos2)) (cdr pos2))))

(define (neighbors pos)
  (define y (car pos))
  (define x (cdr pos))
  (define neighs '())
  (define (check-np! np)
    (unless (> (diff np pos) 1)
      (set! neighs (cons np neighs))))
  (when (> y 0)            (check-np! (cons (- y 1) x)))
  (when (< y (- height 1)) (check-np! (cons (+ y 1) x)))
  (when (> x 0)            (check-np! (cons y (- x 1))))
  (when (< x (- width 1))  (check-np! (cons y (+ x 1))))
  neighs)

(define-record node pos parent cost)

(define (bfs start)
  (define open '()) ; list of open nodes

  (define (open? node) (memq node open))
  (define (open! nodes) (set! open (append open nodes)))

  (define closed (make-vector height)) ; 2D vector of booleans
  (vector-map! (lambda (y r) (make-vector width #f)) closed)

  (define (closed? pos) (vector-ref (vector-ref closed (car pos)) (cdr pos)))
  (define (close! pos) (vector-set! (vector-ref closed (car pos)) (cdr pos) #t))

  (define (%bfs node)
    (define pos (node-pos node))
    (if (equal? pos end)
        node
        (let* ((neighs/pos (neighbors pos))
               (neighs/all (map (lambda (p)
                                  (make-node p node (+ 1 (node-cost node))))
                                neighs/pos))
               (neighs (filter (lambda (n) (not (closed? (node-pos n))))
                               neighs/all)))
          (for-each (lambda (n) (close! (node-pos n))) neighs)
          (open! neighs)
          (if (null? open)
              #f
              (let ((next (car open)))
                (set! open (cdr open))
                (%bfs next))))))

  (define root (make-node start #f 0))
  (close! start)
  (%bfs root))

(define (main path)
  (parse-lines! (with-input-from-file path read-lines))

  ;; Part 1
  (define end (bfs _start))
  (printf "Part 1: ~A\n" (if end (node-cost (bfs _start)) #f))

  ;; Part 2
  (print "Part 2...")
  (define zero-ps '())
  (vector-for-each
   (lambda (y r)
     (vector-for-each
      (lambda (x c)
        (when (zero? (vector-ref (vector-ref grid y) x))
          (set! zero-ps (cons (cons y x) zero-ps))))
      r))
   grid)
  (define count 0)
  (define costs (map (lambda (p)
                       (set! count (+ count 1))
                       (printf "~A\r" count)
                       (let ((end (bfs p)))
                         (if end (node-cost end) #f)))
                     zero-ps))
  (printf "Part 2: ~A\n" (fold (lambda (acc cost)
                                 (if (and acc cost)
                                     (min acc cost)
                                     (or acc cost)))
                               (car costs) (cdr costs))))

(main "12.dat")
