;; 14.scm - Robert Coffey - 2022-12-15

(import (chicken io)
        (chicken string)
        (srfi 1)
        vector-lib)

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

(define-constant T-VOID 0)
(define-constant T-WALL 1)
(define-constant T-SAND 2)

(define (make-grid w h) (vector-unfold (lambda (i) (make-vector w T-VOID)) h))
(define (grid-h grid) (vector-length grid))
(define (grid-w grid) (vector-length (vector-ref grid 0)))
(define (grid-ref grid x y) (vector-ref (vector-ref grid y) x))
(define (grid-set! grid x y e) (vector-set! (vector-ref grid y) x e))

(define (grid-corners paths crack)
  (define poss (cons crack (apply append paths)))
  (define-values (x1 y1 x2 y2) (values #f #f #f #f))
  (let loop ((ps poss))
    (cond ((null? ps) (values x1 y1 x2 y2))
          (else (let ((x (caar ps))
                      (y (cadar ps)))
                  (when (or (not x1) (< x x1)) (set! x1 x))
                  (when (or (not y1) (< y y1)) (set! y1 y))
                  (when (or (not x2) (> x x2)) (set! x2 x))
                  (when (or (not y2) (> y y2)) (set! y2 y))
                  (loop (cdr ps)))))))

;; Adjust the coordinates of path to be relative to x1 and y1.
(define (path-adjust path x1 y1)
  (let loop ((ps path))
    (cond ((null? ps) '())
          (else (cons (map - (car ps) (list x1 y1))
                      (loop (cdr ps)))))))

;; Get a range of points which span two points of a path.
(define (path-range p1 p2)
  (define-values (x1 y1 x2 y2) (values (car p1) (cadr p1) (car p2) (cadr p2)))
  (if (= y1 y2) ; otherwise: x1 = x2
      (let* ((dist (abs (- x2 x1))))
        (zip (map + (iota (1+ dist)) (make-list (1+ dist) (min x1 x2)))
             (make-list (1+ dist) y1)))
      (let* ((dist (abs (- y2 y1))))
        (zip (make-list (1+ dist) x1)
             (map + (iota (1+ dist)) (make-list (1+ dist) (min y1 y2)))))))

(define (parse-grid paths crack-init)
  (define-values (x1 y1 x2 y2) (grid-corners paths crack-init))
  (define-values (w h) (values (1+ (- x2 x1)) (1+ (- y2 y1))))
  (set! y2 (+ y2 2)) (set! h (1+ (- y2 y1)))                    ; Part 2
  (set! x1 (- x1 h)) (set! x2 (+ x2 h)) (set! w (1+ (- x2 x1))) ; Part 2
  (define paths-off (map (lambda (p) (path-adjust p x1 y1)) paths))
  (define crack-off (map - crack-init (list x1 y1)))
  (define grid (make-grid w h))
  (define (add-rocks! path)
    (cond ((null? (cdr path)))
          (else (let ((range (path-range (car path) (cadr path))))
                  (let loop ((r range))
                    (cond ((null? r))
                          (else (grid-set! grid (caar r) (cadar r) T-WALL)
                                (loop (cdr r))))))
                (add-rocks! (cdr path)))))
  (for-each add-rocks! paths-off)
  (add-rocks! `((0 ,(1- h)) (,(1- w) ,(1- h)))) ; Part 2
  (values grid crack-off))

(define (can-fall? grid x y)
  (define-values (w h) (values (grid-w grid) (grid-h grid)))
  (and (>= x 0) (< x w) (>= y 0) (< y h) (= T-VOID (grid-ref grid x y))))

(define (drop-sand! grid crack)
  (let loop ((x (car crack))
             (y (cadr crack)))
    (let ((ny (1+ y)))
      (cond ((can-fall? grid x ny) (loop x ny))
            ((can-fall? grid (1- x) ny) (loop (1- x) ny))
            ((can-fall? grid (1+ x) ny) (loop (1+ x) ny))
            ;;((or (>= ny (1+ (grid-h grid))) (not (< 0 x (grid-h grid)))) #f) ; Part 1
            ((= T-SAND (grid-ref grid x y)) #f) ; Part 2
            (else (grid-set! grid x y T-SAND) #t)))))

(define (main path)
  (define paths (map (lambda (l) (map (lambda (p) (map string->number
                                                       (string-split p ",")))
                                      (string-split l " -> ")))
                     (with-input-from-file path read-lines)))
  (define crack-init '(500 0))

  ;; Part 1
  ;;(define-values (grid crack) (parse-grid paths crack-init))
  ;;(let loop () (when (drop-sand! grid crack) (loop)))
  ;;(display "Part 1: ")
  ;;(print (vector-fold (lambda (i acc row)
  ;;                      (+ acc (vector-count (lambda (i e) (= e T-SAND)) row)))
  ;;                    0 grid))

  ;; Part 2
  (define-values (grid crack) (parse-grid paths crack-init))
  (let loop () (when (drop-sand! grid crack) (loop)))
  (display "Part 2: ")
  (print (vector-fold (lambda (i acc row)
                        (+ acc (vector-count (lambda (i t) (= t T-SAND)) row)))
                      0 grid))
  )

(main "14.dat")
