;; 11.scm - Robert Coffey - 2022-12-11 / 2022-12-12

(import (chicken format)
        (chicken io)
        (chicken sort)
        (chicken string)
        matchable
        vector-lib)

(define (last lst) (if (null? (cdr lst)) (car lst) (last (cdr lst))))
(define (drop lst n) (if (zero? n) lst (drop (cdr lst) (- n 1))))

(define-record monkey count items op test true false)

(define-record-printer (monkey mk op)
  (fprintf op "#<monkey ~A ~A>"
           (monkey-count mk)
           (reverse (monkey-items mk))))

(define (monkey-count-inc! monkey)
  (monkey-count-set! monkey (+ 1 (monkey-count monkey))))

(define (monkey-add-item! monkey item)
  (monkey-items-set! monkey (cons item (monkey-items monkey))))

(define (monkey-inspect-items! monkey monkeys prod)
  (define (inspect! item)
    (let* ((worry ((monkey-op monkey) item))
           (true (zero? (modulo worry (monkey-test monkey))))
           (targ ((if true monkey-true monkey-false) monkey)))
      (monkey-add-item! (vector-ref monkeys targ)
                        (modulo worry prod))))
  (let loop ((items (reverse (monkey-items monkey))))
    (unless (null? items)
      (inspect! (car items))
      (monkey-count-inc! monkey)
      (loop (cdr items))))
  (monkey-items-set! monkey '()))

(define (parse-op op arg)
  (lambda (old)
    (let* ((val (if (char-alphabetic? (string-ref arg 0))
                    old
                    (string->number arg)))
           (sp ((if (char=? (string-ref op 0) #\+) + *) old val)))
      #;(quotient sp 3)   ; part 1
      sp                ; part 2
      )))

(define (parse-monkey lines)
  (define last-number (compose string->number last string-split))
  (match lines
    ((_ l-items l-op l-test l-true l-false . _)
     (let ((items (reverse (map string->number
                                (cddr (string-split l-items " ,")))))
           (op (apply parse-op (drop (string-split l-op) 4)))
           (test (last-number l-test))
           (true (last-number l-true))
           (false (last-number l-false)))
       (make-monkey 0 items op test true false)))))

(define (parse-monkeys lines)
  (list->vector (map parse-monkey (chop lines 7))))

(define (update-monkeys! monkeys prod)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length monkeys)))
    (monkey-inspect-items! (vector-ref monkeys i) monkeys prod)))

(define (monkey-business monkeys)
  (define mks (sort monkeys (lambda (m1 m2)
                              (> (monkey-count m1) (monkey-count m2)))))
  (* (monkey-count (vector-ref mks 0)) (monkey-count (vector-ref mks 1))))

(define (print-monkeys monkeys)
  (vector-for-each (lambda (i m) (printf "  ~A\n" m))
                   monkeys))

(define (main path)
  (define lines (with-input-from-file path read-lines))
  (define monkeys (parse-monkeys lines))

  (define prod (vector-fold (lambda (i a m) (* a (monkey-test m))) 1 monkeys))

  ;; Part 1
  ;(print-monkeys monkeys)
  ;(do ((i 0 (+ i 1))) ((= i 20))
  ;  (printf "Round ~A...\n" (+ i 1))
  ;  (update-monkeys! monkeys prod)
  ;  (print-monkeys monkeys))
  ;(printf "Part 1: ~A\n" (monkey-business monkeys))

  ;; Part 2
  (print-monkeys monkeys)
  (do ((i 0 (+ i 1))) ((= i 10000))
    (printf "Round ~A...\n" (+ i 1))
    (update-monkeys! monkeys prod)
    (print-monkeys monkeys))
  (printf "Part 2: ~A\n" (monkey-business monkeys))
  )

(main "11.dat")
