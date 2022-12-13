;; 13.scm - Robert Coffey - 2022-12-13

(import (chicken format)
        (chicken io)
        (chicken sort)
        (srfi 1)
        matchable
        string-tokenize)

(define (list-split pred lst #!optional (sub '()))
  (match lst
    ((x . xs) (if (pred x)
                  (cons (reverse sub) (list-split pred xs '()))
                  (list-split pred xs (cons x sub))))
    (() (cons (reverse sub) '()))))

;; P  -> [ PS | N
;; PS -> ] | P PS
(define (parse-packet str)
  (define toks (string-tokenize str "," "[]"))

  (define (next) (car toks))
  (define (consume!) (set! toks (cdr toks)))
  (define (match? tok) (string=? tok (next)))
  (define (done?) (null? toks))

  (define (P!)
    (cond ((done?) (error 'P "missing tokens"))
          ((match? "[") (consume!) (PS!))
          ((let ((num (string->number (next))))
             (if num
                 (begin (consume!) num)
                 (error 'P "invalid token" (next)))))))

  (define (PS!)
    (cond ((done?) (error 'PS "missing tokens"))
          ((match? "]") (consume!) '())
          (else (cons (P!) (PS!)))))

  (P!))

(define (packet<? l r)
  (cond ((and (number? l) (number? r))
         (cond ((< l r) #t)
               ((> l r) #f)
               (else '())))
        ((and (list? l) (list? r))
         (cond ((null? l) #t)
               ((null? r) #f)
               (else (let ((res (packet<? (car l) (car r))))
                       (cond ((eq? res #t) #t)
                             ((not res) #f)
                             ((null? res) (packet<? (cdr l) (cdr r))))))))
        ((number? l) (packet<? (list l) r))
        ((number? r) (packet<? l (list r)))))

(define (main path)
  (define lines (with-input-from-file path read-lines))
  (define pairs (map (lambda (ps) (list (parse-packet (car ps))
                                        (parse-packet (cadr ps))))
                     (list-split (lambda (l) (string=? l "")) lines)))

  ;; Part 1
  (define order (map (lambda (pair) (apply packet<? pair)) pairs))
  (define is '())
  (let loop ((os order) (i 1))
    (unless (null? os)
      (when (car os) (set! is (cons i is)))
      (loop (cdr os) (+ i 1))))
  (printf "Part 1: ~A\n" (apply + is))

  ;; Part 2
  (define dividers '(((2)) ((6))))
  (define packets (sort (apply append (cons dividers pairs)) packet<?))
  (define d1 (+ 1 (list-index (lambda (p) (equal? p '((2)))) packets)))
  (define d2 (+ 1 (list-index (lambda (p) (equal? p '((6)))) packets)))
  (printf "Part 2: ~A\n" (* d1 d2)))
