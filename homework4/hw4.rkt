
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
  (if (> low high) 
      '() 
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) 
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")] 
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n) 
  (if (= n 0) 
      '()
      (cons (car (s)) 
            (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
 (letrec ([f (lambda (x) 
               (cons (if (= (remainder x 5) 0) (- x) x)
                     (lambda () (f (+ x 1)))))])
   (lambda () (f 1))))

(define dan-then-dog
 (letrec ([f (lambda (x) 
               (cons (if (even? x) "dog.jpg" "dan.jpg") 
                     (lambda () (f (+ x 1)))))])
   (lambda () (f 1))))

(define (stream-add-zero s)
 (letrec ([f (lambda (x) 
               (cons (cons 0 (car (x))) 
                     (lambda () (f (cdr (x))))))])
   (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
             (cons (cons (list-nth-mod xs x)
                         (list-nth-mod ys x))
                   (lambda () (f (+ x 1)))))])
  (lambda ( ) (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (current-pos)
             (if (equal? current-pos (vector-length vec)) #f
                 (let ([ref (vector-ref vec current-pos)])
                   (cond [(and (pair? ref) (equal? (car ref) v)) ref]
                         [else (f (+ current-pos 1))]))))])
  (f 0)))

(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [pos 0]
          [f (lambda (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans
                     (cdr ans)
                     (let ([new-ans (assoc v xs)]) (if new-ans          
                                                      (begin
                                                        (vector-set! memo pos new-ans)
                                                        (set! pos (+ pos 1))
                                                        new-ans)
                                                      new-ans)))))])
    f))