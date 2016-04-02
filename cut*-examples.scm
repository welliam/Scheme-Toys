((cut* <>) 3) ; 3
((cut* (* (<> 3 4) 3)) +) ; 21

(define f
  (quasi-cute* (<> ,(begin (print 'hi!) 1)))) ; prints: hi!

(f (lambda (x) (+ x 1))) ; 2

(define g
  (let* ((x 5)
         (g (quasi-cute*
             (<> ,(begin (print 'immediately!) x)
                 (cadr `(,(print 'application!) 5))))))
    (set! x #f)
    g)) ; prints: immediately!

(g +) ; 10, prints: application!
;; notice how it stops searching for unquotes when quasiquote is encountereed
