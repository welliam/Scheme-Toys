(define-syntax anaphoric
  (syntax-rules (it)
    ((_ k var (a . d))
     (anaphoric (ana-rec k var d) var a))
    ((_ (k ...) var it)
     (k ... var))
    ((_ (k ...) var x)
     (k ... x))))

(define-syntax ana-rec
  (syntax-rules ()
    ((_ k var d a)
     (anaphoric (ana-rec2 k a) var d))))

(define-syntax ana-rec2
  (syntax-rules ()
    ((_ (k ...) a d)
     (k ... (a . d)))))

(define-syntax aif
  (syntax-rules ()
    ((_ pred then otherwise)
     (anaphoric (finish-ana-if x pred otherwise) x then))))

(define-syntax finish-ana-if
  (syntax-rules ()
    ((_ x pred otherwise then)
     (let ((x pred))
       (if x then otherwise)))))
