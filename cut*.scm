(define-syntax cut*-help
  (syntax-rules (<>)
    ((_ k vars (a . d))
     (cut*-help (cut*-rec k d) vars a))
    ((_ (k ...) (vars ...) <>)
     (k ... (vars ... x) x))
    ((_ (k ...) vars x)
     (k ... vars x))))

(define-syntax cut*-rec
  (syntax-rules ()
    ((_ k d vars a)
     (cut*-help (cut*-rec2 k a) vars d))))

(define-syntax cut*-rec2
  (syntax-rules ()
    ((_ (k ...) a vars d)
     (k ... vars (a . d)))))

(define-syntax cut*-finish
  (syntax-rules ()
    ((_ vars form)
     (lambda vars . form))))

(define-syntax cut*
  (syntax-rules ()
    ((_ . form)
     (cut*-help (cut*-finish) () form))))
