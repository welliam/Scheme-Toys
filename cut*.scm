(define-syntax insp
  (syntax-rules ()
    ((_ x ...) '(x ...))))

;; cut* ------------------------------------------

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

;; lift-expression (for cute*) -------------------

(define-syntax lift-expressions
  (syntax-rules ()
    ((_ (k ...) (kvs ...) (a . b))
     (has-cut? (a . b)
               (lift-expressions (lift-rec (k ...) b) (kvs ...) a)
               (k ... (kvs ... (x (a . b))) x)))
    ((_ (k ...) kvs x)
     (k ... kvs x))))

(define-syntax lift-rec
  (syntax-rules ()
    ((_ k b kvs a)
     (lift-expressions (lift-rec2 k a) kvs b))))

(define-syntax lift-rec2
  (syntax-rules ()
    ((_ (k ...) a kvs b)
     (k ... kvs (a . b)))))

(define-syntax has-cut?
  (syntax-rules (<>)
    ((_ (a . b) found not-found)
     (has-cut? a found (has-cut? b found not-found)))
    ((_ <> found not-found)
     found)
    ((_ x found not-found)
     not-found)))

;; cute* -----------------------------------------

(define-syntax cute*
  (syntax-rules ()
    ((_ . form)
     (lift-expressions (finish-cute*) () form))))

(define-syntax finish-cute*
  (syntax-rules ()
    ((_ kvs form)
     (cut*-help (finish-cute*-2 kvs) () form))))

(define-syntax finish-cute*-2
  (syntax-rules ()
    ((_ kvs vars form)
     (let kvs (lambda vars . form)))))
