(define-syntax insp
  (syntax-rules ()
    ((_ x ...)
     '(results: x ...))))

(define-syntax wrap-begin
  (syntax-rules ()
    ((_ (k ...) form)
     (k ... form))
    ((_ (k ...) form form* ...)
     (k ... (begin form form* ...)))))

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
     (lambda vars form))))

(define-syntax cut*
  (syntax-rules ()
    ((_ form form* ...)
     (wrap-begin (cut*-help (cut*-finish) ())
                 form form* ...))))

;; quasi-lift-expressions (for quasi-cute*) ------

(define-syntax quasi-lift-expressions
  (syntax-rules (quasiquote unquote)
    ((_ (k ...) kvs (quasiquote form))
     (k ... kvs (quasiquote form)))
    ((_ (k ...) (kvs ...) (unquote form))
     (k ... (kvs ... (x form)) x))
    ((_ (k ...) (kvs ...) (a . d))
     (quasi-lift-expressions
      (quasi-lift-rec (k ...) d) (kvs ...) a))
    ((_ (k ...) kvs form)
     (k ... kvs form))))

(define-syntax quasi-lift-rec
  (syntax-rules ()
    ((_ k b kvs a)
     (quasi-lift-expressions (quasi-lift-rec2 k a) kvs b))))

(define-syntax quasi-lift-rec2
  (syntax-rules ()
    ((_ (k ...) a kvs b)
     (k ... kvs (a . b)))))

;; quasi-cute* -----------------------------------

(define-syntax quasi-cute*
  (syntax-rules ()
    ((_ form form* ...)
     (wrap-begin (quasi-lift-expressions (finish-quasi-cute*) ())
                 form form* ...))))

(define-syntax finish-quasi-cute*
  (syntax-rules ()
    ((_ kvs form)
     (let kvs (cut* form)))))

;; interesting(?) results on chicken scheme (possibly others?) with
;; cute* and bindings to <>. for example, this is 'uh-oh:
;; (let ((<> 'uh-oh)) ((cute* <>)))
;; notice how cute* doesn't capture the <>... should it?
;;
;; i suppose this is consistent with behavior like the following being
;; unspecified rather than #t:
;; (let ((else #f)) (cond (else #t)))
