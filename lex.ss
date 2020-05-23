(load "util.ss")

(𝒅𝒆𝒇 list-index-ofv
  (λ (find lst)
    (cond
      [(null? lst) (errorf 'list-index-ofv "Bad data ~s" find)]
      [(eqv? find (car lst)) 0]
      [else (add1 (list-index-ofv find (cdr lst)))])))

(𝒅𝒆𝒇 lex
  (λ (exp env)
    (pmatch exp
      [,y (guard (symbol? y)) `(expr_var ,(list-index-ofv y env))]
      [(zero? ,n) `(expr_zero ,(lex n env))]
      [(sub1 ,n) `(expr_sub1 ,(lex n env))]
      [(* ,n₁ ,n₂) `(expr_mult ,(lex n₁ env) ,(lex n₂ env))]
      [(let ((,n ,f)) ,body) `(expr_let ,(lex f env) ,(lex body (cons n env)))]
      [(if ,test ,conseq ,alt) `(expr_if ,(lex test env) ,(lex conseq env) ,(lex alt env))]
      [(letcc ,var ,body) `(expr_letcc ,(lex body (cons var env)))]
      [(throw ,k ,v) `(expr_throw ,(lex k env) ,(lex v env))]
      [(λ (,x) ,body) `(expr_λ ,(lex body (cons x env)))]
      [(,rator ,rand) `(expr_app ,(lex rator env) ,(lex rand env))]
      [,n `(expr_const ,n)])))

(𝒅𝒆𝒇 run
  (λ (program)
    `(define-label main
       (begin [set! vo-to-eval 
                ,(lex program '())]
              [set! vo-env-cps (envr_empty-env)]
              (set! pc value-of-cps)
              (mount-trampoline kt_empty-k vo-k pc)
              (printf "~s\n" ak-v)))))
