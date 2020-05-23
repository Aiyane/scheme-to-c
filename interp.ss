; 程序寄存器
(define-program-counter pc)
; 普通寄存器
(define-registers
  vo-to-eval
  vo-env-cps
  vo-k
  ak-k
  ak-v
  ae-env
  ae-y
  ae-k
  ac-e1
  ac-e2
  ac-k)

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (λ body)
  (app rator rand))

(define-union clos
  (closure body env-cps))

(define-union envr
  (empty-env)
  (extend-env v env-cps))

(define-union kt
  (k-mult-n1 x2 env-cps k^)
  (k-mult-n2 e0 k^)
  (k-sub1 k^)
  (k-zero? k^)
  (k-if conseq alt env-cps k^)
  (k-throw v-exp env-cps)
  (k-let body env-cps k^)
  (k-rator rand env-cps k^)
  (k-operand e12 k^)
  (empty-k jumpout))


(define-label value-of-cps
  (union-case vo-to-eval expr
    [(const const-expr)
      (begin [set! ak-k vo-k]
             [set! ak-v const-expr]
             (set! pc apply-k))]
    [(mult x1 x2)
      (begin [set! vo-to-eval x1]
             [set! vo-k (kt_k-mult-n1 x2 vo-env-cps vo-k)]
             (set! pc value-of-cps))]
    [(sub1 x)
      (begin [set! vo-to-eval x]
             [set! vo-k (kt_k-sub1 vo-k)]
             (set! pc value-of-cps))]
    [(zero x)
      (begin [set! vo-to-eval x]
             [set! vo-k (kt_k-zero? vo-k)]
             (set! pc value-of-cps))]
    [(if test conseq alt)
      (begin [set! vo-to-eval test]
             [set! vo-k (kt_k-if conseq alt vo-env-cps vo-k)]
             (set! pc value-of-cps))]
    [(letcc body)
      (begin [set! vo-to-eval body]
             [set! vo-env-cps (envr_extend-env vo-k vo-env-cps)]
             (set! pc value-of-cps))]
    [(throw k-exp v-exp)
      (begin [set! vo-to-eval k-exp]
             [set! vo-k (kt_k-throw v-exp vo-env-cps)]
             (set! pc value-of-cps))]
    [(let e body)
      (begin [set! vo-to-eval e]
             [set! vo-k (kt_k-let body vo-env-cps vo-k)]
             (set! pc value-of-cps))]
    [(var y)
      (begin [set! ae-env vo-env-cps]
             [set! ae-y y]
             [set! ae-k vo-k]
             (set! pc apply-env))]
    [(λ body)
      (begin [set! ak-k vo-k]
             [set! ak-v (clos_closure body vo-env-cps)]
             (set! pc apply-k))]
    [(app rator rand)
      (begin [set! vo-to-eval rator]
             [set! vo-k (kt_k-rator rand vo-env-cps vo-k)]
             (set! pc value-of-cps))]))

(define-label apply-k
  (union-case ak-k kt
    [(k-mult-n1 x2 env-cps k^)
      (begin [set! vo-to-eval x2]
             [set! vo-env-cps env-cps]
             [set! vo-k (kt_k-mult-n2 ak-v k^)]
             (set! pc value-of-cps))]
    [(k-mult-n2 e0 k^)
      (begin [set! ak-k k^]
             [set! ak-v (* e0 ak-v)]
             (set! pc apply-k))]
    [(k-sub1 k^)
      (begin [set! ak-k k^]
             [set! ak-v (sub1 ak-v)]
             (set! pc apply-k))]
    [(k-zero? k^)
      (begin [set! ak-k k^]
             [set! ak-v (zero? ak-v)]
             (set! pc apply-k))]
    [(k-if conseq alt env-cps k^)
      (if ak-v
          (begin [set! vo-to-eval conseq]
                 [set! vo-env-cps env-cps]
                 [set! vo-k k^]
                 (set! pc value-of-cps))
          (begin [set! vo-to-eval alt]
                 [set! vo-env-cps env-cps]
                 [set! vo-k k^]
                 (set! pc value-of-cps)))]
    [(k-throw v-exp env-cps)
      (begin [set! vo-to-eval v-exp]
             [set! vo-env-cps env-cps]
             [set! vo-k ak-v]
             (set! pc value-of-cps))]
    [(k-let body env-cps k^)
      (begin [set! vo-to-eval body]
             [set! vo-env-cps (envr_extend-env ak-v env-cps)]
             [set! vo-k k^]
             (set! pc value-of-cps))]
    [(k-rator rand env-cps k^)
      (begin [set! vo-to-eval rand]
             [set! vo-env-cps env-cps]
             [set! vo-k (kt_k-operand ak-v k^)]
             (set! pc value-of-cps))]
    [(k-operand e12 k^)
      (begin [set! ac-e1 e12]
             [set! ac-e2 ak-v]
             [set! ac-k k^]
             (set! pc apply-closure))]
    [(empty-k jumpout) (dismount-trampoline jumpout)]))

(define-label apply-env
  (union-case ae-env envr
    [(extend-env v env-cps)
      (if (zero? ae-y)
          (begin [set! ak-k ae-k]
                 [set! ak-v v]
                 (set! pc apply-k))
          (begin [set! ae-env env-cps]
                 [set! ae-y (sub1 ae-y)]
                 (set! pc apply-env)))]
    [(empty-env) (error 'value-of-cps "unbound variable")]))

(define-label apply-closure
  (union-case ac-e1 clos
    [(closure body env-cps)
      (begin [set! vo-to-eval body]
             [set! vo-env-cps (envr_extend-env ac-e2 env-cps)]
             [set! vo-k ac-k]
             (set! pc value-of-cps))]))
