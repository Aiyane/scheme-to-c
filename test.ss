;; test
(let ((f (λ (f)
           (λ (n)
             (if (zero? n)
                 1
                 (* n ((f f) (sub1 n))))))))
  (* (letcc k ((f f) (throw k ((f f) 4)))) 5))
