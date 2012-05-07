(load "./eqt.scm")
(load "./2.4.1-1.scm")

(test-start "2.4.1 複素数表現")

(eqt (cons 100 50)
     (make-from-real-imag 100 50))

(let ((z (make-from-real-imag 100 50)))
  (eqt (cons 100.0 50.0)
       (make-from-mag-ang (magnitude z) (angle z))))

(let ((z1 (make-from-real-imag 100 50))
      (z2 (make-from-real-imag 30 20)))
  (eqt (cons 130 70) (add-complex z1 z2))
  (eqt (cons 70 30) (sub-complex z1 z2))
  (eqt (cons 2000.0000000000005 3499.999999999999) (mul-complex z1 z2))
  (eqt (cons 3.076923076923077 -0.3846153846153845) (div-complex z1 z2))
  )
(test-end)
