(load "./3.3.2.scm")

; A.1
; 上述のqueueの表現方法では項目のリストだけでなく、
; 先頭項目へのポインタと最後尾の項目へのポインタを
; consしたものも含めて印字してしまっていた
(define (print-all queue)
  (newline)
  (display queue))

(define q1 (make-queue))

(insert-queue! q1 'a)
(print-all q1)
(insert-queue! q1 'b)
(print-all q1)

(delete-queue! q1)
(print-all q1)
(delete-queue! q1)
(print-all q1)

; Benのテストでは、front-ptrとrear-ptrの内容を印字している。
; Eva Luはfront-ptrを印字するようにすればどうかと言っている。
; というわけで、上のプログラムをinsert-queue!, delete-queue!に組み込めばよい。

; A.2
(define (print-queue queue)
  (newline)
  (display (front-ptr queue)))

;(newline)
;(insert-queue! q1 'a)
;(print-queue q1)
;(insert-queue! q1 'b)
;(print-queue q1)
;
;(delete-queue! q1)
;(print-queue q1)
;(delete-queue! q1)
;(print-queue q1)

(newline)
