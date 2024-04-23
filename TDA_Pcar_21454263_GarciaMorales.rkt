#lang scheme
(provide (all-defined-out))

;;REQUERIMIENTO 11: Permite crear los carros de pasajeros que conforman un convoy. Los carros pueden ser de tipo terminal (tr) o central (ct).
;Dom: id (int) X capacity (positive integer) X model (string) X type (car-type))
;Rec: pcar
;Tipo de recursividad: No aplica
;CONSTRUCTOR
(define tr "tr")
(define ct "ct")

(define pcar
  (lambda (id capacity model type)
    (list id capacity model type)))

;;GETTERS:
;;Descripción: obiene la el type del pcar
;Dom: pcar
;rec: type
;Tipo de recursión: No aplica
(define get-type
  (lambda (pcar)
    (car(cddr(cdr pcar)))))

;;Descripción: verifica si el pcar, de la lista de pcars, si el primero y ultimo son tr
;Dom: pcar
;rec: boolean
;Tipo de recursión: No aplica
(define tr-type
  (lambda (pcars)
    (cond
      ((null? pcars) #t)
      ((and (equal? "tr" (get-type(car pcars)))
            (equal? "tr" (get-type(car (reverse pcars)))))
       #t)
      (else #f))))
;;Descripción: verifica si el pcar, de la lista de pcars, si el centro son ct
;Dom: pcar
;rec: boolean
;Tipo de recursión: recursión natural
(define ct-type
  (lambda (pcars)
    (cond
      ((null? pcars) #t) 
      ((null? (cdr pcars)) #t) 
      ((equal? "ct" (get-type(car pcars)))
       (ct-type(cdr pcars)))
      (else #f))))
;;Descripción: obtiene la capacidad de un pcar
;Dom: pcar
;rec: positive number
;Tipo de recursión: No aplica
(define get-capacity
  (lambda (pcar)
    (car(cdr pcar))))