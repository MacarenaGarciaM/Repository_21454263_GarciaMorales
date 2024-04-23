#lang scheme
(provide (all-defined-out))
;;REQUERIMIENTO 3: Función que permite establecer enlaces entre dos estaciones.
; Dom: point1 (station)  X point2 (station) X distance (positive-number) X cost (positive-number U {0})
; Rec: section
;Tipo de recursividad: No aplica
;CONSTRUCTOR
(define section
  (lambda (point1 point2 distance cost)
    (list point1 point2 distance cost)))


;;GETTERS:

;;Descripción: Obtiene la primera estación
;Dom: Section
;rec: station1
;Tipo de recursión: No aplica
(define get-station1   
  (lambda (section)
    (car section)))

;;Descripción: Obtiene la segunda estación
;Dom:Section
;rec: station2
;Tipo de recursión: No aplica
(define get-station2 
  (lambda(section)
    (car(cdr section))))

;;Descripción: obiene el length de la sección
;Dom: Section
;rec: station1
;Tipo de recursión: No aplica
(define (get-section-length section)  
  (cadr (reverse section)))

;;Descripción: obiene el costo de la sección
;Dom: Section
;rec: positive number 
;Tipo de recursión: No aplica
(define (get-section-cost section)
  (car(reverse section)))

;;Descripción: obiene la distancia 
;Dom: Section
;rec: positive number
;Tipo de recursión: No aplica
(define get-distance
  (lambda (section)
    (car(cdr(cdr section)))))