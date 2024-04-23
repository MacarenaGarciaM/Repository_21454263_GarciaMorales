#lang scheme
(provide (all-defined-out))
(define r "r")
(define m "m")
(define c "c")
(define t "t")

;; REQUERIMIENTO 2: Función constructora de una estación de metro, las que pueden ser estaciones del tipo: regular (r), mantención (m),
;;combinación (c) o terminal (t)
; Dom: id (int) X name (String)  X type (station-type) X stop-time (positive integer)
; Rec: Station
;Tipo de recursividad: No aplica
;CONSTRUCTOR
(define station 
  (lambda(id name type stop_time)    
    (list id name type stop_time)))

;;GETTERS:
;;Descripción: obiene el nombre de la station
;Dom: Station
;rec: name
;Tipo de recursión: No aplica
(define get-name    
  (lambda(station)
    (car(cdr station))))
;;Descripción: obiene el tipo de la station
;Dom: Station
;rec: type
;Tipo de recursión: No aplica
(define get-station-type
  (lambda (station)
    (caddr station)))

