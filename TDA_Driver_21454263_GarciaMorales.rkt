#lang scheme
(provide (all-defined-out))

;;REQUERIMIENTO 17:Función que permite crear un conductor cuya habilitación de conducción depende del fabricante de tren (train-maker)
;Dom: id (int) X nombre (string) X train-maker (string)
;Rec: driver
;Tipo de recursividad: No aplica
;CONSTRUCTOR
(define driver
  (lambda (id nombre train-maker)
    (list id nombre train-maker)))
