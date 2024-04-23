#lang scheme
(require "TDA_Line_21454263_GarciaMorales.rkt")
(provide (all-defined-out))
;;REQUERIMIENTO 18:Función que permite crear una red de metro.
;Dom: id (int) X nombre (string)
;Rec: subway
;Tipo de recursividad: No aplica
;;CONSTRUCTOR:
(define subway
  (lambda (id nombre)
    (list id nombre)))

;;REQUERIMIENTO 19: Función que permite añadir trenes a una red de metro.
;Dom:sub (subway) X train+
;Rec: subway
;Tipo de recursividad: Recursión natural
(define subway-add-train
  (lambda (subway . trains)
    (if (null? trains)
        (list subway)
        (cons subway (apply subway-add-train trains)))))

;;REQUERIMIENTO 20: Función que permite añadir líneas a una red de metro.
;Dom: sub (subway) X line+
;Rec: subway
;Tipo de recursividad: No aplica
(define (subway-add-line subway . line)
  (cond
    ((null? line) subway)
    ((equal?(get-id line) subway) subway)
    (else (cons (car subway) (cons (cdr subway) line)))))

;;REQUERIMIENTO 21: Función que permite añadir conductores a una red de metro.
;Dom: sub (subway) X driver+ 
;Rec: subway
;Tipo de recursividad: No aplica
(define subway-add-driver
  (lambda (subway . driver)
    (list subway driver)))