#lang scheme
(require "TDA_Pcar_21454263_GarciaMorales.rkt")
(provide (all-defined-out))
;;REQUERIMIENTO 12:
;Dom:id (int) X maker (string) X rail-type (string) X speed (positive number) X station-stay-time (positive number U {0})
;    X pcar* (* indica que pueden especificarse 1 o más carros)
;Rec: train
;Tipo de recursividad: No aplica
;CONSTRUCTOR
(define train
  (lambda (id maker rail-type speed station-stay-time . pcar)
    (define train-list 
      (cons id (cons maker (cons rail-type (cons speed (cons station-stay-time pcar))))))
    (cond
      ((null? (get-pcar train-list)) train-list)
      ((and (equal? (tr-type (get-pcar train-list)) #t)
            (equal? (ct-type (get-pcar (cdr train-list))) #t)) train-list)
      (else '()))))

;;REQUERIMIENTO 13: Función que permite añadir carros a un tren en una posición dada.
;Dom: train (train) X pcar (pcar) X position (positive-integer U {0})
;Rec: train
;Tipo de recursividad: Recursión de cola
(define train-add-car
  (lambda (train pcar posicion)
    (define (train-add-car-tail pcars pcar posicion cont acc)
      (cond
        ((null? pcars) (reverse (cons pcar acc))) 
        ((equal? posicion cont) (train-add-car-tail (cdr pcars) pcar posicion (+ 1 cont) (cons (car pcars) acc)))
        (else (train-add-car-tail (cdr pcars) pcar posicion (+ 1 cont) (cons (car pcars) acc)))))
    (train-add-car-tail train pcar posicion 0 '())))

;;REQUERIMIENTO 14: Función que permite eliminar un carro desde el convoy.
;Dom: train (train) X position (positive-integer U {0})
;Rec: train
;Tipo de recursividad: Recursión de cola
(define train-remove-car
  (lambda (train posicion)
    (define (train-remove-car-tail pcars posicion cont acc)
      (cond
        ((null? pcars) '()) 
        ((equal? posicion cont) (append (reverse acc) (cdr pcars))) 
        (else (train-remove-car-tail (cdr pcars) posicion (+ 1 cont) (cons (car pcars) acc))))) 
    (train-remove-car-tail train (+ 5 posicion) 0 '())))

;;REQUERIMIENTO 15: Función que permite determinar si un elemento es un tren válido, esto es, si el elemento tiene la estructura de tren y los carros que lo
;;conforman son compatibles (mismo modelo) y tienen una disposición coherente con carros terminales (tr) en los extremos y centrales (ct) en medio del convoy.
;Dom:train
;Rec:boolean
;Tipo de recursividad: Recursión natural
(define train?
  (lambda (train)
    (cond
      ((null? (get-pcar train)) #f)
      ((null? (cdr (get-pcar train)))#f) 
      ((null? train)#f)
      ((and (equal? (tr-type (get-pcar train)) #t)
            (equal? (ct-type (get-pcar (cdr train))) #t)) #t) 
      (else #f)))) 
;;REQUERIMIENTO 16:Función que permite determinar la capacidad máxima de pasajeros del tren.
;Dom: train
;Rec: positive-number U {0}
;Tipo de recursividad: Recursión de cola
(define train-capacity
  (lambda (train)
    (define capacity-tail
      (lambda (pcars acc)
        (cond
          ((null? pcars) acc)
          (else
           (capacity-tail (cdr pcars) (+ (get-capacity (car pcars)) acc))))))
    (capacity-tail (get-pcar train) 0)))

;;GETTERS:
;Descripción: obtiene los pcars de un tren
;Dom:train
;rec: pcars
;Tipo de recursión: No aplica
(define (get-pcar train)
  (cond
    ((null? train)null)
    (else(cddr(cdddr train)))))
