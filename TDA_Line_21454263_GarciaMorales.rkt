#lang scheme
(require "TDA_Section_21454263_GarciaMorales.rkt")
(require "TDA_Station_21454263_GarciaMorales.rkt")
(provide (all-defined-out))
;;REQUERIMIENTO 4: Función que permite crear una línea
;Dom: id (int) X name (string) X rail-type (string) X section* (* señala que se pueden agregar 0 o más tramos)
;Rec: line
;Tipo de recursividad: No aplica
;CONSTRUCTOR
(define line
  (lambda (id name rail-type . section)
    (cons id ( cons name (cons rail-type section)))))

;;REQUERIMIENTO 5: Función que permite determinar el largo total de una línea
;Dom: line (line)
;Rec: positive-number
;Tipo de recursividad: No aplica

(define line-length
  (lambda (line)
     (apply + (map get-distance(cdr(cdr (cdr line)))))))

;;REQUERIMIENTO 6: Función que permite determinar el trayecto entre una estación origen y una final.
;Dom: line (line) X station1-name (String) X station2-name (String)
;Rec: positive-number
;Tipo de recursividad: Recursividad de cola

(define (line-section-length line station1-name station2-name)
  (define (section-length-tail sections station1-name station2-name count acc)
    (cond
      ((null? sections) acc)
      ((= count 2) acc)
      ((equal? station1-name (car (car sections)))
       (section-length-tail (cdr sections) station1-name station2-name (+ count 1) (+ acc (get-section-length (car sections)))))
      ((equal? station2-name (car (car sections))) (+ count 1))
      (else
       (section-length-tail (cdr sections) station1-name station2-name count (+ acc (get-section-length (car sections)))))))
  (section-length-tail (get-section line station1-name station2-name) station1-name station2-name 0 0))

;;REQUERIMIENTO 7:  Función que permite determinar el costo total (monetario) de recorrer una línea.
;Dom: line (line)
;Rec: positive-number U {0}
;Tipo de recursividad: Recursión natural
(define es-circular?
  (lambda (line)
    (cond
      ((equal? (primera-estacion line) (ultima-estacion line)) #t)
      ((and (null? (primera-estacion line)) (null? (ultima-estacion line))) #f)
      (else #f))))

(define line-cost
  (lambda (line)
    (define get-section-cost
      (lambda (section)
        (if (null? section)
            0
            (cadr (cddr section)))))
    (define largo-circular
      (lambda (sections estacion-pivote)
        (define (calculate-total section total)
          (if (equal? (cadr section) estacion-pivote)
              total
              (+ total (get-section-cost section))))  
        (foldl calculate-total 0 sections)))
    (if (es-circular? line)
        (largo-circular (cdr (cdr (cdr line))) (primera-estacion line))
        (apply + (map get-section-cost (cdr (cdr (cdr line))))))))

;;REQUERIMIENTO 8:   Función que permite determinar el costo de un trayecto entre una estación origen y una final.
;Dom: line (line) X station1-name (String) X station2-name (String)
;Rec: positive-number U {0}
;Tipo de recursión: Recursión de cola
(define line-section-cost
  (lambda (line station1-name station2-name)
    (define section-cost-tail
      (lambda (lst station1-name station2-name count acc)
        (cond
          ((null? lst) acc)
          ((= count 2) acc)
          ((equal? station1-name (car (car lst)))
           (section-cost-tail (cdr lst) station1-name station2-name (+ count 1) (+ acc (get-section-cost (car lst)))))
          ((equal? station2-name (car (car lst))) (+ count 1))
          (else
           (section-cost-tail (cdr lst) station1-name station2-name count (+ acc (get-section-cost (car lst))))))))
    (section-cost-tail (get-section line station1-name station2-name) station1-name station2-name 0 0)))

;;REQUERIMIENTO 9: Función que permite añadir tramos a una línea
;Dom: line (line) X section (section)
;Rec: line
;Tipo de recursión: Recursión natural
(define line-add-section
  (lambda (line section)
    (cond
    ((null? line)(list section))
    ((equal? (car line) section)line)
    (else
    (cons (car line)(line-add-section(cdr line)section))))))
;;REQUERIMIENTO 10:  Función que permite determinar si un elemento cumple con las restricciones señaladas en apartados anteriores en relación a las estaciones y tramos para poder conformar una línea.
;Dom: line (line)
;Rec: boolean
;Tipo de recursión: Recursión natural

(define (same-type-stations? line)
  (define first-station-type (get-station-type (car(car (get-sections line)))))
  (define (all-same-type? station-list)
    (cond
      ((null? station-list) #t)
      ((not (equal? (get-station-type (car(car station-list))) first-station-type)) #f)
      (else (all-same-type? (cdr station-list)))))
  (all-same-type? (get-sections line)))

(define es-vacia?
  (lambda (line)
    (cond
      ((null? (cdr (cdr (cdr line)))) #t)
      (else #f))))

(define line?
  (lambda (line)
    (cond
      ((equal? #t (es-vacia? line)) #f)                 
      ((equal? #t (same-type-stations? line)) #t)       
      (else #t))))

;;Getters
;;Descripción: obiene el id de la línea
;Dom: line
;rec: id
;Tipo de recursión: No aplica
(define (get-id line)
  (car line))

;;Descripción: crea lista con las secciones que contengan las estaciones en una línea
;Dom: line X station1 X station2
;rec: list
;Tipo de recursión: Recursión de Cola
(define get-section  
  (lambda (line station1 station2)
    (define crear-lista 
      (lambda (sections acc)
        (cond
          ((null? sections) (reverse acc)) 
          ((equal? (get-name (get-station1 (car sections))) station2)
           (and (crear-lista (cdr sections) (cons (car sections) acc)) 
                (reverse acc)))
          ((equal? (get-name (get-station1 (car sections))) station1) 
           (cond
             ((not (equal? (get-name (get-station2 (car sections))) station2)) 
              (crear-lista (cdr sections) (cons (car sections) acc))) 
             (else (reverse acc)))) 
          (else (crear-lista (cdr sections) (cons (car sections) acc)))))) 
    (crear-lista (cdddr line) '())))

;;Descripción: obiene las secciones
;Dom: line
;rec: section
;Tipo de recursión: No aplica
(define get-sections
  (lambda (line)
    (cdr (cddr line))))

;;Descripción: obiene la primera estación de una linea
;Dom: line
;rec: firs-station
;Tipo de recursión: No aplica
(define primera-estacion
  (lambda (line)
    (cond
      ((null? (cdr (cdr (cdr line)))) '())
      (else
       (car (cdr (car (car (cdr (cdr (cdr line)))))))))))
;;Descripción: obiene la ultima estación de una linea
;Dom: line
;rec: last-station
;Tipo de recursión: No aplica
(define ultima-estacion
  (lambda (line)
    (cond
      ((null? (cdr (cdr (cdr line)))) '())
      (else
       (car (cdr (car (cdr (car (reverse line))))))))))
