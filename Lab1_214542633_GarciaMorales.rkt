#lang scheme

(define r "r")
(define m "m")
(define c "c")
(define t "t")
;; REQUERIMIENTO 1: Función constructora de una estación de metro, las que pueden ser estaciones del tipo: regular (r), mantención (m),  combinación (c) o terminal (t)

; Dom: id (int) X name (String)  X type (station-type) X stop-time (positive integer)
; Rec: Station
(define station
  (lambda(id name type stop_time)
    (list id name type stop_time)))
(define e0 (station 1 "USACH" c 30))
(define e1 (station 2 "Estación Central" c 45))
(define e2 (station 3 "ULA" r 45))
(define e3 (station 0 "República" r 45)) ;;se cambió el id ya que se repetía con ULA
(define e4 (station 4 "Los Héroes" c 60))
(define e5 (station 5 "Toesca" r 40))
(define e6 (station 6 "La Moneda" r 40))
(define e7 (station 7 "Cochera" m 3600))
(define e8 (station 8 "Parque OHiggins" r 30))
(define e9 (station 9 "San Pablo" t 40))
(define e10 (station 10 "Los Dominicos" t 60))

;;REQUERIMIENTO 2: Función que permite establecer enlaces entre dos estaciones.
; Dom: point1 (station)  X point2 (station) X distance (positive-number) X cost (positive-number U {0})
; Rec: section
(define section
  (lambda (point1 point2 distance cost)
    (list point1 point2 distance cost)))
(define s0 (section e0 e1 2 50))
(define s1 (section e1 e2 2.5 55))
(define s2 (section e2 e3 1.5  30))
(define s3 (section e3 e4 3  45))
(define s4 (section e4 e5 3  45))
(define s5 (section e4 e6 1.4  50))
(define s6 (section e5 e8 2  40))
(define s7 (section e0 e7 3  0))
(define s8 (section e0 e9 7  100))
(define s9 (section e6 e10 15  250))

;;REQUERIMIENTO 3: Función que permite crear una línea
;Dom: id (int) X name (string) X rail-type (string) X section* (* señala que se pueden agregar 0 o más tramos)
;Rec: line
(define line
  (lambda (id name rail-type . section)
    (cons id ( cons name (cons rail-type section)))))

(define l0 (line 0 "Línea 0" "UIC 60 ASCE"))
(define l1 (line 1 "Línea 1" "100 R.E." s0 s1 s2 s3 s5 s7 s8 s9))

l0
l1