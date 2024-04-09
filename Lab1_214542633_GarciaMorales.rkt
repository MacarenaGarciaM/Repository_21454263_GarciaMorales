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
e0
e2
e1