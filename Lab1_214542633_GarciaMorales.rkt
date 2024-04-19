#lang scheme

(define r "r")
(define m "m")
(define c "c")
(define t "t")
;; REQUERIMIENTO 2: Función constructora de una estación de metro, las que pueden ser estaciones del tipo: regular (r), mantención (m),  combinación (c) o terminal (t)

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



;;REQUERIMIENTO 3: Función que permite establecer enlaces entre dos estaciones.
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


;;REQUERIMIENTO 4: Función que permite crear una línea
;Dom: id (int) X name (string) X rail-type (string) X section* (* señala que se pueden agregar 0 o más tramos)
;Rec: line
(define line
  (lambda (id name rail-type . section)
    (cons id ( cons name (cons rail-type section)))))

(define l0 (line 0 "Línea 0" "UIC 60 ASCE"))
(define l1 (line 1 "Línea 1" "100 R.E." s0 s1 s2 s3 s5 s7 s8 s9))


;;REQUERIMIENTO 5: Función que permite determinar el largo total de una línea
;Dom: line (line)
;Rec: positive-number
(define get-distance
  (lambda (section)
    (car(car(cdr section))))) ;;creamos una función para obtener la distancia de las estaciones

(define line-length
  (lambda (line)
     (apply + (map get-distance(cdr(cdr (cdr line)))))))



;;REQUERIMIENTO 7:  Función que permite determinar el costo total (monetario) de recorrer una línea.
;Dom: line (line)
;Rec: positive-number U {0}

(define line-cost
  (lambda(line)
    (define get-cost
      (lambda(section)
        (if(null?(cdr section)) (car section)
           (get-cost (cdr section)))))
    (cond
      ((null? line)null)
      (else (apply + (map get-cost(cdr(cdr(cdr line)))))))))

;;REQUERIMIENTO 9: Función que permite añadir tramos a una línea
;Dom: line (line) X section (section)
;Rec: line
(define line-add-section
  (lambda (line section)
    (cond
    ((null? line)(list section))
    ((equal? (car line) section)line)
    (else
    (cons (car line)(line-add-section(cdr line)section))))))
(define l2 (line-add-section l0 s0))
(define l3 (line-add-section l2 s1))
(define l4 (line-add-section l3 s2))
(define l5 (line-add-section l4 s3))

;;REQUERIMIENTO 11: Permite crear los carros de pasajeros que conforman un convoy. Los carros pueden ser de tipo terminal (tr) o central (ct).
;Dom: id (int) X capacity (positive integer) X model (string) X type (car-type))
;Rec: pcar
(define tr "tr")
(define ct "ct")

(define pcar
  (lambda (id capacity model type)
    (list id capacity model type)))
(define pc0 (pcar 0 100 "NS-74" ct))
(define pc1 (pcar 1 100 "NS-74" tr))
(define pc2 (pcar 2 150  "NS-74" tr))
(define pc3 (pcar 3 100 "NS-74" ct))
(define pc4 (pcar 4 100 "AS-2014" ct))
(define pc5 (pcar 5 100 "AS-2014" ct))
(define pc6 (pcar 6 100 "AS-2016" ct))

;;REQUERIMIENTO 12:
;Dom:id (int) X maker (string) X rail-type (string) X speed (positive number) X station-stay-time (positive number U {0})
;    X pcar* (* indica que pueden especificarse 1 o más carros)
;Rec: train

(define (is-train? train)
  (define (train-int lst)
    (cond
      ((null? lst) #t)
      ((equal? "tr" (car lst))
       (cond
         ((equal? "tr" (car (reverse lst)))
          (train-int (cdr lst)))))
      ((member "ct" (car lst))
       (train-int (cdr lst)))
      (else #f)))
  (train-int (get-pcar train)))

(define (get-pcar train)
  (cddr(cddr (cdr train))))

(define train
  (lambda (id maker rail-type speed station-stay-time . pcar)
    (define crear-tren 
      (cons id (cons maker (cons rail-type (cons speed (cons station-stay-time pcar))))))
    (cond
      ((equal? #t (is-train? crear-tren))crear-tren)
      (else null)))) ; Devuelve una lista nula si el tren no es válido
(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5))
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc1 pc0 pc3 pc2))
(define t2 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc1 pc0 pc3 pc4))

;;REQUERIMIENTO 17:
;Dom: id (int) X nombre (string) X train-maker (string)
;Rec: driver

(define driver
  (lambda (id nombre train-maker)
    (list id nombre train-maker)))
(define d0 (driver 0 "name0" "CAF"))
(define d1 (driver 1 "name1" "CAF"))

;;REQUERIMIENTO 18:
;Dom: id (int) X nombre (string)
;Rec: subway

(define subway
  (lambda (id nombre)
    (list id nombre)))
(define sw0 (subway 0 "Metro Santiago"))
(define sw1 (subway 1 "Subte"))
(define sw2 (subway 2 "New York Subway"))