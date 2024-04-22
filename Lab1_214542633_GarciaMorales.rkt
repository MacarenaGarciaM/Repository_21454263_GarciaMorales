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
    (car(cdr(cdr section))))) ;;creamos una función para obtener la distancia de las estaciones

(define line-length
  (lambda (line)
     (apply + (map get-distance(cdr(cdr (cdr line)))))))

;;REQUERIMIENTO 6: Función que permite determinar el trayecto entre una estación origen y una final.
;Dom: line (line) X station1-name (String) X station2-name (String)
;Rec: positive-number
(define (line-section-length line station1-name station2-name)
  (define (find-distance station1 station2 sections)
    (cond ((null? sections) #f)
          ((and (equal? (cadr (car sections)) station1) (equal? (cadr (cadr sections)) station2))
           (caddr (car sections)))
          (else (find-distance station1 station2 (cdr sections)))))

  (define (accumulate-distance station1 station2 sections)
    (cond ((null? sections) 0)
          ((and (equal? (cadr (car sections)) station1) (equal? (cadr (cadr sections)) station2))
           (caddr (car sections)))
          (else (+ (caddr (car sections))
                   (accumulate-distance station1 station2 (cdr sections))))))

  (let ((sections (cdr (cdr (cdr line)))))
    (let ((distance (accumulate-distance station1-name station2-name sections)))
      (if distance
          distance
          "Trayecto no encontrado"))))

;;REQUERIMIENTO 7:  Función que permite determinar el costo total (monetario) de recorrer una línea.
;Dom: line (line)
;Rec: positive-number U {0}

(define primera-estacion
  (lambda (line)
    (cond
      ((null? (cdr (cdr (cdr line)))) '())
      (else
       (car (cdr (car (car (cdr (cdr (cdr line)))))))))))

(define ultima-estacion
  (lambda (line)
    (cond
      ((null? (cdr (cdr (cdr line)))) '())
      (else
       (car (cdr (car (cdr (car (reverse line))))))))))

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
(define (get-section-cost section)
  (car(reverse section)))

(define (line-section-cost line st1 st2)
  (define (find-section-cost-tail list st1 st2 acum acc)
    (cond
      ((= acum 2) acc)
      ((null? list)acc)
      ((member st1 (car list))
       (find-section-cost-tail (cdr list) st1 st2 (+ 1 acum) (+ (get-section-cost (car list)) acc)))
      ((member st2 (car list)) (+ 1 acum))
      (else
       (find-section-cost-tail (cdr list) st1 st2 acum (+ (get-section-cost (car list)) acc)))))
  (find-section-cost-tail (cdddr line) st1 st2 0 0))


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

(define (same-type-stations? line)
  (define first-station-type (get-station-type (car(car (get-sections line)))))
  (define (all-same-type? station-list)
    (cond
      ((null? station-list) #t)
      ((not (equal? (get-station-type (car(car station-list))) first-station-type)) #f)
      (else (all-same-type? (cdr station-list)))))
  
  (all-same-type? (get-sections line)))

(define get-station-type
  (lambda (station)
    (caddr station)))

(define get-sections
  (lambda (line)
    (cdr (cddr line))))


(define es-vacia?
  (lambda (line)
    (cond
      ((null? (cdr (cdr (cdr line)))) #t)
      (else #f))))

(define line?
  (lambda (line)
    (cond
      ((equal? #t (es-vacia? line)) #f)                 ;; Verifica si la línea es vacía
      ((equal? #t (same-type-stations? line)) #t)       ;; Verifica si todas las estaciones son del mismo tipo
      (else #t))))

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

(define (get-pcar train)
  (cddr(cdddr train)))

(define get-type
  (lambda (pcar)
    (car(cddr(cdr pcar)))))

(define tr-type
  (lambda (pcars)
    (cond
      ((null? pcars) #t)
      ((and (equal? "tr" (get-type(car pcars)))
            (equal? "tr" (get-type(car (reverse pcars)))))
       #t)
      (else #f))))

(define ct-type
  (lambda (pcars)
    (cond
      ((null? pcars) #t) 
      ((null? (cdr pcars)) #t) 
      ((equal? "ct" (get-type(car pcars)))
       (ct-type(cdr pcars)))
      (else #f))))


(define train
  (lambda (id maker rail-type speed station-stay-time . pcar)
    (define train-list 
      (cons id (cons maker (cons rail-type (cons speed (cons station-stay-time pcar))))))
    (cond
      ((null? (get-pcar train-list)) train-list)
      ((and (equal? (tr-type (get-pcar train-list)) #t)
            (equal? (ct-type (get-pcar (cdr train-list))) #t)) train-list)
      (else '()))))

(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5))
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc1 pc0 pc3 pc2))
(define t2 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc1 pc0 pc3 pc4))


;;REQUERIMIENTO 13: Función que permite añadir carros a un tren en una posición dada.
;Dom: train (train) X pcar (pcar) X position (positive-integer U {0})
;Rec: train
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
(define train?
  (lambda (train)
    (cond
      ((null? (cdr (get-pcar train)))#f)
      ((null? train)#f)
      ((and (equal? (tr-type (get-pcar train)) #t)
            (equal? (ct-type (get-pcar (cdr train))) #t)) #t) ; Verifica si el tipo de tren es válido
      (else #f)))) ; Devuelve falso en cualquier otro caso

;;REQUERIMIENTO 16:Función que permite determinar la capacidad máxima de pasajeros del tren.
;Dom: train
;Rec: positive-number U {0}
(define get-capacity
  (lambda (pcar)
    (car(cdr pcar))))

(define train-capacity
  (lambda (train)
    (define capacity-tail
      (lambda (pcars acc)
        (cond
          ((null? pcars) acc)
          (else
           (capacity-tail (cdr pcars) (+ (get-capacity (car pcars)) acc))))))
    (capacity-tail (get-pcar train) 0)))

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

;;REQUERIMIENTO 19: Función que permite añadir trenes a una red de metro.
;Dom:sub (subway) X train+
;Rec: subway
(define subway-add-train
  (lambda (subway . trains)
    (if (null? trains)
        (list subway)
        (cons subway (apply subway-add-train trains)))))

;;REQUERIMIENTO 20: Función que permite añadir líneas a una red de metro.
;Dom: sub (subway) X line+
;Rec: subway
(define subway-add-line
  (lambda (subway . line)
    (list subway line)))

;;REQUERIMIENTO 21: Función que permite añadir conductores a una red de metro.
;Dom: sub (subway) X driver+ 
;Rec: subway
(define subway-add-driver
  (lambda (subway . driver)
    (list subway driver)))