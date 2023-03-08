#lang racket
(require data/heap)
(require rackunit)
(require racket/trace)

(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]
          ))


#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

#|STRUKTURY|#

(struct wire
  ([value #:mutable]
   [owner #:mutable]
   [actions #:mutable])
  #:transparent)

(struct action
  ([time]
   [output]
   [input_first]
   [input_second]
   [function])
  #:mutable #:transparent)

(struct wire_action
  ([name]
   [output]
   [input_first]
   [input_second]
   [function])
  #:mutable #:transparent)

(struct sim
  ([actual_time]
   [queue])
  #:mutable #:transparent)

#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

#|ACTION|#

;Porownianie struktur
(define (compare first_action second_action)
  (< (action-time first_action) (action-time second_action)))

;Tabelka opoznienia
(define delaytion
  (list
   (cons "and" 1)
   (cons "or" 1)
   (cons "nand" 1)
   (cons "nor" 1)
   (cons "not" 1)
   (cons "own_function" 0)
   (cons "xor" 2)))

;wire_action -> action
(define (wire_action->action give_action)
  (action
   (+ (cdr (assoc (wire_action-name give_action) delaytion))
      (sim-time (wire-owner (wire_action-output give_action))))
   (wire_action-output give_action)
   (wire_action-input_first give_action)
   (wire_action-input_second give_action)
   (wire_action-function give_action)))

#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

#|SIMULATION|#

;Tworzenie symulacji
(define (make-sim)
  (sim 0 (make-heap compare)))

;Funkcja zwracajaca czas symulacji
(define (sim-time simulation)
  (sim-actual_time simulation))

;Funkcja dodajaca element na heap
(define (add_event given_action simulation)
  (heap-add! (sim-queue simulation) (wire_action->action given_action)))

;Funkcja dodajaca wszystkie akcje zwiazane z wire
(define (add_all_events list_of_events simulation)
  (if (equal? null list_of_events)
      (void)
      (begin
        (add_event (car list_of_events) simulation)
        (add_all_events (cdr list_of_events) simulation))))

;Funkcja realizujaca event na heap
(define (do_event given_action simulation)
  (let ([previous_value (wire-value (action-output given_action))])
    (begin
      (set-wire-value! (action-output given_action)
                       ((action-function given_action) (wire-value (action-input_first given_action))
                                                       (wire-value(action-input_second given_action))))
      (if (xor (wire-value (action-output given_action)) previous_value)
          (add_all_events (wire-actions (action-output given_action))
                          (wire-owner (action-output given_action)))
          (void)))))

;Funkcja do zabierania elementu z heap-a az do okreslonej pory
(define (remove-events simulation time)
  (if (= 0 (heap-count (sim-queue simulation)))
      (void)
      (let ([event (heap-min (sim-queue simulation))])
        (if (>= time (action-time event))
            (begin
              (do_event event simulation)
              (heap-remove-min! (sim-queue simulation))
              (remove-events simulation time))
            (void)))))

;Funkcja przesuwajaca w przod symulacje
(define (sim-wait! simulation time)
  (if (<= time 0)
      (void)
      (begin
        (set-sim-actual_time! simulation (+ 1 (sim-time simulation)))
        (remove-events simulation (sim-time simulation))
        (sim-wait! simulation (- time 1)))))

;Funkcja dodajaca akcje do symulacji
(define (sim-add-action! simulation time function)
  (define event (action (+ (sim-time simulation) time)
                        (make-wire simulation)
                        (make-wire simulation)
                        (make-wire simulation)
                        (λ(x y) (function))))
  (heap-add! (sim-queue simulation) event))

#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

#|WIRE|#

;Tworzenie przewodnika
(define (make-wire simulation)
  (wire #f simulation null))

;Funkcja zmieniajaca wartosc przewodnika
(define (wire-set! given_wire new_value)
  (if (xor (wire-value given_wire) new_value)
      (begin
        (add_all_events (wire-actions given_wire) (wire-owner given_wire))
        (set-wire-value! given_wire new_value))
      (void)))

;Funkcje dodajaca event do przekaznika
(define (wire_event_dual name output input_1 input_2 function)
  (let ([event (wire_action name output input_1 input_2 function)])
    (begin
      (set-wire-actions! input_1 (cons event (wire-actions input_1)))
      (set-wire-actions! input_2 (cons event (wire-actions input_2))))))

(define (wire_event_unary name output input_1 function)
  (let ([event (wire_action name output input_1 input_1 function)])
    (begin
      (set-wire-actions! input_1 (cons event (wire-actions input_1))))))

;Funkcja dodajaca akcje do przekaznika
(define (wire-on-change! given_wire function)
  (define event (wire_action
                 "own_function"
                 (make-wire (wire-owner given_wire))
                 (make-wire (wire-owner given_wire))
                 (make-wire (wire-owner given_wire))
                 (λ(x y) (function))))
  (set-wire-actions! given_wire (cons event (wire-actions given_wire))))

#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#


#|GATES|#

(define (gate-not output input)
  (wire_event_unary "not" output input input (λ(x y) not(x))))

(define (gate-and output input_1 input_2)
  (wire_event_dual "and" output input_1 input_2 (λ(x y) (and x y))))

(define (gate-or output input_1 input_2)
  (wire_event_dual "or" output input_1 input_2 (λ(x y) (or x y))))

(define (gate-xor output input_1 input_2)
  (wire_event_dual "xor" output input_1 input_2 (λ(x y) (xor x y))))

(define (gate-nand output input_1 input_2)
  (wire_event_dual "nand" output input_1 input_2 (λ(x y) (not (and x y)))))

(define (gate-nor output input_1 input_2)
  (wire_event_dual "nor" output input_1 input_2 (λ(x y) (not (or x y)))))

(define (wire-not input)
  (begin
    (define return_value (make-wire (wire-owner input)))
    (set-wire-value! return_value (not(wire-value input)))
    (gate-not return_value input)
    return_value))

(define (wire-and input_1 input_2)
  (begin
    (define return_value (make-wire (wire-owner input_1)))
    (set-wire-value! return_value (and (wire-value input_1) (wire-value input_2)))
    (gate-and return_value input_1 input_2)
    return_value))

(define (wire-or input_1 input_2)
  (begin
    (define return_value (make-wire (wire-owner input_1)))
    (set-wire-value! return_value (or (wire-value input_1) (wire-value input_2)))
    (gate-or return_value input_1 input_2)
    return_value))

(define (wire-xor input_1 input_2)
  (begin
    (define return_value (make-wire (wire-owner input_1)))
    (set-wire-value! return_value (xor (wire-value input_1) (wire-value input_2)))
    (gate-xor return_value input_1 input_2)
    return_value))

(define (wire-nand input_1 input_2)
  (begin
    (define return_value (make-wire (wire-owner input_1)))
    (set-wire-value! return_value (not (and (wire-value input_1) (wire-value input_2))))
    (gate-nand return_value input_1 input_2)
    return_value))

(define (wire-nor input_1 input_2)
  (begin
    (define return_value (make-wire (wire-owner input_1)))
    (set-wire-value! return_value (not (or (wire-value input_1) (wire-value input_2))))
    (gate-nor return_value input_1 input_2)
    return_value))

#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

#|RZECZY DANE OD GORNIE|#

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

(define (flip-flop out clk data)
  (define sim (wire-owner data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))

#|
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

#|TESTY|#


(define test_simulation (make-sim))
(define wire_test_1 (make-wire test_simulation))
(define wire_test_2 (make-wire test_simulation))
(define wire_test_3 (make-wire test_simulation))

(gate-or wire_test_3 wire_test_1 wire_test_2)

(define add_simulation(make-sim))
(define wire_add_1 (make-wire add_simulation))
(define wire_add_2 (make-wire add_simulation))
(define output_1 (wire-xor wire_add_1 wire_add_2))
(define output_2 (wire-and wire_add_1 wire_add_2))

(wire-set! wire_add_1 #t)

(check-equal? (wire-value output_1) #f)
(check-equal? (wire-value output_2) #f)
;(sim-time add_simulation)
(sim-wait! add_simulation 2)
(check-equal? (wire-value output_1) #t)
(check-equal? (wire-value output_2) #f)
(wire-set! wire_add_2 #t)
(sim-wait! add_simulation 2)
(check-equal? (wire-value output_1) #f)
(check-equal? (wire-value output_2) #t)


(define mult_simulation(make-sim))
(define cabel_A0 (make-wire mult_simulation))
(define cabel_A1 (make-wire mult_simulation))
(define cabel_B0 (make-wire mult_simulation))
(define cabel_B1 (make-wire mult_simulation))
(define cabel_H1 (wire-and cabel_A0 cabel_B1))
(define cabel_H2 (wire-and cabel_A1 cabel_B0))
(define cabel_H3 (wire-and cabel_A1 cabel_B1))
(define cabel_H4 (wire-and cabel_H1 cabel_H2))
(define cabel_O1 (wire-and cabel_A0 cabel_B0))
(define cabel_O2 (wire-xor cabel_H1 cabel_H2))
(define cabel_O3 (wire-xor cabel_H4 cabel_H3))
(define cabel_O4 (wire-and cabel_H4 cabel_H3))
(wire-set! cabel_A0 #t)
(sim-wait! mult_simulation 10)
(check-equal? (wire-value cabel_O1) #f)
(check-equal? (wire-value cabel_O2) #f)
(check-equal? (wire-value cabel_O3) #f)
(check-equal? (wire-value cabel_O4) #f)
(wire-set! cabel_B0 #t)
(sim-wait! mult_simulation 10)
(check-equal? (wire-value cabel_O1) #t)
(check-equal? (wire-value cabel_O2) #f)
(check-equal? (wire-value cabel_O3) #f)
(check-equal? (wire-value cabel_O4) #f)
(wire-set! cabel_B0 #f)
(wire-set! cabel_B1 #t)
(sim-wait! mult_simulation 10)
(check-equal? (wire-value cabel_O1) #f)
(check-equal? (wire-value cabel_O2) #t)
(check-equal? (wire-value cabel_O3) #f)
(check-equal? (wire-value cabel_O4) #f)


(define clips_simulation(make-sim))
(define cabel_first_input (make-wire clips_simulation))
(define cabel_second_input (make-wire clips_simulation))
(define cabel_first_output (make-wire clips_simulation))
(define cabel_second_output (make-wire clips_simulation))
(gate-nand cabel_first_output cabel_first_input cabel_second_output)
(gate-nand cabel_second_output cabel_second_input cabel_first_output)
(wire-set! cabel_first_input #t)
(sim-wait! clips_simulation 20)
(check-equal? (wire-value cabel_first_output) #f)
(check-equal? (wire-value cabel_second_output) #t)
(wire-set! cabel_second_input #t)
(wire-set! cabel_first_input #f)
(sim-wait! clips_simulation 20)
(check-equal? (wire-value cabel_first_output) #t)
(check-equal? (wire-value cabel_second_output) #f)
(wire-set! cabel_first_input #t)
(sim-wait! clips_simulation 20)
(check-equal? (wire-value cabel_first_output) #t)
(check-equal? (wire-value cabel_second_output) #f)
