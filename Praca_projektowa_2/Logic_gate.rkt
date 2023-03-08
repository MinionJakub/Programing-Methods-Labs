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

          [flip-flop (-> wire? wire? wire? void?)]))



;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;STRUKTURY

(struct sim
  ([time_of_simulation #:mutable]
   [action_queue #:mutable])
  #:transparent)

(struct wire
  ([condition #:mutable]
   [actions #:mutable]
   [owner])
  #:transparent)

(struct action
  ([time #:mutable]
   [function #:mutable])
  #:transparent)


;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;FUNKCJE POMOCNICZE

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Pomocnicze zwiazane ze struktura action

#|Funkcja do dodawnia akcji do istniejacego przewodu|#
(define (add-action! given_wire function)
  (set-wire-actions! given_wire (cons function (wire-actions given_wire))))

#|Funkcja do dodawnia akcji do kolejki akcji|#
(define (add-actions-to-queue wire_with_actions lists_of_actions simulation)
  (if (equal? null lists_of_actions)
      (void)
      (begin
        (heap-add! (sim-action_queue simulation) ((car lists_of_actions)))
        (add-actions-to-queue wire_with_actions (cdr lists_of_actions) simulation))))

#|Funkcja porównywań akcji (do tworzenia heap-a)|#
(define/contract (compare_actions first_action second_action)
  (-> action? action? boolean?)
  (< (action-time first_action) (action-time second_action)))

#|Funkcja która wykonuje akcje|#
(define (make-action given_action)
  ((action-function given_action)))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Pomocnicze do bramek

#|Funkcja do nie zaprzeczonych binarnych bramek|#
(define (double-action wire_output wire_first_input wire_second_input function time actual_time)
  (λ()(action
    (+ actual_time time)
    (λ()(wire-set! wire_output
               (function (wire-value wire_first_input) (wire-value wire_second_input)))))))

#|Funkcja do zaprzeczonych binarnych bramek |#
(define (double-not-action wire_output wire_first_input wire_second_input function time actual_time)
  (action
    (+ actual_time time)
    (λ()(wire-set! wire_output
               (not (function (wire-value wire_first_input) (wire-value wire_second_input)))))))

#|Funkcja do unarnych bramek|#
(define (unary-action wire_output wire_input function time actual_time)
  (λ()(action
   (+ actual_time time)
   (λ()(wire-set! wire_output
                  (function (wire-value wire_input)))))))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Pomocnicza zwiazane z symulacja

#|Funkcja do realizowania akcji na kolejce|#
(define (resolve-actions simulation)
  (if (or
       (= (heap-count (sim-action_queue simulation)) 0)
       (< (sim-time_of_simulation simulation) (action-time (heap-min (sim-action_queue simulation)))))
      (void)
      (begin
        (make-action (heap-min (sim-action_queue simulation)))
        (heap-remove-min! (sim-action_queue  simulation))
        (resolve-actions simulation))))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Pomocnicza zwiazana z wire 

#|Funkcja do ustawienia wartosci by nie bylo dziwnych stanow poczatkowych (nie wywoluje kaskady zmiany stanow)
- dla binarnych bramek logicznych nie zanegowanych|#
(define (change-value output  first_input second_input function)
  (set-wire-condition! output (function (wire-condition first_input) (wire-condition second_input))))

#|Funkcja do ustawienia wartosci by nie bylo dziwnych stanow poczatkowych (nie wywoluje kaskady zmiany stanow)
- dla binarnych bramek logicznych zanegowanych|#
(define (change-not-value output  first_input second_input function)
  (set-wire-condition! output (not (function (wire-condition first_input) (wire-condition second_input)))))

#|Funkcja do ustawienia wartosci by nie bylo dziwnych stanow poczatkowych (nie wywoluje kaskady zmiany stanow)
- dla unarnych bramek logicznych|#
(define (change-unary-value output  first_input function)
  (set-wire-condition! output (function (wire-condition first_input))))

;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;OPERACJE NA SYMULACJI

#|Utworzenie symulacji|#
(define/contract (make-sim)
  (-> sim?)
  (sim 0 (make-heap compare_actions)))

#|Funkcja zwracjaca czas|#
(define (sim-time simulation)
  (sim-time_of_simulation simulation))

#|Dodanie wydarzenia do symulacji|#
(define (sim-add-action! simulation time function)
  (heap-add! (sim-action_queue simulation)
             (action (+ (sim-time_of_simulation simulation) time)
                     (λ()function))))

#|Przesuniecie w czasie symulacji|#
(define (sim-wait! simulation time)
  (if (<= time 0)
      (void)
      (begin
        (set-sim-time_of_simulation! simulation (+ 1 (sim-time_of_simulation simulation)))
        (resolve-actions simulation)
        (sim-wait! simulation (- time 1)))))

;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;OPERACJE NA PRZEWODZIE 

#|Utworzenie przewodu|#
(define/contract (make-wire simulation)
  (-> sim? wire?)
  (wire #f null simulation))

#|Ustawienie wartosci przewodu oraz nastepnie dodanie jego akcji do kolejki|#
(define (wire-set! wire_to_set value)
  (begin
    (set-wire-condition! wire_to_set value)
    (add-actions-to-queue wire_to_set (wire-actions wire_to_set) (wire-owner wire_to_set))))

#|Funkcja zwracajaca stan przewodu|#
(define (wire-value conduit)
  (wire-condition conduit))

#|Funkcja do dodanie action do przewodu|#
(define (wire-on-change! conduit function)
  (begin
    (add-action! conduit (action 0 function))
    (function)
    (void)))


;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;BRAMKI LOGICZNE

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Binarne niezaprzeczone bramki logiczne

#|Bramka logiczna and|#
(define (gate-and wire_output wire_first_input wire_second_input)
  (if (and
       (equal? (wire-owner wire_first_input) (wire-owner wire_second_input))
       (equal? (wire-owner wire_first_input) (wire-owner wire_output)))
      (begin
        (add-action!
         wire_first_input
         (double-action wire_output wire_first_input wire_second_input (λ(x y)(and x y)) 1 (sim-time (wire-owner wire_output))))
        (add-action!
         wire_second_input
         (double-action wire_output wire_first_input wire_second_input (λ(x y)(and x y)) 1 (sim-time (wire-owner wire_output))))
        (change-value wire_output wire_first_input wire_second_input (λ(x y)(and x y))))
      (error "ONE OR MORE WIRES HAVE DIFFRENT OWNER THAN ANOTHER ONE")))

#|Bramka logiczna or|#
(define (gate-or wire_output wire_first_input wire_second_input)
  (if (and
       (equal? (wire-owner wire_first_input) (wire-owner wire_second_input))
       (equal? (wire-owner wire_first_input) (wire-owner wire_output)))
      (begin
        (add-action!
         wire_first_input
         (double-action wire_output wire_first_input wire_second_input (λ(x y)(or x y)) 1 (sim-time (wire-owner wire_output))))
        (add-action!
         wire_second_input
         (double-action wire_output wire_first_input wire_second_input (λ(x y)(or x y)) 1 (sim-time (wire-owner wire_output))))
        (change-value wire_output wire_first_input wire_second_input (λ(x y)(or x y))))
      (error "ONE OR MORE WIRES HAVE DIFFRENT OWNER THAN ANOTHER ONE")))

#|Bramka logiczna xor|#
(define (gate-xor wire_output wire_first_input wire_second_input)
  (if (and
       (equal? (wire-owner wire_first_input) (wire-owner wire_second_input))
       (equal? (wire-owner wire_first_input) (wire-owner wire_output)))
      (begin
        (add-action!
         wire_first_input
         (double-action wire_output wire_first_input wire_second_input (λ(x y)(xor x y)) 2 (sim-time (wire-owner wire_output))))
        (add-action!
         wire_second_input
         (double-action wire_output wire_first_input wire_second_input (λ(x y)(xor x y)) 2 (sim-time (wire-owner wire_output))))
        (change-value wire_output wire_first_input wire_second_input (λ(x y)(xor x y))))
      (error "ONE OR MORE WIRES HAVE DIFFRENT OWNER THAN ANOTHER ONE")))


;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Unarne bramki logiczne

#|Bramka logiczna not|#
(define (gate-not wire_output wire_input)
  (if (equal? (wire-owner wire_input) (wire-owner wire_output))
      (begin
        (add-action!
         wire_input
         (unary-action wire_output wire_input (λ(x)(not x)) 1 (sim-time (wire-owner wire_output))))
        (change-unary-value wire_output wire_input (λ(x)(not x))))
      (error "WIRES HAVE DIFFRENT OWNER THAN ANOTHER ONE")))


;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Binarne zaprzeczone bramki logiczne

#|Bramka logiczna nand|#
(define (gate-nand wire_output wire_first_input wire_second_input)
  (if (and
       (equal? (wire-owner wire_first_input) (wire-owner wire_second_input))
       (equal? (wire-owner wire_first_input) (wire-owner wire_output)))
      (begin
        (add-action!
         wire_first_input
         (double-not-action wire_output wire_first_input wire_second_input (λ(x y)(and x y)) 1))
        (add-action!
         wire_second_input
         (double-not-action wire_output wire_first_input wire_second_input (λ(x y)(and x y)) 1))
        (change-not-value wire_output wire_first_input wire_second_input (λ(x y)(and x y))))
      (error "ONE OR MORE WIRES HAVE DIFFRENT OWNER THAN ANOTHER ONE")))

#|Bramka logiczna nor|#
(define (gate-nor wire_output wire_first_input wire_second_input)
  (if (and
       (equal? (wire-owner wire_first_input) (wire-owner wire_second_input))
       (equal? (wire-owner wire_first_input) (wire-owner wire_output)))
      (begin
        (add-action!
         wire_first_input
         (double-not-action wire_output wire_first_input wire_second_input (λ(x y)(or x y)) 1))
        (add-action!
         wire_second_input
         (double-not-action wire_output wire_first_input wire_second_input (λ(x y)(or x y)) 1))
        (change-not-value wire_output wire_first_input wire_second_input (λ(x y)(or x y))))
      (error "ONE OR MORE WIRES HAVE DIFFRENT OWNER THAN ANOTHER ONE")))


;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;Polaczenie make-wire z gate-cos


;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Unarne

#|make-wire + gate-not|#
(define (wire-not input)
  (define output (make-wire (wire-owner input)))
  (begin
    (gate-not output input)
    output))


;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Binarne niezaprzeczone

#|make-wire + gate-and|#
(define (wire-and first_input second_input)
  (define output (make-wire (wire-owner first_input)))
  (begin
    (gate-and output first_input second_input)
    output))

#|make-wire + gate-or|#
(define (wire-or first_input second_input)
  (define output (make-wire (wire-owner first_input)))
  (begin
    (gate-or output first_input second_input)
    output))

#|make-wire + gate-xor|#
(define (wire-xor first_input second_input)
  (define output (make-wire (wire-owner first_input)))
  (begin
    (gate-xor output first_input second_input)
    output))

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;Binarne zaprzeczone

#|make-wire + gate-nand|#
(define (wire-nand first_input second_input)
  (define output (make-wire (wire-owner first_input)))
  (begin
    (gate-nand output first_input second_input)
    output))

#|make-wire + gate-nor|#
(define (wire-nor first_input second_input)
  (define output (make-wire (wire-owner first_input)))
  (begin
    (gate-nor output first_input second_input)
    output))

;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;RZECZY DANE OD GORNIE

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


;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;TESTY
#|
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
(sim-wait! add_simulation 1)
(check-equal? (wire-value output_1) #f)
(check-equal? (wire-value output_2) #t)

(define xor_neg_simulation (make-sim))
(define input_xor_neg_1 (make-wire xor_neg_simulation))
(define input_xor_neg_2 (make-wire xor_neg_simulation))
(define middle_input (make-wire xor_neg_simulation))
(define xor_neg_output (make-wire xor_neg_simulation))
(gate-xor xor_neg_output input_xor_neg_1 input_xor_neg_2)
(gate-not xor_neg_output middle_input)
(wire-set! input_xor_neg_1 #t)
(sim-wait! xor_neg_simulation 3)

(define xor_xor_simulation (make-sim))
(define input_xor_xor_1 (make-wire xor_xor_simulation))
(define input_xor_xor_2 (make-wire xor_xor_simulation))
(define input_xor_xor_3 (make-wire xor_xor_simulation))
(define middle_1 (make-wire xor_xor_simulation))
(define middle_2 (make-wire xor_xor_simulation))
(define output_xor_xor (make-wire xor_xor_simulation))
(gate-xor middle_1 input_xor_xor_1 input_xor_xor_2)
(gate-xor middle_2 input_xor_xor_3 input_xor_xor_2)
(gate-xor output_xor_xor middle_1 middle_2)
(wire-set! input_xor_xor_1 #t)

(sim-wait! xor_xor_simulation 3)|#