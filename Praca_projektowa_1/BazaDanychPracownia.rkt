;Jakub Chomiczewski
#lang racket

(require rackunit)

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

;TESTY SA NA SAMYMY DOLE!!!!!!!!!!!!!!!!!!!!

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Wstawianie

;funkcja pomocnicza make_question_type
(define (make_question_type type)
  (cond [(equal? 'string type) (λ (x) (string? x))]
        [(equal? 'number type) (λ (x) (number? x))]
        [(equal? 'symbol type) (λ (x) (symbol? x))]
        [(equal? 'boolean type) (λ (x) (boolean? x))]))

;funkcja pomocnicza make_list_of_type
(define (make_list_of_type tablica)
  (foldr (λ (elem list_of_type) (cons (column-info-type elem) list_of_type)) null (table-schema tablica)))

;funkcja pomocnicza make_query_insert
(define (make_query_insert input list_of_types)
  (cond [(and (null? list_of_types) (null? input)) #t]
        [(xor (null? list_of_types) (null? input)) "Zla ilosc argumentow"]
        [else (if ((make_question_type (car list_of_types)) (car input))
                  (make_query_insert (cdr input) (cdr list_of_types))
                  (string-append "Zle wejscie \"" (~a(car input)) "\" typ powinien byc: " (symbol->string (car list_of_types))))])) 

;funkcja wstawiania
(define (table-insert row tab)
  (define var (make_query_insert row (make_list_of_type tab)))
  (if (boolean? var)
      (table (table-schema tab)
             (cons row (table-rows tab)))
      (error var)))

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Projekcja

;funkcja pomocnicza find_index_of_column
(define (find_index_of_column elem list_of_names num)
    (if (null? list_of_names)
        "Nie ma takiej kolmuny"
        (if (equal? (column-info-name (car list_of_names)) elem) num
            (find_index_of_column elem (cdr list_of_names) (+ num 1)))))

;funkcja pomocnicza make_list_of_indexes_col
(define (make_list_of_indexes_col columns tablica lista)
  (if(null? columns) lista
     (let ([elem (find_index_of_column(car columns) tablica 0)])
       (if (number? elem)
           (make_list_of_indexes_col (cdr columns) tablica (cons elem lista))
           (error elem)))))

;funkcja pomocnicza what_choose_from_columns
(define (what_choose_from_columns columns tablica)
  (make_list_of_indexes_col columns (table-schema tablica) null))

;funkcja pomocnicza make_list_of_chosen_columns
(define (make_list_of_chosen_columns columns tablica lista)
  (define list_of_indexes (what_choose_from_columns columns tablica))
  (foldr (λ (row lista_list)
           (cons(foldl
                 (λ (elem lista) (cons (list-ref row elem) lista))
                 null list_of_indexes)
                lista_list))
           null (table-rows tablica)))

;funkcja pomocnicza make_new_schema_for_projection
(define (make_new_schema_for_projection columns tablica lista)
  (define list_of_indexes (what_choose_from_columns columns tablica))
  (foldl (λ(elem lista) (cons (list-ref (table-schema tablica) elem) lista)) null list_of_indexes))

;funkcja projekcji
(define (table-project cols tab)
  (table
   (make_new_schema_for_projection cols tab null)
   (make_list_of_chosen_columns cols tab null)))


;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Zmiana nazwy

;funkcja pomocnicza table_renamed_schema
(define (table_renamed_schema column newcolumn tablica)
  (foldr (λ (elem lista)
           (if (equal? (column-info-name elem) column)
               (cons (column-info newcolumn (column-info-type elem)) lista)
               (cons elem lista))) null (table-schema tablica)))


;funkcja zmiany nazwy
(define (table-rename col ncol tab)
  (table (table_renamed_schema col ncol tab) (table-rows tab)))
  

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Sortowanie

;sortowanie jest leksykograficzne 

;funkcja pomocnicza split
(define (split xs)
  (define half (quotient(length xs) 2))
  (define (recursive xs lista i)
    (if (null? xs) lista
        (if (< i half)
            (let ([newlista (recursive (cdr xs) lista (+ i 1))]) (cons (cons (car xs) (car newlista)) (cdr newlista)))
            (let ([newlista (recursive (cdr xs) lista (+ i 1))]) (cons (car newlista) (cons (car xs)(cdr newlista)))))))
  (if (null? (cdr xs)) (list xs)
      (recursive xs (list (list)) 0)))

;funkcja pomocnicza merge
(define (merge right_list left_list index)
  (cond [(null? right_list) left_list]
        [(null? left_list) right_list]
        [else
         (let ([right_elem (list-ref (car right_list) index)])
           (let ([left_elem (list-ref (car left_list) index)])
             (if (string<=? (~a right_elem) (~a left_elem))
                    (cons (car right_list) (merge (cdr right_list) left_list index))
                    (cons (car left_list) (merge right_list (cdr left_list) index)))))]))


;funkcja pomocnicza merge-sort
(define (merge-sort lista index)
  (define var (split lista))
  (if (null? (cdr lista))
      lista
      (merge (merge-sort (car var) index) (merge-sort (cdr var) index) index)))

;funkcja pomocnicza
(define (mult_merge_sort tablica list_of_indexes)
  (if (null? list_of_indexes)
      tablica
      (mult_merge_sort (merge-sort tablica (car list_of_indexes)) (cdr list_of_indexes))))

;funkcja sortujaca
(define (table-sort cols tab) 
  (define list_of_indexes (make_list_of_indexes_col cols (table-schema tab) null))
  (table (table-schema tab) (mult_merge_sort (table-rows tab) list_of_indexes)))


;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

;funkcja pomocnicza make_list_under_condition
(define (make_list_under_condition condition lista index output)
  (if (null? lista)
      output
      (if (condition (list-ref (car lista) index))
          (cons #t (make_list_under_condition condition (cdr lista) index output))
          (cons #f (make_list_under_condition condition (cdr lista) index output)))))

;funkcja pomocnicza equal_question <- truth table
(define (equal_question condition tablica)
  (define index (find_index_of_column (eq-f-name condition) (table-schema tablica) 0))
  (make_list_under_condition (λ (x) (equal? x (eq-f-val condition))) (table-rows tablica) index null))
  

;funkcja pomocnicza less_than_question <- truth table
(define (less_than_question condition tablica)
  (define index (find_index_of_column (lt-f-name condition) (table-schema tablica) 0))
  (make_list_under_condition
   (λ (x) (cond [(number? x) (< x (lt-f-val condition))]
                [(string? x) (string<? x (lt-f-val condition))]
                [else (error "Unknown type to compare")]))
   (table-rows tablica) index null))


;funkcja pomocnicza if_two_columns_has_the_same_val
(define (if_two_columns_have_the_same_val condition tablica)
  (define index1 (find_index_of_column (eq2-f-name condition) (table-schema tablica) 0))
  (define index2 (find_index_of_column (eq2-f-name2 condition) (table-schema tablica) 0))
  (define (make_question_double index1 index2 lista output)
    (if(null? lista) output
       (if (equal? (list-ref (car lista) index1) (list-ref (car lista) index2))
           (cons #t (make_question_double index1 index2 (cdr lista) output))
           (cons #f (make_question_double index1 index2 (cdr lista) output)))))
  (make_question_double index1 index2 (table-rows tablica) null))

;funkcja pomocnicza and_truth_tables
(define (and_truth_tables right left)
  (if (and (null? right) (null? left))
      null
      (if (or (null? right) (null? left))
          (error "rozne rozmiary tablic")
          (cons (and (car right) (car left)) (and_truth_tables (cdr right) (cdr left))))))



;funkcja pomocnicza or_truth_tables
(define (or_truth_tables right left)
  (if (and (null? right) (null? left))
      null
      (if (or (null? right) (null? left))
          (error "rozne rozmiary tablic")
          (cons (or (car right) (car left)) (or_truth_tables (cdr right) (cdr left))))))

;funkcja pomocnicza not_truth_table
(define (not_truth_table truth_table)
  (if (null? truth_table) null
      (if (car truth_table)
          (cons #f (not_truth_table (cdr truth_table)))
          (cons #t (not_truth_table (cdr truth_table))))))

#|
(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))
|#

;funkcja pomocnicza unarm_form
(define (unarm_form form tab)
  (cond [(and-f? form) (and_truth_tables (unarm_form (and-f-r form) tab) (unarm_form (and-f-l form) tab))]
        [(or-f? form) (or_truth_tables (unarm_form (or-f-r form) tab) (unarm_form (or-f-l form) tab))]
        [(not-f? form) (not_truth_table (unarm_form (not-f-e form) tab))]
        [(eq-f? form) (equal_question form tab)]
        [(eq2-f? form) (if_two_columns_have_the_same_val form tab)]
        [(lt-f? form) (less_than_question form tab)]
        [else (error "Unkown form")]))

;funkcja pomocnicza create_list_from_truth_table
(define (create_list_from_truth_table truth_table tablica)
  (if(null? truth_table)
    null
    (if (car truth_table)
        (cons (car tablica) (create_list_from_truth_table (cdr truth_table) (cdr tablica)))
        (create_list_from_truth_table (cdr truth_table) (cdr tablica)))))

;funkcja selektujaca
(define (table-select form tab)
  (table
   (table-schema tab)
   (create_list_from_truth_table (unarm_form form tab) (table-rows tab))))

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


; Złączenie kartezjańskie

;funkcja pomocnicza cross_join_row_rows
(define (cross_join_row_rows row rows lista)
  (if (null? rows) lista
      (cons (append row (car rows)) (cross_join_row_rows row (cdr rows) lista))))

;funkcja pomocnicza cross_join_rows_rows
(define (cross_join_rows_rows rows1 rows2 lista)
  (if (null? rows1) lista
      (cross_join_row_rows (car rows1) rows2 (cross_join_rows_rows (cdr rows1) rows2 lista))))

;funkcja table-cross-join
(define (table-cross-join tab1 tab2)
  (table (append (table-schema tab1) (table-schema tab2)) (cross_join_rows_rows (table-rows tab1) (table-rows tab2) null)))


;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Złączenie

;funkcja pomocnicza find_the_same <- zwraca liste z nazwa kolumny jesli wystepuje ona w drugiej tabeli
(define (find_the_same name schemat lista)
    (if(null? schemat) lista
       (if(equal? name (car schemat))
          (cons name lista)
          (find_the_same name (cdr schemat) lista))))

;funkcja pomocnicza find_all_the_same_columns <- zwraca liste tych samych kolumn ze schematu
(define (find_all_the_same_columns schema1 schema2 lista)
  (if (null? schema1) lista
      (find_the_same (car schema1) schema2 (find_all_the_same_columns (cdr schema1) schema2 lista))))

;funkcja pomocnicza make_name_list <- zwraca liste samych nazw tych kolumn ktore sie powtarzaja
(define (make_name_list list_the_same lista)
  (if (null? list_the_same) lista
      (cons (column-info-name (car list_the_same)) (make_name_list (cdr list_the_same) lista))))

;funkcja pomocnicza rename_all_names <- zmienia nazwy w drugiej tablicy na liczby
(define (rename_all_names name_list tablica num)
  (if(null? name_list) (cons tablica num)
     (rename_all_names (cdr name_list) (table-rename (car name_list) num tablica) (+ num 1))))

;funkcja pomocnicza list_of_names_and_change_names <- zwarca liste par (nazwa liczba) gdzie liczba odpowiada zmienionej nazwie kolumny
(define (list_of_names_and_change_names list_of_names number acc lista)
  (if(= number acc) lista
     (cons (cons (car list_of_names) acc) (list_of_names_and_change_names (cdr list_of_names) number (+ acc 1) lista))))

;funkcja pomocnicza make_form <- tworzy formule do select na bazie list_of_names_and_change_names
(define (make_form list_of_change_names)
  (if(null? list_of_change_names)
     null
     (if(null? (cdr list_of_change_names))
        (eq2-f (car (car list_of_change_names)) (cdr (car list_of_change_names)))
        (and-f
         (eq2-f (car (car list_of_change_names)) (cdr (car list_of_change_names)))
         (make_form (cdr list_of_change_names))))))

;funkcja pomocnicza table_natural_join_with_too_many_colums <- robi naturalne zlaczenia ale zostawia niepotrzebne kolumny (jesli sa)
(define (table_natural_join_with_too_many_colums tablica1 tablica2)
  (define list_the_same (find_all_the_same_columns (table-schema tablica1) (table-schema tablica2) null))
  (define name_list (make_name_list list_the_same null))
  (define rename_and_number (rename_all_names name_list tablica2 0))
  (define list_of_change_names (list_of_names_and_change_names name_list (cdr rename_and_number) 0 null))
  (define form (make_form list_of_change_names))
  (if (null? form)
      (table-cross-join tablica1 (car rename_and_number))
      (table-select form (table-cross-join tablica1 (car rename_and_number)))))

;funkcja pomocnicza list_to_project <- tworzy liste kolumn do projekcji pomija liczby
(define (list_to_project schemat)
  (if (null? schemat) null
      (if (number? (column-info-name (car schemat)))
          (list_to_project (cdr schemat))
          (cons (column-info-name (car schemat)) (list_to_project (cdr schemat)))))) 
  
;funkcja zlaczenia
(define (table-natural-join tab1 tab2)
  (define too_many_columns (table_natural_join_with_too_many_colums tab1 tab2))
  (define projection_list (list_to_project (table-schema too_many_columns)))
  (if (= (length projection_list) (length (table-schema too_many_columns)))
      too_many_columns
      (table-project projection_list too_many_columns)))

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;TESTY

;testy do funkcji make-question
(check-equal? ((make_question_type 'string) "abc") #t "Sprawdzenie czy utworzy poprawne pytanie dla string")
(check-equal? ((make_question_type 'string) 1) #f "Sprawdzenie czy utworzy poprawne pytanie dla string")
(check-equal? ((make_question_type 'boolean) #t) #t "Sprawdzenie czy utworzy poprawne pytanie dla boolean")
(check-equal? ((make_question_type 'number) 1) #t "Sprawdzenie czy utworzy poprawne pytanie dla number")
(check-equal? ((make_question_type 'symbol) '+) #t "Sprawdzenie czy utworzy poprawne pytanie dla symbol")

;test do funkcji make_list_of_type
(check-equal? (make_list_of_type cities) '(string string number boolean) "Test poprawnej odpowiedzi")

;Testy do funkcji make_query_insert
(check-equal? (make_query_insert (list "Poland" '+) (make_list_of_type countries))
              "Zle wejscie \"+\" typ powinien byc: number"
              "Test blendu")
(check-equal? (make_query_insert (list "Poland" "Poland") (make_list_of_type countries))
              "Zle wejscie \"Poland\" typ powinien byc: number"
              "Test blendu")
(check-equal? (make_query_insert (list "Poland") (make_list_of_type countries))
              "Zla ilosc argumentow"
              "Test blendu")
(check-equal? (make_query_insert (list "Poland" 38) (make_list_of_type countries))
              #t "Test poprawnej odpowiedzi")

;Testy do funkcji table-insert
(check-equal? (table-rows (table-insert (list " Rzeszow " " Poland " 129 #f)cities))
        '((" Rzeszow " " Poland " 129 #f)
          ("Wrocław" "Poland" 293 #f)
          ("Warsaw" "Poland" 517 #t)
          ("Poznań" "Poland" 262 #f)
          ("Berlin" "Germany" 892 #t)
          ("Munich" "Germany" 310 #f)
          ("Paris" "France" 105 #t)
          ("Rennes" "France" 50 #f))
        "Test poprawnosci")
(check-exn
 exn:fail?
 (λ () (table-rows (table-insert (list " Rzeszow " " Poland " 129 "abc") cities)))) ;wymagania dokumentacji by uzyc λ

;Testy do funkcji find_index_of_column
(check-equal? (find_index_of_column 'area (table-schema cities) 0) 2 "Test czy zwraca index")
(check-equal? (find_index_of_column 'capitals (table-schema cities) 0) "Nie ma takiej kolmuny" "Test czy zwraca messege")

;Testy do funkcji make_list_of_indexes_col
(check-equal? (make_list_of_indexes_col '(city country) (table-schema cities) null) '(1 0) "Test czy zwarca liste")
(check-exn
 exn:fail?
 (λ ()(make_list_of_indexes_col '(city countries) (table-schema cities) null)) "Test erroru")

;Test do funkcji make_list_of_chosen_columns
(check-equal? (make_list_of_chosen_columns '(city country) cities null)
              '(("Wrocław" "Poland")
                ("Warsaw" "Poland")
                ("Poznań" "Poland")
                ("Berlin" "Germany")
                ("Munich" "Germany")
                ("Paris" "France")
                ("Rennes" "France"))
              "Test poprawnosci wyboru")

;Test do funkcji make_new_schema_for_projection
(check-equal? (make_new_schema_for_projection '(city country) cities null)
              (list (column-info 'city 'string) (column-info 'country 'string)) "Test poprawnego utworzenia schematu")

;Testy do funkcji table-project
(check-equal? ( table-project '(capital city country ) cities )
              (table
               (list (column-info 'capital 'boolean) (column-info 'city 'string) (column-info 'country 'string))
               '((#f "Wrocław" "Poland")
                 (#t "Warsaw" "Poland")
                 (#f "Poznań" "Poland")
                 (#t "Berlin" "Germany")
                 (#f "Munich" "Germany")
                 (#t "Paris" "France")
                 (#f "Rennes" "France")))
              "Test poprawnej projekcji")
(check-exn
 exn:fail? (λ()( table-project '(capital city countries ) cities )) "Test czy nie pozwoli dokonac projekcji z blednym wejsciem")


;test do funkcji table_renamed_schema
(check-equal?
 (table_renamed_schema 'city 'name cities)
 (list (column-info 'name 'string) (column-info 'country 'string) (column-info 'area 'number) (column-info 'capital 'boolean))
 "Test czy poprawny nowy schemat")


;testy do funkcji table-rename
(check-equal? ( table-rename 'city 'name cities )
              (table
               (list (column-info 'name 'string) (column-info 'country 'string)
                     (column-info 'area 'number) (column-info 'capital 'boolean))
               '(("Wrocław" "Poland" 293 #f)
                 ("Warsaw" "Poland" 517 #t)
                 ("Poznań" "Poland" 262 #f)
                 ("Berlin" "Germany" 892 #t)
                 ("Munich" "Germany" 310 #f)
                 ("Paris" "France" 105 #t)
                 ("Rennes" "France" 50 #f)))
              "Test czy zamieni city -> name")
(check-equal? ( table-rename 'area 'obszar cities )
              (table
               (list (column-info 'city 'string) (column-info 'country 'string)
                     (column-info 'obszar 'number) (column-info 'capital 'boolean))
               '(("Wrocław" "Poland" 293 #f)
                 ("Warsaw" "Poland" 517 #t)
                 ("Poznań" "Poland" 262 #f)
                 ("Berlin" "Germany" 892 #t)
                 ("Munich" "Germany" 310 #f)
                 ("Paris" "France" 105 #t)
                 ("Rennes" "France" 50 #f)))
              "Test czy zamieni area -> obszar")

;test do funkcji split
(check-equal?
 (split (table-rows cities))
 '((("Wrocław" "Poland" 293 #f) ("Warsaw" "Poland" 517 #t) ("Poznań" "Poland" 262 #f))
  ("Berlin" "Germany" 892 #t)
  ("Munich" "Germany" 310 #f)
  ("Paris" "France" 105 #t)
  ("Rennes" "France" 50 #f))
 "Test poprawnego splitu")

;testy do funkcji merge
(check-equal?
 (merge (list '(1 1) '(2 1) '(3 1)) (list '(1 2) '(3 2) '(4 2)) 0)
 '((1 1) (1 2) (2 1) (3 1) (3 2) (4 2))
 "Test merge po pierwszym elemencie")
(check-equal?
 (merge (list '(1 1) '(2 1) '(3 2)) (list '(3 1) '(2 2) '(4 2)) 1)
 '((1 1) (2 1) (3 1) (3 2) (2 2) (4 2))
 "Test merge po drugim elemencie")

;testy do funkcji merge-sort
(check-equal?
 (merge-sort (list '(1 1) '(2 1) '(3 2) '(1 2) '(2 2) '(4 1)) 0)
 '((1 1) (1 2) (2 1) (2 2) (3 2) (4 1))
 "Test sortowania po pierwszym elemencie")
(check-equal?
 (merge-sort (list '(1 1) '(2 1) '(3 2) '(1 2) '(2 2) '(4 1)) 1)
 '((1 1) (2 1) (4 1) (3 2) (1 2) (2 2))
 "Test sortowania po drugim elemencie")

;testy do table-sort
(check-equal?
 (table-rows (table-sort '(country city) cities))
 '(("Paris" "France" 105 #t)
  ("Rennes" "France" 50 #f)
  ("Berlin" "Germany" 892 #t)
  ("Munich" "Germany" 310 #f)
  ("Poznań" "Poland" 262 #f)
  ("Warsaw" "Poland" 517 #t)
  ("Wrocław" "Poland" 293 #f))
 "Test sortowania po kraj miasto")
(check-equal?
 (table-rows (table-sort '(capital area) cities))
 '(("Poznań" "Poland" 262 #f)
  ("Wrocław" "Poland" 293 #f)
  ("Munich" "Germany" 310 #f)
  ("Rennes" "France" 50 #f)
  ("Paris" "France" 105 #t)
  ("Warsaw" "Poland" 517 #t)
  ("Berlin" "Germany" 892 #t))
 "Test sortowania po stolica obszar") ; spowodu sortowania leksykograficznego 50 jest pozniej niz 310
(check-equal?
 (table-rows (table-sort '(population) countries))
 '(("Poland" 38) ("Spain" 47) ("France" 67) ("Germany" 83))
 "Testowanie po innej tablicy")

;testy do funkcji equal_question
(check-equal?
 (equal_question (eq-f 'capital #t) cities)
 '(#f #t #f #t #f #t #f)
 "Test generacji poprawnej truth-table dla warunku czy stolica")
(check-equal?
 (equal_question (eq-f 'city "Warsaw") cities)
 '(#f #t #f #f #f #f #f)
 "Test generacji poprawnej truth-table dla warunku czy miasto to Warszawa")


;testy do funkcji less_than_question
(check-equal?
 (less_than_question (lt-f 'area 200) cities)
 '(#f #f #f #f #f #t #t)
 "Test generacji poprawnej truth-table dla zapytania o obszar ponizej 200")
(check-equal?
 (less_than_question (lt-f 'city "Munich") cities)
 '(#f #f #f #t #f #f #f)
 "Test generacji poprawnej truth-table dla zapytania o nazwy miast leksykograficznie mniejsze od Munich")


;test do funkcji if_two_columns_has_the_same_val
(define help-test
  (table
   (list (column-info 'col1 'boolean) (column-info 'col2 'boolean))
   (list '(#t #t) '(#t #f) '(#f #f) '(#f #t))))
(check-equal?
 (if_two_columns_have_the_same_val (eq2-f 'col1 'col2) help-test)
 '(#t #f #t #f)
 "Test czy poprawnie porownuje dwie kolumny")


;testy do funkcji and_truth_tables
(check-equal?
 (and_truth_tables (less_than_question (lt-f 'area 400) cities) (less_than_question (lt-f 'city "Munich") cities))
 '(#f #f #f #f #f #f #f)
 "Test czy generuje poprawna truth-table po z-and-owaniu dwoch innych")
(check-equal?
 (and_truth_tables (less_than_question (lt-f 'area 400) cities) (less_than_question (lt-f 'city "Rennes") cities))
 '(#f #f #t #f #t #t #f)
 "Test2 czy generuje poprawna truth-table po z-and-owaniu dwoch innych")

;test do funkcji not_truth_table
(check-equal?
 (not_truth_table (and_truth_tables (less_than_question (lt-f 'area 400) cities) (less_than_question (lt-f 'city "Rennes") cities)))
 '(#t #t #f #t #f #f #t)
 "Test poprawnej negacji")


;test do funkcji selektujacej
(check-equal?
 (table-rows (table-select (and-f (eq-f 'capital #t) (not-f (lt-f 'area 300))) cities))
 '(("Warsaw" "Poland" 517 #t) ("Berlin" "Germany" 892 #t))
 "Test1 do funkcji selektujacej")
(check-equal?
 (table-rows (table-select (or-f (and-f (eq-f 'capital #t) (lt-f 'area 300)) (eq-f 'city "Berlin")) cities))
 '(("Berlin" "Germany" 892 #t) ("Paris" "France" 105 #t))
 "Test2 do funkcji selektujacej")

;test funkcji cross_join_rows_rows
(check-equal?
 (cross_join_rows_rows (table-rows cities) (table-rows countries) null)
 '(("Wrocław" "Poland" 293 #f "Poland" 38)
  ("Wrocław" "Poland" 293 #f "Germany" 83)
  ("Wrocław" "Poland" 293 #f "France" 67)
  ("Wrocław" "Poland" 293 #f "Spain" 47)
  ("Warsaw" "Poland" 517 #t "Poland" 38)
  ("Warsaw" "Poland" 517 #t "Germany" 83)
  ("Warsaw" "Poland" 517 #t "France" 67)
  ("Warsaw" "Poland" 517 #t "Spain" 47)
  ("Poznań" "Poland" 262 #f "Poland" 38)
  ("Poznań" "Poland" 262 #f "Germany" 83)
  ("Poznań" "Poland" 262 #f "France" 67)
  ("Poznań" "Poland" 262 #f "Spain" 47)
  ("Berlin" "Germany" 892 #t "Poland" 38)
  ("Berlin" "Germany" 892 #t "Germany" 83)
  ("Berlin" "Germany" 892 #t "France" 67)
  ("Berlin" "Germany" 892 #t "Spain" 47)
  ("Munich" "Germany" 310 #f "Poland" 38)
  ("Munich" "Germany" 310 #f "Germany" 83)
  ("Munich" "Germany" 310 #f "France" 67)
  ("Munich" "Germany" 310 #f "Spain" 47)
  ("Paris" "France" 105 #t "Poland" 38)
  ("Paris" "France" 105 #t "Germany" 83)
  ("Paris" "France" 105 #t "France" 67)
  ("Paris" "France" 105 #t "Spain" 47)
  ("Rennes" "France" 50 #f "Poland" 38)
  ("Rennes" "France" 50 #f "Germany" 83)
  ("Rennes" "France" 50 #f "France" 67)
  ("Rennes" "France" 50 #f "Spain" 47))
 "Test poprawnego iloczynu kartezjanskiego")

;Testy do table-cross-join
(check-equal?
 (table-cross-join cities (table-rename 'country 'country2 countries))
 (table
 (list
  (column-info 'city 'string)
  (column-info 'country 'string)
  (column-info 'area 'number)
  (column-info 'capital 'boolean)
  (column-info 'country2 'string)
  (column-info 'population 'number))
 '(("Wrocław" "Poland" 293 #f "Poland" 38)
   ("Wrocław" "Poland" 293 #f "Germany" 83)
   ("Wrocław" "Poland" 293 #f "France" 67)
   ("Wrocław" "Poland" 293 #f "Spain" 47)
   ("Warsaw" "Poland" 517 #t "Poland" 38)
   ("Warsaw" "Poland" 517 #t "Germany" 83)
   ("Warsaw" "Poland" 517 #t "France" 67)
   ("Warsaw" "Poland" 517 #t "Spain" 47)
   ("Poznań" "Poland" 262 #f "Poland" 38)
   ("Poznań" "Poland" 262 #f "Germany" 83)
   ("Poznań" "Poland" 262 #f "France" 67)
   ("Poznań" "Poland" 262 #f "Spain" 47)
   ("Berlin" "Germany" 892 #t "Poland" 38)
   ("Berlin" "Germany" 892 #t "Germany" 83)
   ("Berlin" "Germany" 892 #t "France" 67)
   ("Berlin" "Germany" 892 #t "Spain" 47)
   ("Munich" "Germany" 310 #f "Poland" 38)
   ("Munich" "Germany" 310 #f "Germany" 83)
   ("Munich" "Germany" 310 #f "France" 67)
   ("Munich" "Germany" 310 #f "Spain" 47)
   ("Paris" "France" 105 #t "Poland" 38)
   ("Paris" "France" 105 #t "Germany" 83)
   ("Paris" "France" 105 #t "France" 67)
   ("Paris" "France" 105 #t "Spain" 47)
   ("Rennes" "France" 50 #f "Poland" 38)
   ("Rennes" "France" 50 #f "Germany" 83)
   ("Rennes" "France" 50 #f "France" 67)
   ("Rennes" "France" 50 #f "Spain" 47)))
 "Test1 cross-join")
(check-equal?
 ( table-cross-join countries (table-rename 'country 'country2 cities))
 (table
 (list
  (column-info 'country 'string)
  (column-info 'population 'number)
  (column-info 'city 'string)
  (column-info 'country2 'string)
  (column-info 'area 'number)
  (column-info 'capital 'boolean))
 '(("Poland" 38 "Wrocław" "Poland" 293 #f)
   ("Poland" 38 "Warsaw" "Poland" 517 #t)
   ("Poland" 38 "Poznań" "Poland" 262 #f)
   ("Poland" 38 "Berlin" "Germany" 892 #t)
   ("Poland" 38 "Munich" "Germany" 310 #f)
   ("Poland" 38 "Paris" "France" 105 #t)
   ("Poland" 38 "Rennes" "France" 50 #f)
   ("Germany" 83 "Wrocław" "Poland" 293 #f)
   ("Germany" 83 "Warsaw" "Poland" 517 #t)
   ("Germany" 83 "Poznań" "Poland" 262 #f)
   ("Germany" 83 "Berlin" "Germany" 892 #t)
   ("Germany" 83 "Munich" "Germany" 310 #f)
   ("Germany" 83 "Paris" "France" 105 #t)
   ("Germany" 83 "Rennes" "France" 50 #f)
   ("France" 67 "Wrocław" "Poland" 293 #f)
   ("France" 67 "Warsaw" "Poland" 517 #t)
   ("France" 67 "Poznań" "Poland" 262 #f)
   ("France" 67 "Berlin" "Germany" 892 #t)
   ("France" 67 "Munich" "Germany" 310 #f)
   ("France" 67 "Paris" "France" 105 #t)
   ("France" 67 "Rennes" "France" 50 #f)
   ("Spain" 47 "Wrocław" "Poland" 293 #f)
   ("Spain" 47 "Warsaw" "Poland" 517 #t)
   ("Spain" 47 "Poznań" "Poland" 262 #f)
   ("Spain" 47 "Berlin" "Germany" 892 #t)
   ("Spain" 47 "Munich" "Germany" 310 #f)
   ("Spain" 47 "Paris" "France" 105 #t)
   ("Spain" 47 "Rennes" "France" 50 #f)))
  "Test2 cross-join")

 ;Testy do table-natural-join
(check-equal?
 ( table-natural-join cities countries )
 (table
 (list
  (column-info 'city 'string)
  (column-info 'country 'string)
  (column-info 'area 'number)
  (column-info 'capital 'boolean)
  (column-info 'population 'number))
 '(("Wrocław" "Poland" 293 #f 38)
   ("Warsaw" "Poland" 517 #t 38)
   ("Poznań" "Poland" 262 #f 38)
   ("Berlin" "Germany" 892 #t 83)
   ("Munich" "Germany" 310 #f 83)
   ("Paris" "France" 105 #t 67)
   ("Rennes" "France" 50 #f 67)))
 "Test poprawnosci joina")
(check-equal?
 ( table-natural-join cities countries )
 ( table-project '( city country area capital population )
( table-select ( eq2-f ' country ' country1 )
( table-cross-join cities
( table-rename ' country ' country1
countries ) ) ) )
 "Test czy oba zapytania daja ta sama wartosc (zgodnie z trescia powinny)")
(check-equal?
 ( table-natural-join cities help-test)
 (table-cross-join cities help-test)
 "Test poprawnosci joina wzgldem braku podonych kolumn")
