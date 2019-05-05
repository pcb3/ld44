;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cursim) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; pcb3 https://github.com/pcb3/ld44
; Cursim: a simulator of exchange

(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define SIZE 20)
(define WIDTH (* SIZE SIZE))
(define SCENE-SIZE WIDTH)
(define MT (empty-scene SCENE-SIZE SCENE-SIZE))
(define MAX SCENE-SIZE)
(define GEM-SIZE 14)
(define FRUIT-SIZE 10)
(define ANGLE 90)
(define TOMATO "Tomato")
(define AQUAMARINE "Aquamarine")
(define VIOLET "violet")
(define CORNFLOWERBLUE "Cornflowerblue")
(define GOLD "gold")
(define CYCLE-PER-SEASON 10)
(define GEM-THRESHOLD 100)
(define FRUIT-THRESHOLD 100)

; graphical constants
(define (create-gem g) (rhombus GEM-SIZE ANGLE "solid" (gem-colour g)))
(define (create-fruit f) (circle FRUIT-SIZE "solid" (fruit-colour f))) 
(define SCENE
  (scene+line
   (scene+line MT 0 (* SCENE-SIZE 0.75) SCENE-SIZE (* SCENE-SIZE 0.75) 'black)
   0 (* SCENE-SIZE 0.25) SCENE-SIZE (* SCENE-SIZE 0.25) 'black))

; structures
(define-struct currency [jewel input output season economy]) 
(define-struct gem [colour position delta sound])
(define-struct nutrient [type symbol delta sound])
(define-struct climate [type symbol delta sound])
(define-struct fruit [type colour position delta sound])
(define-struct season (quarter delta sound))
(define-struct economy (supply demand gdp inflation))

; Interpretation

; a Gem is a structure
; (make-gem String Posn String)
; a gem is a unit of currency. Inputs are exchanged into
; gems that when combined are exchanged again into outputs

(define RUBY (make-gem TOMATO (make-posn SIZE SIZE) 1 ""))
(define TURQUOISE (make-gem AQUAMARINE (make-posn (* 2 SIZE) (* 2 SIZE)) 1 ""))
(define QUARTZ (make-gem VIOLET (make-posn (* 3 SIZE) (* 3 SIZE)) 1 ""))
(define SAPHIRE (make-gem CORNFLOWERBLUE (make-posn (* 4 SIZE) (* 4 SIZE)) 1 ""))
(define OPAL (make-gem GOLD (make-posn (* 5 SIZE) (* 5 SIZE)) 1 ""))

; a Nutrient is a structure
; (make-nutrient String String Number String)
; a nutrient represents an input; Nitrogen, Calcium, Copper etc.

(define N2 (make-nutrient "Nitrogen" "N2" 1 ""))
(define CA (make-nutrient "Calcium" "Ca" 1 ""))
(define CU (make-nutrient "Copper" "Cu" 1 ""))

; a Climate is a structure
; (make-climate String String Number String)
; a climate is an input into; Oxygen, UV, precipitation, temperature

(define O2 (make-nutrient "Oxygen" "O2" 1 ""))
(define LIGHT (make-nutrient "Light" "EMW" 1 ""))
(define H20 (make-nutrient "Precipitation" "H2O" 1 ""))
(define TEMPERATURE (make-nutrient "Temperature" "C" 1 ""))

; a Fruit is a structure
; (make-fruit String String Posn Number String)
; a fruit is an output

(define BLOOD (make-fruit "Blood orange" TOMATO (make-posn 0 0) 1 ""))
(define LIME (make-fruit "Lime" AQUAMARINE (make-posn 0 0) 1 ""))
(define PEACH (make-fruit "Peach" VIOLET (make-posn 0 0) 1 ""))
(define BERRY (make-fruit "Blueberry" CORNFLOWERBLUE (make-posn 0 0) 1 ""))
(define MANGO (make-fruit "Mango" GOLD (make-posn 0 0) 1 ""))

; a Season is a structure
; (make-season String Number String)
; a season represents the dynamic force
; acting upon the inputs and otputs of the system

(define SUMMER (make-season "Summer" 4 ""))
(define SPRING (make-season "Spring" 3 ""))
(define AUTUMN (make-season "Autumn" 2 ""))
(define WINTER (make-season "Winter" 1 ""))

; an Economy is a structure
; (make-economoy Number Number Number Number)
; an economy represents the empiric value of the system donated in gems

(define ECONOMY0 (make-economy 0 0 0 0))

; a Jewel is one of:
; - '()
; (cons gem jewel)
; a Jewel is a list of gems

(define JEWEL0 '())

(define JEWEL1 (list RUBY TURQUOISE QUARTZ SAPHIRE OPAL))

; an Input is one of:
; - '()
; - (cons nutrient input)
; - (cons climate input)
; - (cons season input)
; an input is a list of external inputs that create currency (gems)
; by the tree

(define INPUT0 '())

(define INPUT1 (list CA CU N2 O2 H20 LIGHT TEMPERATURE))

(define INPUT2 (list CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE
                     CA CU N2 O2 H20 LIGHT TEMPERATURE))

; an output is one of:
; - '()
; - (cons fruit output)
; - (cons climate output)
; - (cons nutrient output)
; an oupput is a list exports that go back into the system and
; exhaust currency (gems)

(define OUTPUT0 '())

(define OUTPUT1 (list BLOOD LIME PEACH BERRY MANGO))

; a Currency is a structure
; (make-currency Jewel Input Output Season Economy)
; currency is the state of the program. It is represented by a tree.
; Each field represents an interaction with the system. 

(define CURRENCY0 (make-currency '() '() '()
                                 SPRING
                                 ECONOMY0))

(define CURRENCY1 (make-currency JEWEL1 INPUT1 OUTPUT1 SUMMER ECONOMY0))

; functions

; Currency -> Image
; consumes a currency c, and draws an image to the screen

(check-expect (render-jewel CURRENCY0 MT) MT)

(check-expect (render-jewel
               (make-currency (list RUBY) '() '() '() '()) MT)
              (place-image (create-gem RUBY) 20 20 MT))

(define (fn-render-jewel c im)
  (cond
    [(empty? (currency-jewel c)) ...]
    [else (... (create-gem (first (currency-jewel c)))
               (posn-x (gem-position (first (currency-jewel c))))
               (posn-y (gem-position (first (currency-jewel c))))
               (fn-render-jewel (make-currency
                                 (rest (currency-jewel c))
                                 (currency-input c)
                                 (currency-output c)
                                 (currency-season c)
                                 (currency-economy c)) ...))]))

(define (render-jewel c im)
  (cond
    [(empty? (currency-jewel c)) im]
    [else (place-image (create-gem (first (currency-jewel c)))
                       (posn-x (gem-position (first (currency-jewel c))))
                       (posn-y (gem-position (first (currency-jewel c))))
                       (render-jewel (make-currency
                                      (rest (currency-jewel c))
                                      (currency-input c)
                                      (currency-output c)
                                      (currency-season c)
                                      (currency-economy c)) im))]))

; Currency -> Image
; consumes a currency c, and draws an image to the screen

(check-expect (render-output CURRENCY0 MT) MT)

(check-expect (render-output
               (make-currency '() '() (list BLOOD) '() '()) MT)
              (place-image (create-fruit BLOOD) 0 0 MT))

(define (fn-render-output c im)
  (cond
    [(empty? (currency-output c)) ...]
    [else (... (create-fruit (first (currency-output c)))
               (posn-x (fruit-position (first (currency-output c))))
               (posn-y (fruit-position (first (currency-output c))))
               (fn-render-output (make-currency
                                  (currency-jewel c)
                                  (currency-input c)
                                  (rest (currency-output c))
                                  (currency-season c)
                                  (currency-economy c)) ...))]))

(define (render-output c im)
  (cond
    [(empty? (currency-output c)) im]
    [else (place-image (create-fruit (first (currency-output c)))
                       (posn-x (fruit-position (first (currency-output c))))
                       (posn-y (fruit-position (first (currency-output c))))
                       (render-output (make-currency
                                       (currency-jewel c)
                                       (currency-input c)
                                       (rest (currency-output c))
                                       (currency-season c)
                                       (currency-economy c)) im))]))

; Currency -> Image
; consumes a currency c, and draws an image to the screen

(check-expect
 (render
  (make-currency
   (list RUBY)
   '()
   (list BLOOD)
   WINTER ECONOMY0))
 (place-image
  (create-gem RUBY)
  20 20
  (place-image
   (create-fruit BLOOD) 0 0
   (overlay/align/offset
    "middle" "top"
    (beside (text
             (string-append "| Supply: " (number->string
                                          (economy-supply ECONOMY0))) 16 'black)
            (text
             (string-append " | Gems: " (number->string
                                         (economy-demand ECONOMY0))) 16 'black)
            (text (string-append " | Fruit: "
                                 (number->string
                                  (economy-gdp ECONOMY0)) " |")
                  16 'black)) -5 -5 MT))))
                                  
(define (fn-render c)
  (render-jewel c
                (render-output c
                               (render-economy c ...))))

(define (render c)
  (render-jewel c
                (render-output c
                               (render-economy c MT))))

; Currency MouseEvent -> Currency
; consumes a currency c, a mouse event me, and outputs a new currency

(define (click c me) c)

; Currency -> Image
; consumes a currency c, and draws the economy info bar to the screen

(check-expect (render-economy
               (make-currency '() '() '() '() ECONOMY0) MT)
              (overlay/align/offset
               "center" "top"
               (beside (text
                        (string-append "| Supply: " (number->string
                                                     (economy-supply ECONOMY0))) 16 'black)
                       (text
                        (string-append " | Gems: " (number->string
                                                    (economy-demand ECONOMY0))) 16 'black)
                       (text (string-append " | Fruit: "
                                            (number->string
                                             (economy-gdp ECONOMY0)) " |")
                             16 'black))
               -5 -5 MT))

(define (fn-render-economy c im)
  (overlay/align/offset
   ... ...
   (beside
    (text
     (string-append ...
      (number->string (economy-demand (currency-economy c)))) ... ...)
    (text (string-append ...
           (number->string (economy-supply (currency-economy c)))) ... ...)
    (text (string-append ...
           (number->string (economy-gdp (currency-economy c)))) ...))
   ... ... ...))

(define (render-economy c im)
  (overlay/align/offset
   "center" "top"
   (beside
    (text
     (string-append "| Supply: "
                    (number->string
                     (economy-supply (currency-economy c)))) 16 'black)
    (text (string-append " | Gems: "
                         (number->string
                          (economy-demand (currency-economy c)))) 16 'black)
    (text (string-append " | Fruit: "
                         (number->string
                          (economy-gdp (currency-economy c))) " |") 16 'black))
   -5 -5 im))

; Currency -> Currency
; consumes a currency c and updates it each tick

(check-expect (checked-tock CURRENCY0) CURRENCY0)

(check-expect (checked-tock
               (make-currency (list RUBY) '() (list BLOOD) WINTER ECONOMY0))
              (make-currency (list RUBY)
                             (list O2)
                             (list BLOOD)
                             WINTER ECONOMY0))

(define (checked-tock c) c)

(define (fn-tock c)
  (make-currency
   (cond
     [else (if (gem-threshold? c)
               (generate-gem c)
               (currency-jewel c))])
   (generate-nutrient c)
   (cond
     [else (if (fruit-threshold? (currency-jewel c))
               (generate-fruit c)
               (currency-output c))])
   (cond
     (else (if (change-season? c)
               (set-season c)
               (currency-season c))))
   (update-economy c)))

(define (tock c)
  (make-currency
   (cond
     [else (if (gem-threshold? c)
               (generate-gem c)
               (currency-jewel c))])
   (generate-nutrient c)
   (cond
     [else (if (fruit-threshold? (currency-jewel c))
               (generate-fruit c)
               (currency-output c))])
   (cond (else (if (change-season? c)
                   (set-season c)
                   (currency-season c))))
   (update-economy c)))
    
; Currency -> Boolean
; consumes a currency c, returns true if the required number of
; inputs have been reached to make a new gem

(check-expect (gem-threshold? CURRENCY0) #false)

(check-expect (gem-threshold?
               (make-currency '() INPUT2 '() WINTER ECONOMY0)) #false)

(check-expect (gem-threshold?
               (make-currency '() INPUT2 '() AUTUMN ECONOMY0)) #true) 

(define (fn-gem-threshold? c)
  (cond
    [else (if (... (... (length (currency-input c))
                        (season-delta (currency-season c))) ...)
              ...
              ...)]))

(define (gem-threshold? c)
  (cond
    [else (if (>= (* (length (currency-input c))
                     (season-delta (currency-season c))) GEM-THRESHOLD)
              #true
              #false)]))

; Currency -> Currency
; consumes a currency c, outputs a randomly
; generated gem and adds it to jewel

(check-expect (checked-generate-gem CURRENCY0)
              (make-currency
               (cons RUBY (currency-jewel CURRENCY0))
               '() '() SPRING ECONOMY0))

(define (checked-generate-gem c)
  (make-currency
   (cons RUBY (currency-jewel c))
   (currency-input c)
   (currency-output c)
   (currency-season c)
   (currency-economy c)))

(define (fn-generate-gem c)
  (...
   (... (... ...) (currency-jewel c))
   (currency-input c)
   (currency-output c)
   (currency-season c)
   (currecny-economy c)))
                          
(define (generate-gem c)
  (make-currency
   (cons (random-gem c 5) (currency-jewel c))
   (currency-input c)
   (currency-output c)
   (currency-season c)
   (currency-economy c)))

; Number -> Gem
; consumes a currency c, number n, and outputs a randomly selected gem
; with a position determined by jewel

(check-expect (checked-random-gem CURRENCY0 0) RUBY)

(define (checked-random-gem c n)
  (cond [(equal? n 0) RUBY]
        [(equal? n 1) TURQUOISE]
        [(equal? n 2) QUARTZ]
        [(equal? n 3) SAPHIRE]
        [(equal? n 4) OPAL]))

(define (fn-random-gem c n)
  (cond
    [(zero? (random n))
     (make-gem
      ...
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn ... ...)
             (make-posn
              (... (posn-x (gem-position (first (currency-jewel c))))
                   ...)
              (... (posn-y (gem-position (first (currency-jewel c))))
                   ...)))])
      ... "")]
    [(equal? (random n) ...)
     (make-gem
      ...
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn ... ...)
             (make-posn
              (... (posn-x (gem-position (first (currency-jewel c))))
                   ...)
              (... (posn-y (gem-position (first (currency-jewel c))))
                   ...)))])
      ... "")]
    [(equal? (random n) ...)
     (make-gem
      ...
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn ... ...)
             (make-posn
              (... (posn-x (gem-position (first (currency-jewel c))))
                   ...)
              (... (posn-y (gem-position (first (currency-jewel c))))
                   ...)))])
      ... "")]
    [(equal? (random n) ...)
     (make-gem
      ...
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn ... ...)
             (make-posn
              (... (posn-x (gem-position (first (currency-jewel c))))
                   ...)
              (... (posn-y (gem-position (first (currency-jewel c))))
                   ...)))])
      ... "")]
    [(equal? (random n) ...)
     (make-gem
      ...
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn ... ...)
             (make-posn
              (... (posn-x (gem-position (first (currency-jewel c))))
                   ...)
              (... (posn-y (gem-position (first (currency-jewel c))))
                   ...)))])
      ... "")]))
    
(define (random-gem c n)
  (cond
    [(zero? (random n))
     (make-gem
      "Ruby"
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn SIZE SIZE)
             (make-posn
              (+ (posn-x (gem-position (first (currency-jewel c))))
                 SIZE)
              (+ (posn-y (gem-position (first (currency-jewel c))))
                 SIZE)))])
      1 "")]
    [(equal? (random n) 1)
     (make-gem
      "Turquoise"
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn SIZE SIZE)
             (make-posn
              (+ (posn-x (gem-position (first (currency-jewel c))))
                 SIZE)
              (+ (posn-y (gem-position (first (currency-jewel c))))
                 SIZE)))])
      1 "")]
    [(equal? (random n) 2)
     (make-gem
      "Quartz"
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn SIZE SIZE)
             (make-posn
              (+ (posn-x (gem-position (first (currency-jewel c))))
                 SIZE)
              (+ (posn-y (gem-position (first (currency-jewel c))))
                 SIZE)))])
      1 "")]
    [(equal? (random n) 3)
     (make-gem
      "Saphire"
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn SIZE SIZE)
             (make-posn
              (+ (posn-x (gem-position (first (currency-jewel c))))
                 SIZE)
              (+ (posn-y (gem-position (first (currency-jewel c))))
                 SIZE)))])
      1 "")]
    [(equal? (random n) 4)
     (make-gem
      "Opal"
      (cond
        [else
         (if (empty? (currency-jewel c))
             (make-posn SIZE SIZE)
             (make-posn
              (+ (posn-x (gem-position (first (currency-jewel c))))
                 SIZE)
              (+ (posn-y (gem-position (first (currency-jewel c))))
                 SIZE)))])
      1 "")]))

; Currency -> Currency
; consumes a currency c, randomly generates a nutrient, adds it to the list,
; and returns input

(check-expect (generate-nutrient CURRENCY0)
              (make-currency '() (list O2) '() WINTER ECONOMY0))

(define (checked-generate-nutrient c)
  (make-currency
   (currency-jewel c)
   (cons O2 (currency-input c))
   (currency-output c)
   (currency-season c)
   (currency-economy c)))

(define (fn-generate-nutrient c)
  (...
   (currency-jewel c)
   (... (... (...)) (currency-input c))
   (currency-output c)
   (currency-season c)
   (currency-economy c)))
     
(define (generate-nutrient c)
  (make-currency
   (currency-jewel c)
   (cons (random-nutrient (random 6)) (currency-input c))
   (currency-output c)
   (currency-season c)
   (currency-economy c)))

; Currency Number -> Nutrient
; consumes a currency c and a number n, and outputs a random nutrient

(check-expect (random-nutrient 3) O2)

(define (fn-random-nutrient n)
  (cond [(zero? n) CA]
        [(equal? n 1) CU]
        [(equal? n 2) N2]
        [(equal? n 3) O2]
        [(equal? n 4) H20]
        [(equal? n 5) LIGHT]
        [(equal? n 6) TEMPERATURE]))

(define (random-nutrient n)
  (cond [(zero? n) CA]
        [(equal? n 1) CU]
        [(equal? n 2) N2]
        [(equal? n 3) O2]
        [(equal? n 4) H20]
        [(equal? n 5) LIGHT]
        [(equal? n 6) TEMPERATURE]))

; Currency -> Currency
; consumes a jewel j, returns true if the the number and type of gems
; to generate a new fruit has been reached

(check-expect (fruit-threshold? '()) #false)

(check-expect (fruit-threshold? (list SAPHIRE OPAL SAPHIRE OPAL SAPHIRE)) #true)

(check-expect (fruit-threshold? (list RUBY RUBY RUBY)) #true)

;(define (fn-fruit-threshold? j)
;  (cond
;    [(empty? j) ...]
;    [else (if (>= (length j) 


(define (fruit-threshold? j) #false)

; Currency -> Currency
; consumes a currency c, generates a new fruit dependent on jewel composition,
; adds it to the list and returns output

(check-expect (checked-generate-fruit CURRENCY0)
              (make-currency
               '() '() (cons BLOOD (currency-output CURRENCY0)) SPRING ECONOMY0))

(define (checked-generate-fruit c)
  (make-currency
   (currency-jewel c)
   (currency-input c)
   (cons BLOOD (currency-output c))
   (currency-season c)
   (currency-economy c)))

(define (fn-generate-fruit c)
  (...
   (currency-jewel c)
   (currency-input c)
   (... ... (currency-fruit c))
   (currency-season c)
   (currecny-economy c)))
                          
(define (generate-fruit c)
  (make-currency
   (currency-jewel c)
   (currency-input c)
   (cons (random-fruit c 5) (currency-output c))
   (currency-season c)
   (currency-economy c)))

; Currency -> Currency
; consumes a currency c, a number n, and generates a random fruit
; at a specific position

(define (fn-random-fruit c n)
  (cond
    [(zero? (random n))
     (make-fruit
      ... ...
      (create-position c ...)
      ... ...)]
    [(equal? (random n) 1)
     (make-fruit
      ... ...
      (create-position c ...)
      ... ...)]
    [(equal? (random n) 2)
     (make-fruit
      ... ...
      (create-position c ...)
      ... ...)]
    [(equal? (random n) 3)
     (make-fruit
      ... ...
      (create-position c ...)
      ... ...)]
    [(equal? (random n) 4)
     (make-fruit
      ... ...
      (create-position c ...)
      ... ...)]))

(define (random-fruit c n)
  (cond
    [(zero? (random n))
     (make-fruit
      "Blood orange" TOMATO
      (create-position c BLOOD)
      1 "")]
    [(equal? (random n) 1)
     (make-fruit
      "Lime" AQUAMARINE
      (create-position c LIME)
      1 "")]
    [(equal? (random n) 2)
     (make-fruit
      "Peach" VIOLET
      (create-position c PEACH)
      1 "")]
    [(equal? (random n) 3)
     (make-fruit
      "Blueberry" CORNFLOWERBLUE
      (create-position c BERRY)
      1 "")]
    [(equal? (random n) 4)
     (make-fruit
      "Mango" GOLD
      (create-position c MANGO)
      1 "")]))

; Currency Fruit -> Posn
; conusmes a currency c and an item, and outputs a position depending on
; the positions of the other elements

(check-expect (create-position CURRENCY0 BLOOD)
              (make-currency '() '()
                             (list BLOOD)
                             SPRING ECONOMY0))

(check-expect (create-position
               (make-currency '() '() (list BLOOD)
                              SPRING ECONOMY0) MANGO)
              (make-currency '() '()
                             (list (make-fruit "Mango" GOLD
                                               (make-posn SIZE SIZE) 0 "") BLOOD)
                             SPRING ECONOMY0))

(define (fn-create-position c item)
  (cond
    [(gem? item)
     (cond
       [else
        (if (empty? (currency-jewel c))
            (make-posn ... ...)
            (make-posn
             (... (posn-x (gem-position (first (currency-jewel c))))
                  ...)
             (... (posn-y (gem-position (first (currency-jewel c))))
                  ...)))])]
    [(fruit? item)
     (cond
       [else
        (if (empty? (currency-output c))
            (make-posn ... ...)
            (make-posn
             (... (posn-x (fruit-position (first (currency-output c))))
                  ...)
             (... (posn-y (fruit-position (first (currency-output c))))
                  ...)))])]))

(define (create-position c item)
  (cond
    [(gem? item)
     (cond
       [else
        (if (empty? (currency-jewel c))
            (make-posn SIZE SIZE)
            (make-posn
             (+ (posn-x (gem-position (first (currency-jewel c))))
                SIZE)
             (+ (posn-y (gem-position (first (currency-jewel c))))
                SIZE)))])]
    [(fruit? item)
     (cond
       [else
        (if (empty? (currency-output c))
            (make-posn SIZE SIZE)
            (make-posn
             (+ (posn-x (fruit-position (first (currency-output c))))
                SIZE)
             (+ (posn-y (fruit-position (first (currency-output c))))
                SIZE)))])]))
    

; Currency -> Boolean
; consumes a currency c, checks and updates the season if the umber of inputs
; (cycles) has been reached

(check-expect (change-season?
               (make-currency (list O2 O2 O2 O2 O2 O2 O2 O2 O2 O2)
                              '() '() WINTER ECONOMY0))
              #true)

(check-expect (change-season?
               (make-currency '() '() '() WINTER ECONOMY0))
              #false)

(define (fn-change-season? c)
  (cond
    [(empty? (currency-season c)) ...]
    [(zero? (modulo (length (currency-input c)) ...)) ...]
    [else ...]))

(define (change-season? c)
  (cond
    [(empty? (currency-season c)) #false]
    [(zero? (modulo (length (currency-input c)) CYCLE-PER-SEASON)) #true]
    [else #false]))

; Currency -> Currency
; consumes a currency c, outputs new currency with updated season field.

(check-expect (set-season
               (make-currency '() '() '() SPRING ECONOMY0))
              SUMMER)

(check-expect (set-season
               (make-currency '() '() '() WINTER ECONOMY0))
              SPRING)

(define (fn-set-season c)
  (cond
    [(equal? "Spring" (season-quarter (currency-season c)))
     ...]
    [(equal? "Summer" (season-quarter (currency-season c)))
     ...]
    [(equal? "Autumn" (season-quarter (currency-season c)))
     ...]
    [(equal? "Winter" (season-quarter (currency-season c)))
     ...]))

(define (set-season c)
  (cond
    [(equal? "Spring" (season-quarter (currency-season c)))
     SUMMER]
    [(equal? "Summer" (season-quarter (currency-season c)))
     AUTUMN]
    [(equal? "Autumn" (season-quarter (currency-season c)))
     WINTER]
    [(equal? "Winter" (season-quarter (currency-season c)))
     SPRING]))

; Currency -> Currency
; consumes a currency c, returns a new economy with updated fields

(check-expect (update-economy CURRENCY0) CURRENCY0)

(check-expect (update-economy
               (make-currency (list RUBY)
                              (list INPUT1)
                              (list MANGO BLOOD) WINTER
                              ECONOMY0))
              (make-currency (list RUBY)
                             (list INPUT1)
                             (list MANGO BLOOD) WINTER
                             (make-economy 1 7 2 0)))

(define (fn-update-economy c)
  (... (currency-jewel c)
       (currency-input c)
       (currency-output c)
       (currency-season c)
       (... (... (currency-input c))
            (... (currency-jewel c))
            (... (currency-output c))
            ...)))
    
(define (update-economy c)
  (make-currency (currency-jewel c)
                 (currency-input c)
                 (currency-output c)
                 (currency-season c)
                 (make-economy (length (currency-input c))
                               (length (currency-jewel c))
                               (length (currency-output c))
                               0)))

; Gem -> Boolean
; consumes a gem, and returns true if gem's position exceeds
; the screen boundary

(check-expect (boundary? (make-gem TOMATO (make-posn 10 40) 0 "")) #false)

(check-expect (boundary? (make-gem TOMATO (make-posn 390 40) 0 "")) #false)

(check-expect (boundary? (make-gem TOMATO (make-posn 10 390) 0 "")) #false)

(check-expect (boundary? (make-gem TOMATO (make-posn 390 390) 0 "")) #false)

(check-expect (boundary? (make-gem TOMATO (make-posn 10 30) 0 "")) #true)

(check-expect (boundary? (make-gem TOMATO (make-posn 400 40) 0 "")) #true)

(check-expect (boundary? (make-gem TOMATO (make-posn 10 395) 0 "")) #true)

(check-expect (boundary? (make-gem TOMATO (make-posn 391 391) 0 "")) #true)

(define (fn-boundary? gem)
  (cond
    [(< (posn-x (gem-position gem)) ...) ...]
    [(> (posn-x (gem-position gem)) (... SCENE-SIZE ...)) ...]
    [(< (posn-y (gem-position gem)) ...) ...]
    [(> (posn-y (gem-position gem)) (... SCENE-SIZE ...)) ...]
    [else ...]))


(define (boundary? gem)
  (cond
    [(< (posn-x (gem-position gem)) 10) #true]
    [(> (posn-x (gem-position gem)) (- SCENE-SIZE 10)) #true]
    [(< (posn-y (gem-position gem)) 40) #true]
    [(> (posn-y (gem-position gem)) (- SCENE-SIZE 10)) #true]
    [else #false]))

; Currency -> Currency
; launches the program from some initial state c

(define (cursim rate)
  (big-bang CURRENCY0
    [on-tick tock rate]
    [to-draw render]
    ;    [on-mouse click]
    ;    [stop-when last-world-? last-picture]
    [state #t]
    ;    [close-on-stop 3]
    [name "Cursim"]
    ))

; usage
;(cursim 1)
