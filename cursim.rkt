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
                                 (make-season "" 0 "")
                                 (make-economy 0 0 0 0)))

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

(check-expect (render CURRENCY0) MT)

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
    "left" "top"
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
               "left" "top"
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
   "left" "top"
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

(define (fn-tock c) c)

(define (tock c)
  (make-currency
   (cond
     [else (if (gem-threshold? (currency-input c) (currency-season c))
               (generate-gem c)
               (currency-jewel c))])
   (generate-input c)
   (cond
     [else (if (fruit-threshold? (currency-jewel c))
               (generate-fruit c)
               (currency-output c))])
   (cond (else (if (change-season? c)
                   (set-season c)
                   (currency-season c))))
   (update-economy c)))
    


; Currency -> Boolean
; consumes input in, and season s, returns true if the required number of inputs have been
; reached to make a new gem
(define (gem-threshold? in s) #false)

; Currency -> Currency
; consumes a currency c, outputs a randomly
; generated gem and adds it to jewel
(define (generate-gem c) c)

; Currency -> Currency
; consumes a currency c, randomly generates an input, adds it to the list,
; and returns input
(define (generate-input c) c)

; Currency -> Currency
; consumes a jewel j, returns true if the the number and type of gems
; to generate a new fruit has been reached
(define (fruit-threshold? j) #false)

; Currency -> Currency
; consumes a currency c, generates a new fruit dependent on jewel composition,
; adds it to the list and returns output
(define (generate-fruit c) c)

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
(define (update-economy c) c)

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
