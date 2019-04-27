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
(define SCREEN-SIZE (* WIDTH WIDTH))
(define MT (emty-scene SCENE-SIZE SCENE-SIZE))
(define MAX SCENE-SIZE)
(define ANGLE 90)

; graphical constants
(define (GEM c) (rhombus SIZE ANGLE "solid" c))

; structures
(define-struct currency [jewel input output quarter system]) 
(define-struct gem [name position colour sound])
(define-srtuct nutrient [type position sound])
(define-struct climate [type positon sound])
(define-struct fruit [name position sound])
(define-struct season (quarter delta sound))
(define-struct economy (supply demand gdp inflation))

; Interpretation

; a Currency is a structure
; (make-currency Jewel Input Output Season Economy)
; currency is the state of the program. It is represented by a tree.
; Each field represents an interaction with the system. 

(define CURRENCY0 (make-currency '() '() '() '() '()))

; a Gem is a structure
; (make-gem String Posn String String)
; a gem is a unit of currency. Inputs are exchanged into
; gems that when combined are exchanged again into outputs

; a Nutrient is a structure
; (make-nutrient String Posn String)
; a nutrient represents an external input into the tree

; a Climate is a structure
; (make-climate String Posn String)
; a climate is an external input into the tree

; a Fruit is a structure
; (make-fruit String Posn String)
; a fruit is an internal output of the tree

; a Season is a structure
; (make-season String Number String)
; a season is the current quarter and represents the dynamic force
; acting upon the inputs and otputs of the system

; an Economy is a structure
; (make-economoy Number Number Number Number)
; an economy represents the empiric value of the system donated in gems

; a Jewel is one of:
; - '()
; (cons gem jewel)
; a Jewel is a list of gems

; an Input is one of:
; - '()
; - (cons nutrient input)
; - (cons climate input)
; - (cons season input)
; an input is a list of external inputs that create currency (gems)
; by the tree

; an output is one of:
; - '()
; - (cons fruit output)
; - (cons climate output)
; - (cons nutrient output)
; an oupput is a list of internal exports that go back into the system and
; exhaust currency (gems)

; a Quarter is one of:
; - '()
; - (cons season quarter)
; a quarter is a either Winter, Spring, Summer, and Autumn.
; each season affects the inputs and outputs of the system by a value, delta

; a system is one of:
; - '()
; - (cons supply (cons demand (cons gdp (cons inflation system))))
; a system is a list of numbers representing its economic value 

; functions

; Currency -> Currency
; consumes a currency c and updates it each tick
(define (tock c) c)

(check-expect (tock CURRENCY0) CURRENCY0)

(check-expect (tock (make-currency
                     (list (make-gem

; Currency -> Currency
; launches the program from some initial state c

(define (cursim-main rate)
  (big-bang (make-currency )
    [on-tick tock rate]
    [to-draw render]
    [on-mouse click]
    ;[stop-when last-world-? last-picture]
    [state #t]
    ;[close-on-stop 3]
    [name "Cursim"]
    ))

; usage
;(cursim 1)