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
(define-struct currency [jewel input output season])
(define-struct gem [name position colour sound])
(define-srtuct nutrient [type position sound])
(define-struct climate [type positon sound])
(define-struct fruit [name position sound])
(define-struct season (quarter delta sound))

; Interpretation

; a Currency is a structure
; (make-currency Jewel Input Output Season)
; currency is the state of the program. Each field represents an object
; in the dynamic system

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