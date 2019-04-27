;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cursim) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; my entry to Ludum Dare 44

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

; Currency -> Currency
; launches the program from some initial state c

(define (cursim-main rate)
  (big-bang (make-currency )
    [on-tick tock rate]
    [to-draw render]
    [on-mouse click]
    ;[stop-when last-world-connected? last-picture]
    [state #t]
    ;[close-on-stop 3]
    [name "Cursim"]
    ))

; usage
;(cursim 1)