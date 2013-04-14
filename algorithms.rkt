#lang racket

(require "algorithms/brute-force.rkt"
         "algorithms/c-style/sad.rkt")

(provide c-style
         (rename-out [find-pattern-in-source brute-force]
                     [find-pattern-in-source/tolerance brute-force/tolerance]))
