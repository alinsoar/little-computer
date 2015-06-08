#lang racket ; -*- mode:racket ; buffer-read-only:t -*-

;;; A fast scheduler used by the simulator of electrical signal flow.

;;; Copyright (C) 2015 Alin C Soare

;;; This file is part of the Little Computer.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define propagation/combinational/max/delay 100)

(define FILO
  (let ((SEGSIZE (expt 2 12))
        (SEGALLOCATED 10)
        (SEGCOUNT (expt 2 14))
        (SEGMINCOUNT 100)
        (ep 'nil)
        (vp 'nil)
        (eseg 'nil)
        (vseg 'nil))
    
    (define SEGMENTS (make-vector SEGCOUNT))
    (define SSET!
      (lambda (SEG OFF x)
        (vector-set! (vector-ref SEGMENTS SEG) OFF x)))
    (define SGET
      (lambda (SEG OFF)
        (let ((v (vector-ref (vector-ref SEGMENTS SEG) OFF)))
          ;; clean the action after it is executed.
          (vector-set! (vector-ref SEGMENTS SEG) OFF 'nil)
          (grow-e)
          v)))
    
    (define reset
      (lambda ()
        (set! ep 0)
        (set! vp -1)
        (set! vseg 0)
        (set! eseg 0)))
    
    (define initialize
      (lambda ()
        (build-vector SEGALLOCATED new-segment)))
    
    (define new-segment
      (lambda (idx)
        (vector-set! SEGMENTS idx (make-vector SEGSIZE))))

    (define move-segments
      (lambda ()
        (and (> eseg SEGMINCOUNT)
             (> eseg (/ SEGALLOCATED 4))
             (begin
               (for-each (lambda (idx)
                           (vector-copy! (vector-ref SEGMENTS (- idx eseg))
                                         0
                                         (vector-ref SEGMENTS idx)))
                         (range eseg (add1 vseg)))
               (set! vseg (- vseg eseg))
               (set! eseg 0)))))
    
    (define purge-segments
      (lambda ()
        (and (> SEGALLOCATED SEGMINCOUNT)
             (< vseg (/ SEGALLOCATED 2))
             (begin
               (set! SEGALLOCATED (add1 vseg))
               (for-each (lambda (i) (vector-set! SEGMENTS i 'nil))
                         (range (add1 SEGALLOCATED) SEGCOUNT))))))

    (define grow-e
      (lambda ()
        (cond ((< ep (sub1 SEGSIZE))
               (set! ep (add1 ep)))
              (else
               (set! ep 0)
               (set! eseg (add1 eseg))))))
    
    (define grow-v
      (lambda ()
        (move-segments)
        (purge-segments)
        (cond ((< vp (sub1 SEGSIZE))
               (set! vp (add1 vp)))
              (else
               (set! vp 0)
               (set! vseg (add1 vseg))
               (and (= vseg SEGALLOCATED)
                    (begin (new-segment SEGALLOCATED)
                           (set! SEGALLOCATED (add1 SEGALLOCATED))))))))
    
    (define end?
      (lambda ()
        (and (> ep vp) (= eseg vseg))))
    
    
    
    (lambda (?)
      (case ?
        ('ADD! (lambda (a) (grow-v) (SSET! vseg vp a)))
        ('DEL!  (lambda () (SGET eseg ep)))
        ('INIT!  (lambda () (reset) (initialize)))
        ('RESET  reset)
        ('END?  end?)
        (else (error 'FILO))))))
(define ++ (FILO 'ADD!))
(define -- (FILO 'DEL!))
(define init (FILO 'INIT!))
(define reset (FILO 'RESET))
(define end? (FILO 'END?))

(define SCHEDULER
;;;  Scheduler is a device that puts order in the evaluation of the
;;;  propagators and simulates the time.
;;; 
;;;  Propagators are a model of the electrical signal flow.
;;;
;;;  How it works:
;;;
;;;  1. PROPAGATOR installs an action on input cells A and B.
;;;
;;;  2. When cell A or B changes its value, it executes its actions.
;;;     The actions installed by some propagator in the cell will send
;;;     tasks to the scheduler after execution.  Not any action sends
;;;     tasks to the scheduler.  For example, an action like a probe
;;;     (useful for debugging) will not add anything to the scheduler,
;;;     so it will not propagate a signal.
;;;
;;;  3. The scheduler executes its tasks inserted by cells in FILO
;;;     order, as this is how the electrical signal propagates in
;;;     time.
;;;
;;;  4. An action has the form (PROPAGATION-DELAY . THUNK).  The
;;;     propagation delay is the minimal delay required for the
;;;     electrical signal to stabilize its value in a hardware
;;;     structure of the given circuit.  If the propagation time cross
;;;     over the limit PROPAGATION/COMBINATIONAL/MAX/DELAY, it means
;;;     the combinational logic defined by HDL equations has a loop
;;;     and we signal an error.
;;;
;;;
;;;                                    o-------o
;;;  +--------------+                 /         \
;;; -+              |  A             / action    o
;;;  |  propagator  +--o-------+    / on cell A  |                   __
;;; -+              |  |       |   /             |                  /
;;;  +--------------+  |       |  o              |                 /
;;;                    |       |        +--------+-----+          /
;;;                    |       |        |              |         /   C
;;;                    |       +-------->  THE NAND    |  out   |    L
;;;                    |                |              +------->|    O
;;;                    |       +-------->  PROPAGATOR  |   ^    |    U
;;;   +--------------+ |       |        |              |   |     \   D
;;;  -+              | |   B   |        +--------+-----+   |      \
;;;  -+  propagator  +-|---o---+                 |         |       \
;;;  -+              | |   |     o               |         |        \__
;;;   +--------------+ |   |      \              |         |
;;;                    |   |       \ action      |         |
;;;                    |   |        \ on cell B  o         |
;;;                    |   |         \          /          |
;;;                    |   |          o--------o           |
;;;                    |   |                               |
;;;                    |   |    +------------------+       |
;;;                    +-> +--> |    SCHEDULER     |-->----+
;;;                             +------------------+
;;;
  (let ((ERROR (let ((status false))
                 (lambda (?)
                   (case ?
                     ('? status)
                     ('SET! (set! status 'ad-infinitum))
                     (else (error "sch/status"))))))
        (last-propagation-delay 0))
    
    ;; ..............................
    (define new/action
      (lambda (d initial-propagation-time a)
        (cons (+ d initial-propagation-time)
              (lambda ()
                (if (pair? a)
                    ;; all the actions of a wire were scheduled.
                    (for-each (lambda (a d)
                                (execute/action
                                 a
                                 (+ d initial-propagation-time)))
                              (car a)
                              (cdr a))
                    ;; a single action was scheduled.
                    (a))
                (cond ((end?)
                       (set! last-propagation-delay 0)
                       (reset)
                       'done)
                      ((> last-propagation-delay
                          propagation/combinational/max/delay)
                       (ERROR 'SET!)
                       'done)
                      (else
                       (>>>)))))))
    
    (define execute/action
      (lambda (a init/delay)
        ;; before to run an action `A`, we need to update the
        ;; `last-propagation-delay`.  The updated
        ;; `last-propagation-delay` will be the initial delay for all
        ;; actions scheduled by the execution of `A`.
        (set! last-propagation-delay init/delay)
        (a)))
    
    (define NEW
      (lambda (T action READER-LIST TODO-LIST DELAY-LIST)
        "schedule a thunk"
        (if (eq? 'WIRE action)
            (begin
              (for-each (lambda (r) (r)) READER-LIST)
              (++ (new/action 0
                              last-propagation-delay
                              (cons TODO-LIST DELAY-LIST))))
            (++ (new/action T last-propagation-delay action)))))
    (define >>>
      (lambda ()
        "propagate the signal"
        (let ((a (--)))
          (execute/action (cdr a) (car a)))))
    ;; ..............................
    (init)
    (lambda (p)
      "pemissions"
      (p NEW
         >>>
         (lambda () (ERROR '?))))))
(define SCHED+
  (lambda (d propagator/a M N P)
    (SCHEDULER (lambda (add _ __) (add d propagator/a M N P)))))
(define SCHED>>>
  (lambda ()
    (SCHEDULER (lambda (_ >>> __) (>>>)))))
(define SCHED?
  (lambda ()
    (SCHEDULER (lambda (_ __ s) (s)))))

(provide SCHED+
         SCHED>>>
         SCHED?)
