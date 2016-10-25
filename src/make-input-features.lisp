;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cljtalk)

(defun segment-list (lab)
  (mapcar
   (lambda (x)
     (floor
      (/ (cadr x)
         (/ (world-params-frame-period params) 1000))))
   lab))

(defun duration-list (lab)
  (mapcar (lambda (x)
            (- (caddr x) (cadr x)))
          lab))

(defun make-feature-list (lab params)
  (let ((len (length (world-params-f0 params)))
        (phoneme-id (mapcar #'car lab))
        (segment (segment-list lab))
        (duration (duration-list lab)))
    (setf (nth (1- (length segment)) segment) (1- len))
    (nlet iter ((i 0)
                (phoneme-id phoneme-id)
                (segment segment)
                (duration duration)
                (product nil))
      (if (= (length segment) 1)
        (nreverse product)
        (let ((feature-vec (make-array 40 :element-type 'double-float :initial-element 0d0)))
          (setf (aref feature-vec (car phoneme-id)) 1d0
                (aref feature-vec 38) (* (car duration) 1d0)
                (aref feature-vec 39) (* (if (zerop (car segment))
                                           i
                                           (- i (car segment) 1))
                                         1d0))
          (if (and (<= (car segment) i)
                   (< i (cadr segment)))
            (iter (1+ i) phoneme-id segment duration (cons feature-vec product))
            (iter (1+ i) (cdr phoneme-id) (cdr segment) (cdr duration) (cons feature-vec product))
            ))))))
