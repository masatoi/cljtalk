;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cljtalk)

;; load library
(load-foreign-library "/usr/lib/x86_64-linux-gnu/libfftw3.so.3")

;; define wrappers
(defcfun ("fftw_plan_r2r_1d" fftw-plan) :pointer ; fftw-plan object
  (n :int)
  (in  :pointer) ; double array
  (out :pointer) ; double array
  (kind :int)
  (flags :unsigned-int))

(defcfun ("fftw_execute_r2r" fftw-execute) :void ;; successive entries
  (plan :pointer)
  (in  :pointer)  ;; these will be complexes stored as 
  (out :pointer)) ;; successive entries

(defcfun ("fftw_destroy_plan" destroy-plan) :void
  (plan :pointer))

(defcfun ("fftw_malloc" fftw-malloc) :pointer
  (count :int))

(defcfun ("fftw_free" fftw-free) :void
  (location :pointer))

;; 

(defun dct (filter-result dct-result) ; update dct-result destructively
  (let* ((nframes (array-dimension filter-result 0))
         (len (array-dimension filter-result 1))
         (ni (sqrt (/ 1.0d0  (ash len 2)))))
    (with-foreign-objects ((in :double len) (out :double len))
      (loop for frame from 0 to (1- nframes) do
        ;; set input
        (loop for i from 0 to (1- len) do
          (setf (mem-aref in :double i) (* ni (aref filter-result frame i))))
        
        (let ((plan (fftw-plan len in out (if (evenp len) 5 9) 0)))
          (unwind-protect
              (progn
                (fftw-execute plan in out)
                (loop for i from 0 to (1- len) do
                  (setf (aref dct-result frame i) (mem-aref out :double i))))
            (destroy-plan plan)))))))

(defun idct (dct-result dct-recover-result) ; update dct-recover-result destructively
  (let* ((nframes (array-dimension dct-result 0))
         (len (array-dimension dct-result 1))
         (ni (sqrt (/ 1.0d0 len))))
    (with-foreign-objects ((in :double len) (out :double len))
      (loop for frame from 0 to (1- nframes) do
        ;; set input
        (loop for i from 0 to (1- len) do
          (setf (mem-aref in :double i) (* ni (aref dct-result frame i))))
        
        (let ((plan (fftw-plan len in out (if (evenp len) 4 8) 0)))
          (unwind-protect
              (progn
                (fftw-execute plan in out)
                (loop for i from 0 to (1- len) do
                  (setf (aref dct-recover-result frame i) (mem-aref out :double i))))
            (destroy-plan plan)))))))

(defun fill-up-zero (dct-result dim)
  (loop for frame from 0 to (1- (array-dimension dct-result 0)) do
    (loop for i from dim to (1- (array-dimension dct-result 1)) do
      (setf (aref dct-result frame i) 0d0))))
