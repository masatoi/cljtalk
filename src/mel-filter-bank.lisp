;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cljtalk)

(defstruct (mel-filter-bank (:constructor %make-mel-filter-bank))
  filter-bank
  index-centers)

(defun hz2mel (f)
  (* 1127.01048d0
     (log (+ (/ f 700.0d0) 1.0d0))))

(defun mel2hz (m)
  (* 700.0d0
     (- (exp (/ m 1127.01048d0)) 1.0d0)))

(defun make-spline (x-list y-list)
  (let* ((h (diff-list x-list))
         (dy (diff-list y-list))
         (v (nlet iter ((h h) (dy dy) (product nil))
              (if (null (cdr h))
                (nreverse product)
                (iter (cdr h)
                      (cdr dy)
                      (cons (* 6 (- (/ (cadr dy) (cadr h))
                                    (/ (car dy) (car h))))
                            product)))))
         (mat-size (- (length x-list) 2))
         (vvec (make-array (list mat-size 1) :element-type 'double-float))
         (mat (make-array (list mat-size mat-size) :element-type 'double-float :initial-element 0d0)))
    (loop for j from 0 to (1- mat-size)
          for hj-1 in h
          for hj in (cdr h)
          for vj in v
          do
       (setf (aref vvec j 0) vj) ; init vvec
       (cond ((= j 0)            ; init mat
              (setf (aref mat 0 0) (* 2 (+ hj-1 hj))
                    (aref mat 0 1) hj))
             ((= j (1- mat-size))
              (setf (aref mat (1- mat-size) (- mat-size 2)) hj-1
                    (aref mat (1- mat-size) (1- mat-size)) (* 2 (+ hj-1 hj))))
             (t (setf (aref mat j (1- j)) hj-1
                      (aref mat j j) (* 2 (+ hj-1 hj))
                      (aref mat j (1+ j)) hj))))
    (let* ((mat-1 (m-1 mat))
           (uvec (m* mat-1 vvec))
           (u (append (cons 0d0 (loop for i from 0 to (1- mat-size) collect (aref uvec i 0)))
                      '(0d0)))
           (b (mapcar (^ (uj) (/ uj 2)) u))
           (a (mapcar (^ (uj+1 bj hj) (/ (- uj+1 (* 2 bj)) (* 6 hj))) (cdr u) b h))
           (c (mapcar (^ (hj yj+1 aj bj dj)
                        (/ (- yj+1 (* aj hj hj hj) (* bj hj hj) dj) hj))
                      h (cdr y-list) a b y-list)))
      (^ (x)
        (let* ((pos0 (position-if (^ (xj+1) (<= x xj+1)) x-list)) ; x-list must be sorted
               (pos (cond ((null pos0) (- (length x-list) 2))
                          ((<= pos0 0) 0)
                          (t (1- pos0))))
               (xj (nth pos x-list))
               (yj (nth pos y-list))
               (aj (nth pos a))
               (bj (nth pos b))
               (cj (nth pos c)))
          (+ (* aj (expt (- x xj) 3))
             (* bj (expt (- x xj) 2))
             (* cj (- x xj))
             yj))))))

(defun make-mel-filter-bank (fs fft-size num-channels)
  (let* ((fmax (/ fs 2))        ; Nyquist frequency (Hz)
         (melmax (hz2mel fmax)) ; Nyquist frequency (mel)
         (nmax (floor (/ fft-size 2)))
         (df (/ fs fft-size))   ; Frequency per fft-index
         (dmel (/ melmax (1+ num-channels)))
         (melcenters (loop for i from 1 to num-channels collect (* i dmel)))
         (fcenters (mapcar #'mel2hz melcenters))
         (indexcenter (mapcar (^ (fc) (round (/ fc df))) fcenters))
         (indexstart (cons 0 (subseq indexcenter 0 (1- (length indexcenter)))))
         (indexstop (append (cdr indexcenter) (list nmax)))
         (filter-bank (make-array (list num-channels nmax)
                                  :element-type 'double-float :initial-element 0d0)))
    (loop for c from 0 to (1- num-channels)
          for icenter in indexcenter
          for istart in indexstart
          for istop in indexstop
          do
       (let ((increment (/ 1.0d0 (- icenter istart)))
             (decrement (/ 1.0d0 (- istop icenter))))
         (loop for i from istart to (1- icenter) do
           (setf (aref filter-bank c i) (* (- i istart) increment)))
         (loop for i from icenter to (1- istop) do
           (setf (aref filter-bank c i) (- 1.0d0 (* (- i icenter) decrement))))))
    (%make-mel-filter-bank :filter-bank filter-bank :index-centers indexcenter)))

(defun apply-mel-filter-bank (mel-filter-bank spectrogram)
  (let ((mat (make-matrix (array-dimension spectrogram 0)
                          (floor (/ (array-dimension spectrogram 1) 2)))))
    (loop for i from 0 to (1- (array-dimension mat 0)) do
      (loop for j from 0 to (1- (array-dimension mat 1)) do
        (setf (aref mat i j) (aref spectrogram i j))))
    (mapmat #L(log $1 10)
            (m* mat
                (m-t (mel-filter-bank-filter-bank mel-filter-bank))))))

;; TODO: 高周波数に行くにつれて倍率が上がっているので6で割っているが、対数を取った方がいい？
(defun recover-spectrogram! (filter-bank filter-result spectrogram)
  (loop for frame from 0 to (1- (array-dimension spectrogram 0)) do
    (let* ((fft-size/2 (array-dimension (mel-filter-bank-filter-bank filter-bank) 1))
           (x-list (append (mapcar (lambda (x) (* x 1d0)) (mel-filter-bank-index-centers filter-bank)) (list fft-size/2)))
           (y-list (append (loop for j from 0 to (1- (array-dimension filter-result 1))
                                 collect (expt 10 (aref filter-result frame j)))
                           (list 0d0)))
           (spline-func (make-spline x-list y-list)))
      (loop for i from 0 to (1- fft-size/2) do
        (let ((interpolated (funcall spline-func i)))
          (setf (aref spectrogram frame i)
                (if (<= interpolated 0) ; assure that power is non-zero positive number
                  0.0000001d0
                  (/ interpolated 6.0d0))))))))

(defun normalize-aperiodicity (ap)
  (mapmat #L(cond ((>= $1 1d0) 1d0)
                  ((<= $1 0d0) 0.001d0) ; must be >0
                  (t $1))
          ap))
