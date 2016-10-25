;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cljtalk)

;;; make dataset

(defparameter *lab-dir* "/home/wiz/Sound/dragon/lab/")
(defparameter *wav-dir* "/home/wiz/Sound/dragon/mono-22k/")
(defparameter *n-frames* 20)
(defparameter *dct-dim* 13)
(defparameter *dct-ap-dim* 5)
(defparameter *n-filter-bank* 50)


(defparameter lab (read-lab-file (car (ls *lab-dir*))))
;; (defparameter lab (read-lab-file "/home/wiz/Sound/dragon/lab/vaiueo2d.lab"))
(defparameter wav (make-world-wav-from-file (format-pathname (car (ls *wav-dir*)))))
(defparameter params (analysis wav))

(defun concat-vec (vec-list)
  (reduce (lambda (x y)
            (concatenate 'vector x y))
          vec-list))

(defun make-input-list (lab params &optional (n-frames *n-frames*))
  (nlet iter ((feature-list (make-feature-list lab params))
              (product nil))
    (if (< (length feature-list) n-frames)
      (nreverse product)
      (iter (nthcdr n-frames feature-list)
            (cons (concat-vec (nthcar n-frames feature-list))
                  product)))))

(defun make-output-spec-list (params filter-bank &optional (dct-dim *dct-dim*) (n-frames *n-frames*))
  (let* ((filter-result (apply-mel-filter-bank filter-bank (world-params-spectrogram params)))
         (dct-result (make-array (array-dimensions filter-result)
                                 :element-type 'double-float :initial-element 0d0)))
    (dct filter-result dct-result)
    
    (loop for s from 0 to (1- (floor (length (world-params-f0 params)) n-frames)) collect
      (let ((v (make-array (* dct-dim n-frames) :element-type 'single-float :initial-element 0.0)))
        (loop for frame from 0 to (1- n-frames) do
          (loop for i from 0 to (1- dct-dim) do
            (setf (aref v (+ (* dct-dim frame) i))
                  (coerce (aref dct-result (+ (* s n-frames) frame) i)
                          'single-float))))
        v))))

(defun make-output-ap-list (params &optional (dct-dim *dct-ap-dim*) (n-frames *n-frames*))
  (let* ((dct-result (make-array (array-dimensions (world-params-spectrogram params))
                                 :element-type 'double-float :initial-element 0d0)))
    (dct (world-params-aperiodicity params) dct-result)
    
    (loop for s from 0 to (1- (floor (length (world-params-f0 params)) n-frames)) collect
      (let ((v (make-array (* dct-dim n-frames) :element-type 'single-float :initial-element 0.0)))
        (loop for frame from 0 to (1- n-frames) do
          (loop for i from 0 to (1- dct-dim) do
            (setf (aref v (+ (* dct-dim frame) i))
                  (coerce (aref dct-result (+ (* s n-frames) frame) i)
                          'single-float))))
        v))))

(defun make-output-f0-list (params &optional (n-frames *n-frames*))
  (loop for s from 0 to (1- (floor (length (world-params-f0 params)) n-frames)) collect
    (let ((v (make-array n-frames :element-type 'single-float :initial-element 0.0)))
      (loop for frame from 0 to (1- n-frames) do
        (setf (aref v frame)
              (coerce (aref (world-params-f0 params) (+ (* s n-frames) frame))
                      'single-float)))
      v)))


(defun load-dataset ()
  (let* ((dataset (loop for wav-path in (ls *wav-dir*)
                        for lab-path in (ls *lab-dir*)
                        collect
                     (let* ((wav (make-world-wav-from-file (format-pathname wav-path)))
                            (lab (read-lab-file lab-path))
                            (params (analysis wav))
                            (filter-bank (make-mel-filter-bank (world-wav-fs wav) (fft-size params) *n-filter-bank*)))
                       (format t "loading: ~A~%" wav-path)
                       (mapcar (lambda (input output-spec output-ap output-f0)
                                 (make-regression-datum
                                  :id 1
                                  :target (array-to-mat (concatenate 'vector output-spec output-ap output-f0))
                                  :array  (array-to-mat input)))
                               (make-input-list lab params)
                               (make-output-spec-list params filter-bank)
                               (make-output-ap-list params)
                               (make-output-f0-list params)))))
         (flat-dataset (flatten dataset))
         (data-array (make-array (length flat-dataset))))
    (loop for i from 0 to (1- (length flat-dataset))
          for data in flat-dataset
          do
       (setf (aref data-array i) data)
       (setf (regression-datum-id (aref data-array i)) i))
    data-array))

;; ;; 5855 data
;; (defparameter dataset (load-dataset))

;; ;;; Normalize dataset

;; (defparameter dataset-normal (copy-regression-dataset dataset))

;; (regression-dataset-normalize! dataset-normal :noise-degree 0.0)

;;; Model Definition

(defparameter fnn
  (build-fnn (:class 'regression-fnn :max-n-stripes 100)
    ;; Input Layer
    (inputs (->input :size 800))
    (f1-activations (->activation inputs :name 'f1 :size 512))
    (f1 (->relu f1-activations))
    (f2-activations (->activation f1 :name 'f2 :size 512))
    (f2 (->relu f2-activations))
    (f3-activations (->activation f2 :name 'f3 :size 512))
    (f3 (->relu f3-activations))
    (prediction-activations (->activation f3 :name 'prediction :size 380))
    ;; Output Lump: ->squared-difference
    (prediction (->loss (->squared-difference (activations-output prediction-activations)
                                              (->input :name 'targets :size 380))
                        :name 'prediction))))

(defparameter fnn-relu-dropout-regression
  (build-fnn (:class 'regression-fnn :max-n-stripes 100)
    (inputs (->input :size 800 :dropout 0.2))
    (f1-activations (->activation inputs :name 'f1 :size 512))
    (f1* (->relu f1-activations))
    (f1 (->dropout f1*))
    (f2-activations (->activation f1 :name 'f2 :size 512))
    (f2* (->relu f2-activations))
    (f2 (->dropout f2*))
    (f3-activations (->activation f2 :name 'f3 :size 512))
    (f3* (->relu f3-activations))
    (f3 (->dropout f3*))
    (prediction-activations (->activation f3 :name 'prediction :size 380))
    (prediction (->loss (->squared-difference (activations-output prediction-activations)
                                              (->input :name 'targets :size 380))
                        :name 'prediction))))

;; (train-regression-fnn-process-with-monitor fnn dataset-normal :n-epochs 100)
;; (train-regression-fnn-process-with-monitor fnn dataset2 :n-epochs 100)
