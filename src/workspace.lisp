;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cljtalk)

;;; example

(defparameter in-wav (make-world-wav-from-file "/home/wiz/cl-libworld/lib/World/test/vaiueo2d.wav"))
(defparameter in-wav (make-world-wav-from-file "/home/wiz/Sound/dragon/mono-22k/001.wav"))
(defparameter in-wav (make-world-wav-from-file "/home/wiz/Sound/flowers-voice-database/test.wav"))
(defparameter params (analysis in-wav :f0-floor 71d0))
(defparameter filter-bank (make-mel-filter-bank (world-wav-fs in-wav) (fft-size params) 50))
(defparameter filter-result (apply-mel-filter-bank filter-bank (world-params-spectrogram params)))
(defparameter dct-result
  (make-array (array-dimensions filter-result) :element-type 'double-float :initial-element 0d0))
(defparameter dct-recover-result
  (make-array (array-dimensions filter-result) :element-type 'double-float :initial-element 0d0))
(dct filter-result dct-result)
(fill-up-zero dct-result 13) ; 39
(idct dct-result dct-recover-result)

(clgp:plot-lists (list (coerce (world-wav-data in-wav) 'list)
                       (mapcar #'- (coerce (world-wav-data out-wav) 'list)))
                 :title-list '("Original" "Recovered")
                 :output "/home/wiz/Dropbox/docs/lispmeetup45/input-output-wave.png")

(clgp:plot-list )
(clgp:plot-list (coerce (world-params-f0 params) 'list) :y-label "F0" :x-label "Frames")
(clgp:splot-matrix (world-params-spectrogram params) :y-label "FFT" :x-label "Frames")
(clgp:splot-matrix (world-params-aperiodicity params) :y-label "FFT" :x-label "Frames")
(clgp:splot-matrix (mel-filter-bank-filter-bank filter-bank) :y-label "FFT/2" :x-label "Mel Filter Bank Channels")
(clgp:splot-matrix filter-result :y-label "Mel Filter Bank Channels" :x-label "Frames")
(clgp:splot-matrix dct-result :y-label "Mel Filter Bank Channels" :x-label "Frames")
(clgp:splot-matrix dct-recover-result :y-label "Mel Filter Bank Channels" :x-label "Frames")
(clgp:splot-matrix (world-params-aperiodicity params))

(defparameter dct-result-ap
  (make-array (array-dimensions (world-params-aperiodicity params))
              :element-type 'double-float :initial-element 0d0))
(defparameter dct-recover-result-ap
  (make-array (array-dimensions (world-params-aperiodicity params))
              :element-type 'double-float :initial-element 0d0))

(dct (world-params-aperiodicity params) dct-result-ap)
(fill-up-zero dct-result-ap 5)
(idct dct-result-ap dct-recover-result-ap)

(clgp:splot-matrix (normalize-aperiodicity dct-recover-result-ap)
                   :y-label "FFT" :x-label "Frames" :output "/home/wiz/Dropbox/docs/lispmeetup45/aperiodicity-recover.png")

;; filter-resultを元のスペクトグラムに復元する

(recover-spectrogram! filter-bank dct-recover-result (world-params-spectrogram params))
(setf (world-params-aperiodicity params) (normalize-aperiodicity dct-recover-result-ap))

(defparameter out-wav (synthesis params))
(output-world-wav-to-file out-wav "/home/wiz/tmp/vaiueo2d-13.wav")

(clgp:splot-matrix (world-params-aperiodicity params))
(aref (world-params-aperiodicity params) 30 0)
(aref linear-ap 50 0)

(clgp:plot-list
 (loop for i from 0 to (1- (array-dimension (world-params-aperiodicity params) 1)) collect
   (aref (world-params-aperiodicity params) 60 i)))

(clgp:plot-list
 (loop for i from 0 to (1- (array-dimension (world-params-aperiodicity params) 0)) collect
   (aref (world-params-aperiodicity params) i 512)))

(defparameter linear-ap
  (make-array (array-dimensions (world-params-aperiodicity params))
              :element-type 'double-float :initial-element 0.001d0))

(length (loop for y from 0 to 1 by (/ 1d0 (array-dimension (world-params-aperiodicity params) 1))
              collect y))

(loop for frame from 22 to 137 do
  (loop for i from 0 to (1- (array-dimension (world-params-aperiodicity params) 1))
        for y from 0d0 to 1d0 by (/ 1d0 (array-dimension (world-params-aperiodicity params) 1))
        do
     (setf (aref linear-ap frame i) (+ y 0.001d0))))
    
(clgp:splot-matrix linear-ap)
(clgp:splot-matrix (world-params-aperiodicity params))


;;; 音素割り当て

(clgp:splot-matrix filter-result :y-label "Mel Filter Bank Channels" :x-label "Frames")

(loop for frame in segment do
  (loop for i from 0 to (1- (array-dimension filter-result 1)) do
    (setf (aref filter-result frame i)
          -8d0)))
