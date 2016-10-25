;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cljtalk)

(defparameter phonemes
  #("a" "a:" "b" "by" "ch" "d" "e" "e:" "f" "g" "gy" "h" "hy" "i" "i:" "j" "k"
    "ky" "m" "n" "N" "ny" "o" "o:" "p" "q" "r" "ry" "s" "sh" "t" "ts" "u" "u:" "w" "y" "z"
    "sp"))

(defun read-line-lab-file (path)
  (with-open-file (f path)
    (nlet iter ((product nil))
      (let ((read-token (read-line f nil 'eof)))
        (if (eq read-token 'eof)
          (nreverse product)
          (iter (cons read-token product)))))))

;; return (position start end)
(defun read-token (line-string)
  (let ((str-list (ppcre:split " " line-string)))
    (list (position-if (lambda (x)
                         (if (or (string= (caddr str-list) "silB")
                                 (string= (caddr str-list) "silE"))
                           (string= "sp" x)
                           (string= (caddr str-list) x)))
                       phonemes)
          (parse-number:parse-positive-real-number (car str-list))
          (parse-number:parse-positive-real-number (cadr str-list)))))

(defun read-lab-file (path)
  (mapcar #'read-token (read-line-lab-file path)))

;; (read-all-lab-file "/home/wiz/src/segmentation-kit/wav/")

(defun read-all-lab-file (dir)
  (let* ((lab-files (remove-if-not (^ (p) (string= (pathname-type p) "lab")) (ls dir)))
         (phonome-lists (mapcar (^ (file)
                                  (remove-duplicates (mapcar #'car (read-lab-file file))
                                                     :test #'string=))
                                lab-files)))
    (reduce (^ (phonome-list1 phonome-list2)
              (union phonome-list1 phonome-list2 :test #'string=))
            phonome-lists)))

;; (read-all-lab-file "/home/wiz/src/segmentation-kit/wav/")
;; => ("i:" "z" "ch" "p" "a:" "ny" "gy" "hy" "by" "u:" "ky" "f" "ry" "j" "silB" "q"
;;     "ts" "y" "w" "d" "t" "sp" "u" "h" "g" "o" "e:" "o:" "sh" "k" "b" "n" "r" "i"
;;     "m" "a" "s" "e" "N" "silE")

;; 38 phoneme (without silB, silE)

;;; Plot F0 and phoneme segment

;; (defparameter in-wav (world:make-world-wav-from-file "/home/wiz/Sound/dragon/mono-22k/001.wav"))

;; (let* ((lab-list (cljtalk::read-lab-file "/home/wiz/src/segmentation-kit/wav/vaiueo2d.lab"))
;;        (n-frame (length (world-params-f0 params)))
;;        (frame-size (/ (caddr (wiz:last1 lab-list)) (1- n-frame)))
;;        (lab-x-list (alexandria:flatten
;;                     (mapcar (lambda (x)
;;                               (list (cadr x) (cadr x) (caddr x) (caddr x)))
;;                             lab-list)))
;;        (x-list (loop for x from 0.0 by frame-size
;;                      for i from 0 to (1- n-frame) collect
;;                      x)))
;;   (clgp:plot-lists
;;    (list (loop for elem across (world-params-f0 params) collect elem)
;;          (alexandria:flatten (loop repeat (length lab-list) collect '(0 160 160 0))))
;;    :x-lists (list x-list lab-x-list)
;;    :x-label "time[sec]"
;;    :title-list '("f0" "phoneme")
;;    :output "/home/wiz/tmp/vaiueo2d-f0-alignment.png")
;;   )
