
(defpackage :rnn-model
  (:use :cl))

(in-package :rnn-model)

(defmacro mvalues (&rest args)
  `(multiple-value-call #'values ,@args))

(defun train (data-type lang1 lang2)
  (let ((max-length (max (calc-max-length :tok data-type :en)
			 (calc-max-length :tok data-type :ja)))
	(data-size (calc-data-size :tok data-type lang1)))
    (multiple-value-bind (lang1-w2i lang1-i2w) (collect-tokens data-type lang1)
      (multiple-value-bind (w2i i2w)
	  (collect-tokens data-type lang2 lang1-w2i lang1-i2w)
	
	(unless (= data-size (calc-data-size :tok data-type lang2))
	  (error "Incorresponding the number of data-size between lang1 and lang2"))

	(multiple-value-bind (train-x train-y)
	    (init-train-datas data-type lang1 lang2 w2i data-size)
        (aref train-y 0)
	)))))
