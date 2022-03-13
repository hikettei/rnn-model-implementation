
(defpackage :translate-model
  (:use :cl))

(in-package :translate-model)

(defmacro mvalues (&rest args)
  `(multiple-value-call #'values ,@args))

(defun prepare-train-param (data-type lang1 lang2)
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


	  (values train-x train-y w2i i2w data-size))))))

(defun train (data-type lang1 lang2)
  (multiple-value-bind (train-x train-y w2i i2w data-size) (prepare-train-param data-type lang1 lang2)
    ))