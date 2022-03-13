
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

	(register-word w2i i2w "<EOS>")
	(register-word w2i i2w "<UNK>")
	
	(multiple-value-bind (train-x train-y)
	    (init-train-datas data-type lang1 lang2 w2i data-size)

	  (values train-x train-y w2i i2w data-size max-length))))))

(defun train (data-type lang1 lang2)
  (multiple-value-bind (train-x train-y w2i i2w data-size max-length) (prepare-train-param data-type lang1 lang2)
    
    ))

(defun make-matrix (rows cols)
  (make-array (list rows cols) :initial-element 0.0 :element-type 'single-float))

(defun mmul (ma mb)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array single-float (* *)) ma mb))
  (let ((rows (array-dimension ma 0))
        (cols (array-dimension mb 1)))
    (declare (type fixnum rows cols))
    (let ((result (make-matrix rows cols)))
      (declare (type (simple-array single-float (* *)) result))
      (dotimes (row rows)
        (dotimes (col cols)
          (dotimes (k cols)
            (incf (aref result row col)
                  (* (aref ma row k) (aref mb k col))))))
      result)))

(defstruct LSTM

  )

(defstruct RNN hidden weights bias embedding)

(defun rnn-layer (layer-size max-length)
  (make-RNN :hidden    (make-matrix layer-size max-length)
	    :weights   (make-matrix layer-size max-length)
	    :bias      (make-matrix max-length 1)
	    :embedding (make-matrix layer-size max-length)))

(defun rnn-forward (rnn l time x)
  (with-slots (hidden weights bias embedding) rnn

    ))

(defun encoder-embedding-layer (input-size x y)

  )
