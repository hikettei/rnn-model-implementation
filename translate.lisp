
(defpackage :translate-model
  (:use :cl))

(in-package :translate-model)


(defclass gaussiandb () ((mean :initform nil
			       :initarg :mean
			       :accessor gaussiandb-mean)
			 (var :initform nil
			      :initarg :var
			      :accessor gaussiandb-var)))

(defun double-random ()
  (let ((i (random 1.0)))
    (if (eq i 0.0)
	(setq i (double-random))) i))

(defmethod gaussiandb-random ((gs gaussiandb))
  (let* ((r (double-random))
	 (c (sqrt (* -2 (log r)))))
    (if (< (double-random) 0.5)
	(+    (* c
	      (sin (* 2.0 pi (double-random)))
	      (gaussiandb-var gs))
	      (gaussiandb-mean gs))
	(+    (* c
	      (cos (* 2.0 pi (double-random)))
	      (gaussiandb-var gs))
	      (gaussiandb-mean gs)))))

(defun make-gtrain-data (x y g1 g2)
  (let ((data (make-array `(,x ,y) :initial-element 0.0))
	(rgns (list g1 g2)))
    (dotimes (c x)
      (dotimes (layerN y)
	(setf (aref data c layerN)
	      (/ (gaussiandb-random (nth (mod layerN 2) rgns)) (* 2 pi)))))
    data))

(defun make-ss (x y)
  (let ((g1 (make-instance 'gaussiandb :mean -2.0 :var 1.0))
	(g2 (make-instance 'gaussiandb :mean  2.0 :var 1.0)))
    (make-gtrain-data x y g1 g2)))


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
    (let ((embedding-rnn (rnn-layer 4 max-length)))
      (rnn-t-times embedding-rnn train-x)
      ))))

(defun make-matrix (rows cols)
  (make-array (list rows cols) :initial-element 0.0 :element-type 'single-float))

(defun matrix-concat (X Y)
  (make-array '(1 2) :initial-contents `((,X ,Y))))

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

(defun m+ (&rest args)
  (let ((result (make-array (array-dimensions (car args)) :initial-element 0.0)))
    (let ((rows (array-dimension result 0))
	  (cols (array-dimension result 1)))
      (dolist (m args)
	(dotimes (row rows)
	  (dotimes (col cols)
	    (incf (aref result row col) (aref m row col))))))
    result))

(defun mtanh (matrix)
  (let ((result (make-array (array-dimensions matrix) :initial-element 0.0)))
    (let ((rows (array-dimension result 0))
	  (cols (array-dimension result 1)))
      (dotimes (row rows)
	(dotimes (col cols)
	  (incf (aref result row col)
		(tanh (aref matrix row col))))))
    result))

(defstruct LSTM

  )

(defstruct RNN hidden weights bias embedding max-length)

(defun init-layer (layer-size max-length)
  (loop for i below layer-size
	collect (make-matrix 1 max-length)))

(defun init-layers (layer-size max-length &optional (nd 1))
  (loop for i below layer-size
	collect (loop for ii below nd
		      collect (make-matrix 1 max-length))))

(defun rnn-layer (layer-size max-length)
  (make-RNN :hidden    (init-layers layer-size max-length max-length)
	    :weights   (init-layer layer-size max-length)
	    :bias      (init-layer 1 max-length)
	    :embedding (make-matrix layer-size max-length)
	    :max-length max-length))

; 層が変わるときにパラメーターを更新する
(defun rnn-t-times (rnn input-x)
  (with-slots (hidden weights bias embedding max-length) rnn
    (setf (nth 0 weights) (make-ss max-length 1))
    (loop for i from 1 to (1- (length hidden))
	  do (loop for n from 0 to (1- (length input-x))
		   do (progn
			(setf (nth 0 (nth i hidden)) 1)
			(loop for time from 1 to (1- (array-dimension (aref input-x n) 0))
			      do (rnn-forward rnn i time (aref input-x n))))))
    rnn))

(defun rnn-forward (rnn l time x)
  (with-slots (hidden weights bias embedding max-length) rnn
    (let* ((embedding-vector (mmul embedding (aref x (1- time))))
	   (at (m+ (mmul (nth time (nth l hidden)) (nth (1- l) weights))
		   (mmul embedding-vector
			 (nth l weights))
		   (car bias)))
	   (ht (mtanh at)))
      (setf (nth time (nth l hidden)) ht))))

(defun encoder-embedding-layer (input-size x y)

  )
