
(in-package :rnn-model)

(defparameter *data-path* "./kftt-data-1.0/data/")
(defparameter *file-begins-with* "/kyoto-")

(defmacro kftt-data-path (type data-type language)
  `(concatenate 'string
		*data-path*
		(case ,type
		  (:orig "orig")
		  (:tok "tok")
		  (T (error "Invaild keyform")))
		*file-begins-with*
		(case ,data-type
		  (:dev "dev")
		  (:test "test")
		  (:train "train")
		  (:tune "tune")
		  (T (error "Invaild keyform")))
		"."
		(case ,language
		  (:en "en")
		  (:ja "ja")
		  (T (error "Invaild keyform")))))

(defun split (x str)
  (let ((pos (search x str))
        (size (length x)))
    (if pos
      (cons (subseq str 0 pos)
            (split x (subseq str (+ pos size))))
      (list str))))

(defun collect-tokens (data-type language)
  (let ((w2i (make-hash-table))
	(i2w (make-hash-table))
	(token-count 0))
    (with-open-file (f (kftt-data-path :tok data-type language) :external-format :utf8)
      (loop for line = (handler-case (read-line f nil nil)
			 (error (_) (declare (ignore _)) nil))
	    while line
	    do (dolist (l (split " " line))
		 (unless (gethash l w2i)
		   (progn (setf (gethash l w2i) token-count)
			  (setf (gethash token-count i2w) l)
			  (incf token-count 1))))))
    (values w2i i2w)))

