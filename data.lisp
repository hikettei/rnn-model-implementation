
(in-package :translate-model)

(defparameter *data-path* "./kftt-data-1.0/data/")
(defparameter *file-begins-with* "/kyoto-")

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

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
		  (:devtest "devtest")
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

(defun collect-tokens (data-type language &optional (w2i (make-hash-table :test 'equal)) (i2w (make-hash-table :test 'equal)))
  (let ((token-count (1+ (hash-table-count i2w))))
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

(defmacro register-word (w2i i2w word)
  (with-gensyms (s)
    `(let ((,s (hash-table-count ,w2i)))
       (setf (gethash ,word ,w2i) (1+ ,s))
       (setf (gethash (1+ ,s) ,i2w) ,word)
       
       ; Assure oneness
       (unless (= (hash-table-count ,w2i)
		  (hash-table-count ,i2w))
	 (error "incorrect word count")))))

(defmacro n2onehot (dict n)
  (with-gensyms (v)
    `(let ((,v (make-array (list (hash-table-count ,dict) 1) :initial-element 0.0 :element-type 'single-float)))
       (setf (aref ,v (if ,n ,n (gethash "<UNK>" w2i)) 0) 1.0)
       ,v)))

(defmacro onehot2n (onehot)
  `(position 1 ,onehot))

(defmacro word2vector (dict sentence max-length)
  (with-gensyms (translated-vector tokenized i)
    `(let ((,translated-vector (make-array ,max-length))
	   (,tokenized (split " " ,sentence)))
       (dotimes (,i (length ,tokenized))
	 (setf (aref ,translated-vector ,i)
	       (n2onehot ,dict (gethash (nth ,i ,tokenized) ,dict))))
       (setf (aref ,translated-vector (length ,tokenized))
	     (n2onehot ,dict (gethash "<EOS>" ,dict)))
       ,translated-vector)))

(defmacro vector2word (dict vector)
  (with-gensyms (translated-word i)
    `(let ((,translated-word (make-array (length ,vector))))
       (dotimes (,i (length ,vector))
	 (setf (aref ,translated-word ,i)
	       (onehot2n (gethash (nth ,i ,vector) ,dict))))
       ,translated-word)))

(defmacro with-open-kftt-file (line type data-type language &body body)
  (with-gensyms (buffer)
    `(with-open-file (,buffer (kftt-data-path ,type ,data-type ,language) :external-format :utf8)
       (loop for ,line = (handler-case (read-line ,buffer nil nil)
			   (error (_) (declare (ignore _)) nil))
	     while ,line
	     do ,@body))))

(defmacro translate-into-vector (target data-type lang dict max-length)
  (with-gensyms (line i)
    `(let ((,i 0))
      (with-open-kftt-file ,line :tok ,data-type ,lang
				  (setf (aref ,target ,i)
					(word2vector ,dict ,line ,max-length))
				  (incf ,i 1)))))
				   
(defun init-train-datas (data-type lang1 lang2 w2i data-size)
  (let* ((max-length (max (calc-max-length :tok data-type lang1)
			  (calc-max-length :tok data-type lang2)))
	 (train-x (make-array data-size))
	 (train-y (make-array data-size)))
    
    (translate-into-vector train-x data-type lang1 w2i max-length)
    (translate-into-vector train-y data-type lang2 w2i max-length)
    (values train-x train-y)))


(defun calc-max-length (type data-type lang)
  (let ((max-size 0))
    (with-open-kftt-file line type data-type lang
      (setq max-size (max max-size (length (split " " line)))))
    ; In the end of sequence, we allocate the place for <EOS>
    (1+ max-size)))

(defun calc-data-size (type data-type lang)
  (let ((data-size 0))
    (with-open-kftt-file _ type data-type lang
      (incf data-size 1))
    data-size))
