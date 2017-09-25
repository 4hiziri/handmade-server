(ql:quickload '(:ningle :clack :lack :fast-http :flexi-streams :cl-fad :split-sequence))

(defvar *app* (make-instance 'ningle:<app>))
(defvar *port* 30018)
(defvar *download-dir* "~/Downloads/")

(use-package :split-sequence)

(defun split-seq-by-seq (seq1 seq2)
  "(split-seq-by-seq '(1 2) '(1 2 0 3 1 2 4 5 7 1 2)) => ((1 2) (0 3 1 2) (4 5 7 1 2) NIL)"
  (labels ((inner-loop (seq2 acc)
	     (let ((pos (search seq1 seq2)))
	       (if pos
		   (inner-loop (subseq seq2 (+ pos (length seq1)))
			       (cons (subseq seq2 0 (+ pos (length seq1))) acc))
		   (reverse (cons seq2 acc))))))
    (inner-loop seq2 nil)))

(defun split-field-multipart (content)
  (flet ((inner-convert (pair)
	   (cons (map 'string #'code-char (first pair))
		 (second pair))))
    (let* ((fields (split-seq-by-seq '(#xd #xa) content))
	   (disposition (nth 1 fields))
	   (type (nth 2 fields))
	   (content (apply #'append (mapcar (lambda (x) (coerce x 'list))
					    (subseq fields 4 (- (length fields) 2))))))
      (list
       (inner-convert (split-sequence (char-code #\:) disposition))
       (inner-convert (split-sequence (char-code #\:) type))
       (cons "content" (subseq content 0 (- (length content) 2)))))))

(defun split-disposition (disposition)  
  (let* ((disposition (map 'string #'code-char disposition))
	 (fields (split-sequence #\; disposition)))
    (mapcar (lambda (x)
	      (let ((list (split-sequence #\= x)))
		(cons (remove #\space (first list))
		      (remove-if (lambda (x) (or (char= x (code-char #xa))
						 (char= x (code-char #xd))
						 (char= x #\")
						 (char= x #\')))
				 (second list)))))
	    fields)))

(defun parse-multipart (content)
  (let* ((alist (split-field-multipart content))
	 (filename (cdr (assoc "filename"
			       (split-disposition (cdr (assoc "Content-Disposition" alist :test #'string=)))
			       :test #'string=)))
	 (content (cdr (assoc "content" alist :test #'string=))))
    (cons filename content)))

(defun download-from-stream (filename stream &optional (directory *download-dir*))  
  (if (or (fad:directory-exists-p directory)
	  (nth-value 1 (ensure-directories-exist directory)))
      (with-open-file (file (fad:merge-pathnames-as-file directory filename)
			    :direction :output
			    :element-type 'unsigned-byte
			    :if-exists :rename)
	(do ((byte (read-byte stream nil nil) (read-byte stream nil nil)))
	    ((null byte) t)
	  (write-byte byte file)))
      nil))

(defun download-from-list (filename list &optional (directory *download-dir*))  
  (if (or (fad:directory-exists-p directory)
	  (nth-value 1 (ensure-directories-exist directory)))
      (with-open-file (file (fad:merge-pathnames-as-file directory filename)
			    :direction :output
			    :element-type 'unsigned-byte
			    :if-exists :rename)
	(do ((byte (pop list) (pop list)))
	    ((null byte) t)
	  (write-byte byte file)))
      nil))

(setf (ningle:route *app* "/")
      "<p>welcome</p>
<p>menu</p>
<a href='/send'>send</a>")

(setf (ningle:route *app* "/send")
      "<form method='post' action='/send/upload' enctype='multipart/form-data'>
<input type='file' name='uploadfile' size='1024'><br>
<input type='submit' value='send'>
</form>")

(defun ningle-parse (params)
  (let ((stream (second (car params)))
	(field (third (car params))))
    (and stream
	 (download-from-stream (gethash "filename" field)
			       (flex:make-flexi-stream (second (car params)))))))

(defun hack-parse (content)
  (let ((alist (parse-multipart content)))
    (download-from-list (first alist) (coerce (rest alist) 'list))))

(setf (ningle:route *app* "/send/upload" :method :post)
      #'(lambda (params)
	  (let ((*download-dir* "~/misc/project/handmade-server/download/"))
	    (if (or (ningle-parse params)
		    (hack-parse (lack.request:request-content ningle:*request*)))
		"Success!"
		"Fail!"))))

(clack:clackup *app* :port *port*)
