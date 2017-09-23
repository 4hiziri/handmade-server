(ql:quickload '(:ningle :clack :lack :fast-http :split-sequence :flexi-streams :cl-fad))

(defvar *app* (make-instance 'ningle:<app>))
(defvar *port* 30018)
(defvar *download-dir* "~/Downloads/")

(defun parse-multipart/form-data (data)
  (let ((data (split-sequence #\; data))
	(alist nil))
    (push (cons :content-disposition (second (split-sequence #\: (pop data)))) alist)
    (push (cons :name (second (split-sequence #\= (pop data)))) alist)
    (push (cons :filename data) alist)
    alist))

(defun download-file (filename stream &optional (directory *download-dir*))  
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

(setf (ningle:route *app* "/")
      "<p>welcome</p>
<p>menu</p>
<a href='/send'>send</a>")

(setf (ningle:route *app* "/send")
      "<form method='post' action='/send/upload' enctype='multipart/form-data'>
<input type='file' name='uploadfile' size='1024'><br>
<input type='submit' value='send'>
</form>")

(setf (ningle:route *app* "/send/upload" :method :post)
      #'(lambda (params)
	  (let ((stream (second (car params)))
		(field (third (car params))))
	    (if (and stream (download-file (gethash "filename" field)
					   (flex:make-flexi-stream (second (car params)))
					   "/home/tkgsy/misc/project/handmade-server/src/download/"))
		"Success!"
		"Fail!"))))

(clack:clackup *app* :port *port*)
