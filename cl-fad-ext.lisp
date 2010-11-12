;;; some lisp to simplify handling directories on heirarchical filesystems (eg POSIX systems)

(defpackage #:cl-fad-ext
  (:use #:cl #:cl-fad)
  (:export #:absolute-pathname-p
	   #:relative-pathname-p
	   #:normalize-path
	   #:subdirectory
	   #:subdirectory-p
	   #:parent-directory
	   #:parent-directory-p))
	   
(in-package #:cl-fad-ext)

(defun absolute-pathname-p (path)
  "Returns true if path is an absolute path"
  (when (eql (first (pathname-directory path)) :absolute)
    path))

(defun relative-pathname-p (path)
  "Returns true if path is relative absolute path"
  (when (eql (first (pathname-directory path)) :relative)
    path))

(defun normalize-directory (directory)
  "Removes .. and . from the directory path."
  ;;TODO fix it so it works with symbolic links
  (labels ((filter-directory (directory)
	     (cond ((null directory) nil)
		   ((equal (first directory) ".")
		    (filter-directory (rest directory)))
		   ((member (first directory) '(:back :up "..") :test #'equal)
		    (rest (filter-directory (rest directory))))
		   (t (cons (first directory) 
			    (filter-directory (rest directory)))))))
    (cons (first directory) 
	  (nreverse (filter-directory
		     (reverse (rest directory)))))))
  

(defun normalize-path (path)
  "Returns a pathname equivalent to 'path' but without '..' or '.'
   For example: /foo/./bar/../baz/ becomes /foo/baz/"
  (make-pathname :defaults path
		 :directory (normalize-directory (pathname-directory path))))

(defun subdirectory (path1 path2)
  "Concatenates path1 and path2, returning a path which is a subdirectory of path1"
  (when (eql (first (pathname-directory path2)) :absolute)
    (warn "~A is an absolute pathname" path2))
  (unless (directory-pathname-p path1)
    (warn "~A is not a directory" path1))
  (make-pathname :defaults path1 
		 :directory (normalize-directory (append (pathname-directory
							  (pathname-as-directory path1))
							 (rest (pathname-directory path2))))
		 :name (pathname-name path2)
		 :type (pathname-type path2)))

(defun absolute-pathname (path &optional (default-pathname *default-pathname-defaults*))
  "Converts path to an absolute pathname by concatenating it to default-pathname."
  (if (relative-pathname-p path)
      (subdirectory default-pathname path)
      path))

(defun subdirectory-p (path1 path2 &optional (default-pathname *default-pathname-defaults*))
  "Returns true if path2 is a subdirectory of path1"
  (unless (directory-pathname-p path1)
    (warn "~A is not a directory" path1))
  (let ((dir1 (pathname-directory (absolute-pathname (pathname-as-directory path1) default-pathname)))
	(dir2 (pathname-directory (absolute-pathname path2 default-pathname))))
    (and (every #'equal dir1 dir2)
	 (< (length dir1) (length dir2)))))

(defun parent-directory (path &optional (number 1))
  "Returns the parent directory of path."
  (let ((path-dir (pathname-directory path)))
    (make-pathname :defaults path
		   :directory (if (> (list-length path-dir) number)
				  (butlast path-dir number)
				  (progn 
				    (warn "~A already at Root: there is no parent directory" path)
				    path-dir)))))

(defun parent-directory-p (path1 path2)
  "Returns true if path2 is a parent directory of path1"
  (subdirectory-p path2 path1))

;;; --- for manipulating the current directory

(defpackage #:change-directory
  (:use #:cl #:cl-fad #:cl-fad-ext)
  (:export #:cwd
	   #:cd))
	   
(in-package #:change-directory)

(defvar *cwd* *default-pathname-defaults*
  "Holds the current working directory.")

(defvar *sync-default-pathname-defaults* t
  "Specifies whether *default-pathname-defaults* will be synced to the current working directory.
   Default is true.")

(defvar *directory-history* nil
  "Contains a list of working directories previously visited.")

(defvar *directory-future* nil
  "Contains a list working directories visited and left while going back in the history.")

(defun cwd ()
  "Returns the current working directory."
  *cwd*)

(defun set-cwd (newpath)
  "Primary setter for the current working directory.
   Keeps it normalized and syncs *default-pathname-defaults* to the
   current working directory when said behavior is enabled."
  (setf *cwd* (normalize-path (pathname-as-directory (pathname newpath))))
  (when *sync-default-pathname-defaults*
    (setf *default-pathname-defaults* *cwd*))
  *cwd*)
    
(defsetf cwd () (newpath)
  `(set-cwd ,newpath))

(defun updir (&optional (number 1))
  "Changes to the parent of the current working directory."
  (push (cwd) *directory-history*)
  (setf (cwd) (parent-directory (cwd) number))))

(defun dir-history (&optional (number 1))
  "Moves forward and backward through the directory history."
  (cond ((> number 0)
	 (dotimes (n number)
	   (let ((dir (pop *directory-history*)))
	     (push (cwd) *directory-future*)
	     (setf (cwd) dir))))
	((< number 0)
	 (dotimes (n (- number))
	   (let ((dir (pop *directory-future*)))
	     (push (cwd) *directory-history*)
	     (setf (cwd) dir)))))
  (cwd))

(defun cd (&rest newpath)
  "Manipulates the current working directory by applying the instructions
   in newpath sequentially. Pathnames and pathstrings change the cwd to 
   the paths specified. Relative pathnames and pathstrings are interpreted
   relative to the current cwd. Keyword :up, moves up the directory tree and
   keywords :back and :forward move back and forth through the movement
   history respectively. All keyword arguments and be followed with a number
   to specify the number of times it should be applied."
  (let ((path (pop newpath)))
    (typecase path 
      (list (apply #'cd path))
      ((or string pathname)
       (push (cwd) *directory-history*)
       (setf (cwd)
	     (if (absolute-pathname-p (pathname path))
		 path
		 (subdirectory (cwd) path))))
      (keyword (let ((modifier (if (numberp (first newpath))
				   (pop newpath)
				   1)))
		 (case path
		   (:back (dir-history modifier))
		   (:up (updir modifier))
		   (:forward (dir-history (- modifier)))))))
    (if newpath
	(cd newpath)
	(cwd))))


