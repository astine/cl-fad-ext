;;; some lisp to simplify handling directories on heirarchical filesystems (eg POSIX systems)

(defpackage #:cl-fad-ext
  (:use #:cl #:cl-fad)
  (:export #:absolute-pathname-p
	   #:relative-pathname-p
	   #:normalize-path
	   #:subdirectory
	   #:subdirectory-p
	   #:parent-directory
	   #:parent-directory-p
	   ;;for interactive filesystem usage
	   #:cd
	   #:updir))
	   
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
  (cond ((or (equal (first directory) :relative)
	     (equal (first directory) :absolute))
	 (cons (first directory) (normalize-directory (rest directory))))
	((null directory) nil)
	((equal (first directory) ".")
	 (normalize-directory (rest directory)))
	((and (member (second directory) '(:back :up "..") :test #'equal)
	      (not (member (second directory) '(:back :up "..") :test #'equal)))
	 (normalize-directory (nthcdr 2 directory)))
	(t (cons (first directory)
		 (normalize-directory (rest directory))))))

(defun normalize-path (path)
  "Removes .. and . from the directory path."
  (make-pathname :defaults path
		 :directory (normalize-directory (pathname-directory path))))

(defun subdirectory (path1 path2)
  "Concatenates path1 and path2, returning a path which is a subdirectectory of path1"
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
  (make-pathname :defaults path
		 :directory (butlast (pathname-directory path) number)))

(defun parent-directory-p (path1 path2)
  "Returns true if path2 is a parent directory of path1"
  (subdirectory-p path2 path1))

;;; --- for manipulating the filesystem

(defun cd (newpath)
  "Changes the current working directory."
  (let ((path (pathname-as-directory (pathname newpath))))
    (setf *default-pathname-defaults*
	  (if (absolute-pathname-p path)
	      path
	      (subdirectory *default-pathname-defaults*
			    path)))))

(defun updir (&optional (number 1))
  "Changes to the parent of the current working directory."
  (cd (parent-directory *default-pathname-defaults* number)))
