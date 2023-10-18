(defpackage :flow
  (:use :cl)
  (:export :-> :->> :-<> :-<>>))

(in-package :flow)

(defun <>p (expr)
  (and (symbolp expr)
       (string= expr "<>")))

(defun ><p (expr)
  (and (symbolp expr)
       (string= expr "><")))

(defmacro -> (expr &rest exprs)
  "insert first"
  (if (null exprs)
      expr
      (loop
        with result = expr
        for each in exprs do
          (if (listp each)
              (setq result (list* (car each) result (cdr each)))
              (setq result (list each result)))
        finally (return result)
        )))

(defmacro ->> (expr &rest exprs)
  "insert last"
  (if (null exprs)
      expr
      (loop
        with result = expr
        for each in exprs do
          (if (listp each)
              (setq result (append each (list result)))
              (setq result (list each result)))
        finally (return result)))
  )

(defun replace-<> (args acc)
  (let ((replaced-p nil)
        (result nil))
    (loop
      for each in args do
        (if (<>p each)
            (progn (push acc result)
                   (setq replaced-p t))
            (push each result)))
    (values (nreverse result) replaced-p))
  )

(defun replace-<>-or-insert-last (args acc)
  (multiple-value-bind (newargs replaced-p) (replace-<> args acc)
    (if replaced-p
        newargs
        (append newargs (list acc))
        ))
  )

(defun replace-<>-or-insert-first (args acc)
  (multiple-value-bind (newargs replaced-p) (replace-<> args acc)
    (if replaced-p
        newargs
        (append (list acc) newargs)
        ))
  )

(defmacro -<> (expr &rest exprs)
  "insert first if no <>"
  (if (null exprs)
      expr
      (loop
        with result = expr
        for each in exprs do
          (if (listp each)
              (setq result (list* (car each) (replace-<>-or-insert-first (cdr each) result)))
              (setq result (list each result)))
        finally (return result)))
  )

(defmacro -<>> (expr &rest exprs)
  "insert last if no <>"
  (if (null exprs)
      expr
      (loop
        with result = expr
        for each in exprs do
          (if (listp each)
              (setq result (list* (car each) (replace-<>-or-insert-last (cdr each) result)))
              (setq result (list each result)))
        finally (return result))))

(defmacro <<- (expr &rest exprs)
  `(->> ,@(reverse exprs) ,expr)
  )
