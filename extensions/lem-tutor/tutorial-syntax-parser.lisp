(defpackage :lem-tutor/syntax-parser
  ;; "syntax parser package for the lem tutorial"
  (:use :cl :lem :ppcre)
  (:export :make-tutorial-parser
           :*tutorial-syntax-table*
           :scan-region))

(in-package :lem-tutor/syntax-parser)

(defclass tutorial-syntax-parser () ())

(defun make-tutorial-parser ()
  "Create a new tutorial syntax parser instance."
  (make-instance 'tutorial-syntax-parser))

(defmethod lem/buffer/internal::%syntax-scan-region ((parser tutorial-syntax-parser) start end)
  "Internal method to set up syntax highlighting for a buffer region."
  (with-point ((start start)
               (end end))
    (buffer-start start)
    (buffer-end end)
    (remove-text-property start end :attribute)
    (scan-region start end)))

(defun scan-code-block (point end)
  "Scan a lisp code block from the opening fence to the closing fence.
Point should be on the opening ```lisp line. Advances point past the
closing ``` line and returns point so scan-region can continue from there."
  (let* ((mode 'lem-lisp-mode:lisp-mode)
         (syntax-table (mode-syntax-table mode)))
    (line-offset point 1)
    (with-point ((start point))
      (loop :while (point< point end)
            :until (ppcre:scan "^```\\s*$" (line-string point))
            :while (line-offset point 1))
      (set-region-major-mode start point mode)
      (syntax-scan-region start point
                          :syntax-table syntax-table
                          :recursive-check nil)
      (line-offset point 1) ; step past closing ```
      point)))              ; return position to scan-region

(defun put-line-attribute (point attribute)
  "Apply a highlight attribute to the entire line at point."
  (with-point ((start point)
               (end point))
    (line-start start)
    (line-end end)
    (put-text-property start end :attribute attribute)))

(defun get-header-attribute (line)
  "Return the highlight attribute for a line based on its content.
Returns nil for lines that need no special highlighting."
  (cond
    ((ppcre:scan "^LESSON \\d+:" line)
     'tutorial-lesson-attribute)
    ((ppcre:scan "^[A-Z]+( [A-Z]+)*$" line)
     'tutorial-title-attribute)
    ((ppcre:scan "^>>" line)
     'tutorial-propmpt-attribute)
    ((ppcre:scan "^-----\\s*$" line)
     'tutorial-delimiter-attribute)
    ((ppcre:scan "^--------" line)
     'tutorial-separator-attribute)
    ((ppcre:scan "^\\s*C-|^\\s*M-|^\\s*Alt-x" line)
     'tutorial-keybinding-attribute)))

(defun scan-region (start end)
  "Scan and apply syntax highlighting to the tutorial buffer region."
  (clear-region-major-mode start end)
  (with-point ((point start))
    (loop :while (point< point end)
          :do (let ((line (line-string point)))
                (cond
                  ((ppcre:scan "^```lisp" line)
                   (let ((block-end (scan-code-block (copy-point point) end)))
                     (move-point point block-end)))
                  (t
                   (put-line-attribute point (get-header-attribute line))  
                   (ppcre:do-scans (start-line end-line reg-s reg-e "(\\s*(?:C-|M-)[A-Za-z]Alt-x \\s* (?:\\s+(?:C-|M-|Alt-)[A-Za-z])*)(\\s*[a-z]+(?:-[a-z]*)*)(\\s+[A-z]*)+$" line)
                     (put-text-property
                      (character-offset (copy-point point)(aref reg-s 0))
                      (character-offset (copy-point point)(aref reg-e 0))
                      :attribute 'tutorial-keybinding-attribute)
                     (put-text-property
                      (character-offset (copy-point point)(aref reg-s 1))
                      (character-offset (copy-point point)(aref reg-e 1))
                      :attribute 'tutorial-command-attribute)
                     (put-text-property
                      (character-offset (copy-point point)(aref reg-e 1))
                      (character-offset (copy-point point)end-line)
                      :attribute 'tutorial-description-attribute))
                   (ppcre:do-matches (m-start m-end "C-|M-|Alt-x\\s+[[a-z]]+(-[[a-z]]*)+" line)
                     (put-text-property
                      (character-offset (copy-point point) m-start)
                      (character-offset (copy-point point) m-end)
                      :attribute 'tutorial-keybinding-attribute)))))
          :do (unless (line-offset point 1)
                (return)))))

(defparameter *tutorial-syntax-table*
  (let ((table (make-syntax-table)))
    (set-syntax-parser table (make-tutorial-parser))
    table))
