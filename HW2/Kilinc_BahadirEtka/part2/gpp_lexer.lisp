;; Bahadir Etka Kilinc 1901042701 hw2_part2
(defconstant *output-filepath* "parsed_lisp.txt")
(defun is-inc-delimeter (char)
 (let ((code (char-code char)))
  (cond
    ((= code 40) 40)
    ((= code 41) 41)
    ((= code 34) 34))))
    
(defun is-ws (char)
 (let ((code (char-code char)))
  (if (= code 32)
    t (and (>= code 9) (<= code 13)))))


(defun read-file (&optional (filepath *input-filepath*))
 (let ((tokens nil))
   (with-open-file (stream filepath)
     (loop for line = (read-line stream nil nil)
       while line
       do (setf tokens
           (concatenate 'list tokens (tokens-list line)))))
  tokens))


(defun tokens-list (str)
 (let ((list nil) (j 0) (delimeter nil))
  (setf str (concatenate 'string str (format nil " ")))
  
  (loop for i below (length str)
   for chr = (char str i)
   do (cond
       ((is-ws chr)
        (progn
          (if (not (= i j)) (push (subseq str j i) list))
          (setf j (1+ i))))
       ((setf delimeter (is-inc-delimeter chr))
        (progn
          (if (not (= i j)) (push (subseq str j i) list))
          (push (format nil "~A" (code-char delimeter)) list)
          (setf j (1+ i)))))
    do(if (not (null list))
       (if (string= (format nil ";;") (elt list 0))
        (return))))
  (nreverse list)))


(defun is-numeric (str)
 (loop for c across str
  if (or (> (char-code c) 57) (< (char-code c) 48))
   do (return-from is-numeric nil))
 t)


(defun is-alpha (str)
 (loop for c across str
  do (if
       (and
         (or (< (char-code c) 65) (> (char-code c) 90))
         (or (< (char-code c) 97) (> (char-code c) 122)))
       (return-from is-alpha nil)))
 t)

(defun is-identifier (str)
 (if (zerop (length str)) (return-from is-identifier nil)
   (if (is-alpha (subseq str 0 1))
    (loop for c across (subseq str 1 (length str))
     do(if
        (not (or (is-alpha (format nil "~A" c))
              (is-numeric (format nil "~A" c))))
        (return-from is-identifier nil)))
    (return-from is-identifier nil)))
 t)


(defun is-plus-or-minus (str)
 (if (or
      (string= str (format nil "+"))
      (string= str (format nil "-"))) t nil))


(defun is-init-zero (str)
 (if (and (not (zerop (length str)))
      (string= (subseq str 0 1) (format nil "0")))
  t))

(defun is-value (str)
 (let ((size (length str)))
  (cond
    ((zerop size) nil)
    ((= size 1) (is-numeric str))
    (t (if (and (not (is-init-zero str)) (is-numeric str))
        t)))))


(defun lexical-analyze (tlist)
 (let ((list nil) (q nil))
  (loop for token in tlist
   do(if
      (string= token (format nil "\""))
      (if (null q)
          (progn
            (push (format nil "(\"\"\" \"OP_OC\")") list)
            (setf q t))
          (progn
            (push (format nil "(\"\"\" \"OP_CP\")") list)
            (setf q nil)))
      (push (match-tokens token) list)))
  (nreverse list)))

(defun match-tokens (str)

 (cond                                      
    ((string= (format nil "and") str)     (format nil "(\"and\" \"KW_AND\")"))
    ((string= (format nil "or") str)      (format nil "(\"or\" \"KW_OR\")"))
    ((string= (format nil "not") str)     (format nil "(\"not\" \"KW_NOT\")"))
    ((string= (format nil "equal") str)   (format nil "(\"equal\" \"KW_EQUAL\")")) 
    ((string= (format nil "less") str)    (format nil "(\"less\" \"KW_LESS\")")) 
    ((string= (format nil "nil") str)     (format nil "(\"nil\" \"KW_NIL\")")) 
    ((string= (format nil "list") str)    (format nil "(\"list\" \"KW_LIST\")")) 
    ((string= (format nil "append") str)  (format nil "(\"append\" \"KW_APPEND\")")) 
    ((string= (format nil "concat") str)  (format nil "(\"concat\" \"KW_CONCAT\")")) 
    ((string= (format nil "set") str)     (format nil "(\"set\" \"KW_SET\")")) 
    ((string= (format nil "deffun") str)  (format nil "(\"deffun\" \"KW_DEFFUN\")")) 
    ((string= (format nil "for") str)     (format nil "(\"for\" \"KW_FOR\")")) 
    ((string= (format nil "if") str)      (format nil "(\"if\" \"KW_IF\")")) 
    ((string= (format nil "exit") str)    (format nil "(\"exit\" \"KW_EXIT\")")) 
    ((string= (format nil "load") str)    (format nil "(\"load\" \"KW_LOAD\")")) 
    ((string= (format nil "disp") str)    (format nil "(\"disp\" \"KW_DISP\")")) 
    ((string= (format nil "true") str)    (format nil "(\"true\" \"KW_TRUE\")")) 
    ((string= (format nil "false") str)   (format nil "(\"false\" \"KW_FALSE\")"))
    ((string= (format nil "+") str)       (format nil "(\"+\" \"OP_PLUS\")")) 
    ((string= (format nil "-") str)       (format nil "(\"-\" \"OP_MINUS\")")) 
    ((string= (format nil "/") str)       (format nil "(\"/\" \"OP_DIV\")")) 
    ((string= (format nil "*") str)       (format nil "(\"*\" \"OP_MULT\")")) 
    ((string= (format nil "(") str)       (format nil "(\"(\" \"OP_OP\")")) 
    ((string= (format nil ")") str)       (format nil "(\")\" \"OP_CP\")"))
    ((string= (format nil "**") str)      (format nil "(\"**\" \"OP_DBLMULT\")"))
    ((string= (format nil ",") str)       (format nil "(\",\" \"OP_COMMA\")")) 
    ((string= (format nil ";;") str)      (format nil "(\";;\" \"COMMENT\")")) 
    ((is-identifier str)                  (format nil "(\"~A\" \"IDENTIFIER\")" str)) 
    ((is-value str)                       (format nil "(\"~A\" \"VALUEI\")" str))

    (t (format nil "SYNTAX ERROR '~A' is an invalid token." str))))

(defun is-args ()
 (if *args* (car *args*)))


(defun gppinterpreter (is-args)
 (with-open-file (stream *output-filepath* :direction :output :if-exists :supersede)
  (if (not (null is-args))
  (progn
  (format stream "(")
   (loop for token in (lexical-analyze (read-file is-args))

    do(format stream "~A" token)
    )
    (format stream ")")
  )
   (progn
    (format t "-> ")
    (progn

    (loop for line = (read-line)
    
      while (not (string= (format nil "") line))
      do (progn
      (format t "(")
      (loop for token in
          (lexical-analyze (tokens-list line))
          do (format stream "~A" token)
              (format t "~A" token))
              (format t ")\n")
              (terpri)
      )))))))
(gppinterpreter (is-args))
