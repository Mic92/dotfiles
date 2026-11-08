;;; Actions printed as Rust's Debug does, for golden files and logs.

(in-package #:va)

(defn camel ((s symbol)) string
  (remove #\- (string-capitalize (symbol-name s))))

(defn debug-value ((v t) (type t)) string
  "V of Coalton TYPE as Rust's Debug prints it."
  (cond ((and v (consp type) (eq (first type) 'coalton:Optional))
         (format nil "Some(~a)" (debug-value v (second type))))
        ((and (consp type) (eq (first type) 'coalton:List))
         (format nil "[~{~a~^, ~}]" (mapcar (lambda (x) (debug-value x (second type))) v)))
        (t (debug-plain v type))))

(defn debug-plain ((v t) (type t)) string
  (typecase v
    (null (if (eq type 'coalton:Boolean) "false" "None"))
    ((eql t) "true")
    ;; Coalton constructors without fields, like Power's Poweroff.
    (standard-object
     (let ((name (symbol-name (type-of v))))
       (string-capitalize (subseq name (1+ (position #\/ name))))))
    (string (format nil "~s" v))
    (character (format nil "'~a'" v))
    (float (if (= v (ftruncate v)) (format nil "~,1f" v) (format nil "~f" v)))
    (otherwise (princ-to-string v))))

(defn action-debug ((a (or null action))) string
  "As Rust's golden files show it: \"Fun(Dice(20))\", \"-\" for none."
  (if (null a)
      "-"
      (let* ((c (action-command a))
             (values (loop for v in (action-fields a)
                           for (nil type) in (command-fields c)
                           collect (debug-value v type)))
             (inner (cond ((null values) (command-kind c))
                          ((command-named c)
                           (format nil "~a { ~{~a: ~a~^, ~} }" (command-kind c)
                                   (loop for (name) in (command-fields c) for v in values
                                         collect (substitute #\_ #\- (string-downcase name))
                                         collect v)))
                          (t (format nil "~a(~{~a~^, ~})" (command-kind c) values)))))
        (if (command-bare c) inner (format nil "~a(~a)" (camel (command-skill c)) inner)))))
