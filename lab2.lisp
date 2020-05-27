(defun associative-list-add (list key value)
  (cons (cons key value) list))

(defun associative-list-get (list key)
  (cond ((null list) (values nil nil))
        ((eq key (first (first list))) (values (second (first list)) t))
        (t (associative-list-get (rest list) key))))

(defun property-list-add (list key value)
  (cons key (cons value list)))

(defun property-list-get (list key)
  (cond ((null list) (values nil nil))
        ((eq (first list) key) (values (second list) t))
        (t (property-list-get (rest (rest list)) key))))

(defun binary-tree-add (tree key value)
  (if tree 
   (let ((node-key-value (first tree))
         (first-child (second tree))
         (second-child (third tree)))
     (list
      node-key-value
      (or (and (string< (first node-key-value) key)
               (binary-tree-add first-child key value))
          first-child)
      (or (and (string>= (first node-key-value) key)
               (binary-tree-add second-child key value))
          second-child)))
   (list (cons key value) nil nil)))

(defun binary-tree-get (tree key)
  (let ((node-key (first (first tree)))
        (node-value (rest (first tree))))
    (cond ((null tree) (values nil nil))
          ((string= node-key key) (values node-value t))
          ((string< node-key key) (binary-tree-get (second tree) key))
          (t (binary-tree-get (third tree) key)))))
