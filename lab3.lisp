(defun my-map (func lst)
  (when lst
    (cons (funcall func (first lst))
          (my-map func (rest lst)))))

(defun filter (pred lst)
  (cond ((null lst) nil)
        ((funcall pred (first lst)) (cons (first lst)
                                          (filter pred (rest lst))))
        (t (filter pred (rest lst)))))

(defun my-reduce (func lst &key (initial-value nil initial-value-p))
  (labels ((%my-reduce (res lst)
             (if (null lst)
                 res
                 (%my-reduce (funcall func res (first lst))
                             (rest lst)))))
    (if initial-value-p
        (%my-reduce initial-value lst)
        (when lst
          (%my-reduce (first lst) (rest lst))))))
