;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; string utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ram-string-ends-with (s pat)
  (let (len-s len-pat)
    (setq len-s (length s))
    (setq len-pat (length pat))
    (cond
     ((> len-pat len-s)
      nil)
     (t
      (string-equal (substring s (- len-s len-pat)) pat)))))

(defun ram-string-r-find (s ch)
  "Find the right most occurance of character ch in string s."
  (let (idx r)
    (setq idx (- (length s) 1))
    (while (and (null r) (>= idx 0))
      (if (eql (elt s idx) ch)
	  (setq r idx))
      (setq idx (- idx 1)))
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file and directory name utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ram-dir-get-parent (d)
  "Given a path name d return the parent directory.
Returns nil if no parent exits."
  (let (r len sep)
    (setq r (substring d 0)) ; duplicate d
    (setq len (length r))
    (while (eql (elt r (- len 1)) ?/)
      (setq len (- len 1))
      (setq r (substring r 0 len)))
    (setq sep (ram-string-r-find r ?/))
    (if (and sep (> sep 0))
	(substring r 0 sep)
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ram-utils)
