;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rs-test-cur-buffer nil)

(defun rs-test-setup ()
  (setq rs-test-cur-buffer (generate-new-buffer "*rs-test*")))

(defun rs-test-teardown ()
  (kill-buffer rs-test-cur-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-test-test-01 ()
  (rs-test-setup)
  (rs-test-teardown)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test top level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-test-find-tests ()
  (let (tlist)
    (mapatoms '(lambda (sym)
		 (if (string-match "rs-test-test-" (symbol-name sym))
		     (setq tlist (cons sym tlist)))))
    (setq tlist (sort tlist 'string<))
    tlist))

(defun rs-test-run-tests (tlist)
  (let (cur pass-list fail-list)
    (while tlist
      (setq cur (car tlist))
      (setq tlist (cdr tlist))
      (if (funcall cur)
	  (setq pass-list (cons cur pass-list))
	(setq fail-list (cons cur fail-list))))
    (list pass-list fail-list)))

(defun rs-test-run-all-tests ()
  (let (tlist result)
    (setq tlist (rs-test-find-tests))
    (setq result (rs-test-run-tests tlist))
    (message "Test result: pass=%d fail=%d"
	     (length (car result)) (length (cadr result)))))
	     
