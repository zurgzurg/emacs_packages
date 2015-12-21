;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rs-test-cur-buffer nil)
(defvar rs-test-cur-proc nil)

(defvar rs-test-prog "/home/CORP/rbhamidipaty/lib_emacs/rs-test-gen.py")

(defun rs-test-setup (&rest args)
  (setq rs-test-cur-buffer (generate-new-buffer "*rs-test*"))
  (setq rs-test-cur-proc (apply 'start-process "rs-test-process"
				rs-test-cur-buffer rs-test-prog args))
  (rs-mode-test rs-test-cur-proc))
    

(defun rs-test-teardown ()
  (kill-buffer rs-test-cur-buffer))

(defun rs-test-expect-buffer (str)
  (with-current-buffer rs-test-cur-buffer
    (if (null (string-equal
	       (buffer-substring (point-min) (point-max))
	       str))
	(throw 'mismatch nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-test-test-01 ()
  (rs-test-setup)
  (rs-test-teardown)
  t)

(defun rs-test-test-02 ()
  (rs-test-setup "simple_send" "abc")
  (rs-test-expect-buffer "abc")
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
      (if (catch 'mismatch (funcall cur))
	  (setq pass-list (cons cur pass-list))
	(setq fail-list (cons cur fail-list))))
    (list pass-list fail-list)))

(defun rs-test-run-all-tests ()
  (let (tlist result)
    (setq tlist (rs-test-find-tests))
    (setq result (rs-test-run-tests tlist))
    (message "Test result: pass=%d fail=%d"
	     (length (car result)) (length (cadr result)))))
	     
