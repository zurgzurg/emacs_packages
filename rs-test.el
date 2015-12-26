;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rs-test-cur-buffer nil)
(defvar rs-test-cur-proc nil)
(defvar rs-test-log-buffer nil)
(defvar rs-test-log-marker nil)

(defvar rs-test-prog "/home/CORP/rbhamidipaty/lib_emacs/rs-test-gen.py")

(defun rs-test-setup (&rest args)
  (setq rs-test-cur-buffer (generate-new-buffer "*rs-test*"))
  (setq rs-test-cur-proc (apply 'start-process "rs-test-process"
				rs-test-cur-buffer rs-test-prog args))
  (rs-mode-test rs-test-cur-proc))
    
(defun rs-test-teardown ()
  (delete-process rs-test-cur-proc)
  (kill-buffer rs-test-cur-buffer))

(defun rs-test-expect-buffer (exp)
  (sit-for 0.1)
  (while (eq (process-status rs-test-cur-proc) 'run)
    (rs-test-log "waiting for process to finish")
    (sit-for 0.1))
  (with-current-buffer rs-test-cur-buffer
    (let (act)
      (setq act (buffer-substring (point-min) (point-max)))
      (unless (string-equal act exp)
	(rs-test-log "buffer check failed\nexpected:")
	(print exp rs-test-log-marker)
	(rs-test-log "actual:")
	(print act rs-test-log-marker)
	(throw 'mismatch nil)))))

(defun rs-test-log (fmt &rest args)
  (with-current-buffer rs-test-log-buffer
    (goto-char (point-max))
    (insert (apply 'format fmt args) "\n")
    (set-marker rs-test-log-marker (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-test-test-01 ()
  (rs-test-setup)
  (rs-test-teardown)
  t)

(defun rs-test-test-02 ()
  (rs-test-setup "simple_send" "abc")
  (unwind-protect
      (rs-test-expect-buffer "abc")
    (rs-test-teardown))
  t)

(defun rs-test-test-03 ()
  (rs-test-setup "simple_send" "abc\ndef")
  (unwind-protect
      (rs-test-expect-buffer "abc\ndef")
    (rs-test-teardown))
  t)

(defun rs-test-test-04 ()
  (rs-test-setup "count_lines" "text" "1" "5")
  (unwind-protect
      (rs-test-expect-buffer "text1\ntext2\ntext3\ntext4\ntext5\n")
    (rs-test-teardown))
  t)

(defun rs-test-test-05 ()
  (rs-test-setup "simple_send" "abc\b\b\bxyz")
  (unwind-protect
      (rs-test-expect-buffer "xyz")
    (rs-test-teardown))
  t)

(defun rs-test-test-06 ()
  (rs-test-setup "simple_send" "abc\r\ndef\r\ndef\r\ndef")
  (unwind-protect
      (rs-test-expect-buffer "abc\ndef\ndef\ndef")
    (rs-test-teardown))
  t)

(defun rs-test-test-07 ()
  (rs-test-setup "simple_send" "abc\n\rdef")
  (unwind-protect
      (rs-test-expect-buffer "abc\ndef")
    (rs-test-teardown))
  t)

(defun rs-test-test-08 ()
  (rs-test-setup "simple_send" "1\r\n2\r\n3\r\n4\r\n5\r\n6\r\n")
  (unwind-protect
      (rs-test-expect-buffer "1\n2\n3\n4\n5\n6\n")
    (rs-test-teardown))
  t)

(defun rs-test-test-09 ()
  (rs-test-setup "simple_send" "line1\rline2\rline3\rline4\r\n")
  (unwind-protect
      (rs-test-expect-buffer "line4\n")
    (rs-test-teardown))
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
  (setq rs-test-log-buffer (get-buffer-create "*rs-test-log*"))
  (with-current-buffer rs-test-log-buffer
    (erase-buffer)
    (setq rs-test-log-marker (make-marker))
    (set-marker rs-test-log-marker (point-min)))
  (rs-test-log "start test run %s" (current-time-string))
  (let (cur pass-list fail-list)
    (while tlist
      (setq cur (car tlist))
      (setq tlist (cdr tlist))
      (rs-test-log "Starting test %s" (symbol-name cur))
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
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rs-test)
