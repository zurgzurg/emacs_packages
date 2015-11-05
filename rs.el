;;
;; ram serial mode
;;
(require 'term)

(define-derived-mode rs-mode fundamental-mode "RS"
  "Major mode to interact with a serial port."
  t)

(defun rs-start (port speed)
  "Start ram serial mode."
  (interactive (list (serial-read-name) (serial-read-speed)))
  (let*
      ((process (make-serial-process
		 :port port
		 :speed speed
		 :coding 'no-conversion
		 :noquery t))
       (buffer (process-buffer process)))
    (set-buffer buffer)
    (rs-mode)
    (set-process-filter process 'rs-filter)
    buffer))

(defun rs-start-test ()
  "Test mode."
  (interactive)
  (let*
      ((buffer (generate-new-buffer-name "rs-test"))
       (process (start-process "rs-test" buffer
			       "/usr/bin/python"
			       "/home/CORP/rbhamidipaty/scripts/serial")))
    (set-buffer buffer)
    (rs-mode)
    (set-process-filter process 'rs-filter)
    buffer))

(defun rs-end ()
  (interactive)
  (let ((b (current-buffer)))
    (delete-process (get-buffer-process b))
    (kill-buffer b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; escape sequences
;;
;;  0 - black
;;  1 - read
;;  2 - green
;;  3 - yellow
;;  4 - blue
;;  5 - magenta
;;  6 - cyan
;;  7 - white
;;  9 - default
;;
;; [ 0 m       reset all attributes
;; [ 1 m       set bold/bright attribute
;; [ 2 m       set dim attribute
;;
;; [ 4 m       set underline attribute
;; [ 5 m       set blink attribute
;; [ 7 m       set reverse attribute
;; [ 8 m       set hidden attribute
;;
;; [ 3 <#> m   set foreground to color <#>
;; [ 4 <#> m   set background to color <#>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rs-apply-insert-filters (s)
  (replace-regexp-in-string "\r" "" s))

(defun rs-do-append (buf s)
  (with-current-buffer buf
    (if (= (point) (point-max))
	(insert s)
      (save-excursion
	(goto-char (point-max))
	(insert s)))))

(defun rs-filter (proc string)
  (let (s)
    (setq s (rs-apply-insert-filters string))
    (rs-do-append (process-buffer proc) s)))
