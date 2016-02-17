;;
;; ram serial mode
;;

;;
;; pick up functions: serial-read-name serial-speed
(require 'term)
(require 'rs-test)

;;
;; debug support
;;

;;(setq debug-on-error t)
;;(setq debug-on-error nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; global variables
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rs-mode-map nil
  "Keymap for rs-mode.")

(defvar rs-chunk-size 1000000
  "Chunk size.
 Buffers will not be larger than this limit. Instead the
data will be saved to disk as a file.")

(defvar rs-maximum-size 1000000000
  "Maximum amount of data to save.
A value of nil means no limit - save everything.")

(defvar rs-chunk-directory "/home/CORP/rbhamidipaty"
  "Name of directory in which to save chunks.")

(defvar rs-chunk-file-pattern "pat-%s-%d"
  "File name pattern to use when saving buffer chunks.
The %s is replaced with the serial port name and the %d is the
chunk number.")

(defvar rs-chunk-file-regexp
  "pat-[[:alnum:]]+-[[:digit:]]+"
  "Regexp to match chunk file names.
Used to cleanup chunk output files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer-local config variables
;; these are forced to be buffer-local in rs-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rs-chunk-number 0
  "Next number to use when saving a chunk.")

(defvar rs-serial-port nil
  "Name of serial port for current buffer.")

(defvar rs-process nil
  "The process object - really just the serial port.")

(defvar rs-insert-pos nil
  "Place where text will be inserted.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-self-insert ()
  (interactive)
  (process-send-string rs-process (format "%c" last-command-event))
  ;(message "normal self insert : %s" (prin1-to-string rs-process))
  t)

(defun rs-newline ()
  (interactive)
  (process-send-string rs-process "\r")
  ;(message "sent newline : %s" (prin1-to-string rs-process))
  t)

(defun rs-make-keymap ()
  (let (m code)
    (setq m (make-sparse-keymap))
    (setq code ?\ ) ;; get ascii code for SPACE char
    (while (<= code ?~)
      (define-key m (byte-to-string code) 'rs-self-insert)
      (setq code (+ 1 code)))
    (define-key m "\C-c\C-c" 'rs-self-insert)
    (define-key m " " 'rs-self-insert)
    (define-key m (kbd "C-m") 'rs-newline)
    (define-key m "\d" 'rs-self-insert)
    m))

(defun rs-update-keymap ()
  (interactive)
  (setq rs-mode-map (rs-make-keymap))
  (use-local-map rs-mode-map))

(defun rs-setup-serial-port (port speed)
  "Start ram serial mode."
  (let
      ((process (make-serial-process
		 :port port
		 :speed speed
		 :coding 'no-conversion
		 :noquery t)))
    process))

(defun rs-sentinel (proc event)
  t)

(defun rs-mode-make-local-vars ()
    ;; define all the buffer local rs-mode variables
    (make-local-variable 'rs-chunk-number)
    (make-local-variable 'rs-serial-port)
    (make-local-variable 'rs-process)
    (make-local-variable 'rs-insert-pos))

(defun rs-mode-set-major-and-mode-map ()
    (setq major-mode 'rs-mode)
    (setq mode-name "RS")
    (if (null rs-mode-map)
	(setq rs-mode-map (rs-make-keymap)))
    (use-local-map rs-mode-map))

(defun rs-mode (port speed)
  "Major mode to interact with a serial port."
  (interactive (list (serial-read-name) (serial-read-speed)))
  (let (p b)
    (setq p (rs-setup-serial-port port speed))
    (setq b (process-buffer p))

    (set-buffer b)
    (kill-all-local-variables)
    (rs-mode-make-local-vars)

    (setq rs-insert-pos 1)
    (setq rs-chunk-number 0)
    (setq rs-serial-port port)
    (setq rs-process p)

    (rs-mode-set-major-and-mode-map)

    (set-process-filter p 'rs-filter)
    (set-process-sentinel p 'rs-sentinel)
    (switch-to-buffer b)))

(defun rs-mode-test (p)
  (let (b)
    (setq b (process-buffer p))

    (set-buffer b)
    (kill-all-local-variables)
    (rs-mode-make-local-vars)

    (setq rs-insert-pos 1)
    (setq rs-chunk-number 0)
    (setq rs-serial-port "test")
    (setq rs-process p)

    (rs-mode-set-major-and-mode-map)

    (set-process-filter p 'rs-filter)
    (set-process-sentinel p 'rs-sentinel)))


(defun rs-buffer-is-rs-buffer-p ()
  (if rs-process
      t
    nil))

(defun rs-end ()
  (interactive)
  (let ((b (current-buffer)))
    (delete-process (get-buffer-process b))
    (kill-buffer b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; color escape sequences
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file chunking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-do-chunk-output ()
  (let (fname short-serial-name)
    (setq short-serial-name
	  (replace-regexp-in-string "/dev/" "" rs-serial-port))
    (setq fname (format "%s/%s"
			rs-chunk-directory
			(format rs-chunk-file-pattern
				short-serial-name rs-chunk-number)))
    (let (coding-system-for-write)
      (setq coding-system-for-write 'no-conversion)
      (write-region nil nil fname))
    (setq rs-chunk-number (+ 1 rs-chunk-number))
    (erase-buffer)
    t))

(debug-on-entry 'rs-do-chunk-output)
(cancel-debug-on-entry 'rs-do-chunk-output)

(defun rs-reset ()
  "Reset mode.
Resets chunking. Erases buffer and all saved chunks."
  (interactive)
  (if (null (rs-buffer-is-rs-buffer-p))
      (error "Not in an RS buffer."))
  (let (f file-list)
    (setq file-list (directory-files rs-chunk-directory t
				     rs-chunk-file-regexp))
    (while file-list
      (setq f (car file-list))
      (setq file-list (cdr file-list))
      (delete-file f)))

  (setq rs-chunk-number 0)
  (erase-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer local vars to keep track of the insert
;; point and handle carriage return and newline
;; motion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-enable-debug ()
  (setq rs-test-log-buffer (get-buffer-create "*rs-test-log*"))
  (with-current-buffer rs-test-log-buffer
    (erase-buffer)
    (setq rs-test-log-marker (make-marker))
    (set-marker rs-test-log-marker (point-min))))

(defun rs-log (fmt &rest args)
  (when nil
    (if (null rs-test-log-buffer)
	(rs-enable-debug))
    (apply 'rs-test-log fmt args)))

(defun rs-handle-insert (s)
  (rs-log "got string: <%s>" (prin1-to-string s))
  (let (start idx ch tmp n-erase)
    (setq start 0)
    (while (setq idx (string-match "\\([\b\r\n]\\)" s start))

      (setq tmp (substring s start idx))
      (rs-log "doing insert idx=%d p=%d pm=%d %s"
	      idx (point) (point-max) tmp)

      (if (eql (point) (point-max))
	  (insert tmp)
	(rs-log "deleting %d chars" (length tmp))
	(delete-char (length tmp))
	(insert tmp))

      (setq ch (aref s idx))
      (cond

       ((eql ch ?\b)
	(delete-backward-char 1)
	(setq start (+ 1 idx)))

       ((eql ch ?\n)
	(goto-char (point-max))
	(insert ch)
	(setq start (+ 1 idx)))

       ((eql ch ?\r)
	(beginning-of-line)
	(setq start (+ 1 idx)))))

    (setq tmp (substring s start))
    (rs-log "doing insert2 p=%d pm=%d %s"
	    (point) (point-max) tmp)

    (if (eql (point) (point-max))
	(insert tmp)
      (setq n-erase (min (- (point-max) (point)) (length tmp)))
      (rs-log "deleting2 %d chars" n-erase)
      (delete-char n-erase)
      (insert tmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rs-apply-insert-filters (s)
  ;(replace-regexp-in-string "\r" "" s))
  s)

(defun rs-filter (proc string)
  ;(rs-log "Got string:\n>%s<" string)
  (let (b w wlist want-display-update prev-point)
    (setq b (process-buffer proc))
    (when (buffer-live-p b)
      (with-current-buffer b
	(setq wlist (get-buffer-window-list b nil t))
	(when (= (length wlist) 1)

	  (setq w (car wlist))
	  (setq prev-point (window-point w))
	  (when (= prev-point (point-max))
	    (setq want-display-update t)))

	(save-excursion
	  (goto-char rs-insert-pos)
	  (rs-handle-insert (rs-apply-insert-filters string))
	  (setq rs-insert-pos (point)))

	(if (and (numberp rs-chunk-size) (>= (point-max) rs-chunk-size))
	    (rs-do-chunk-output))

	(if want-display-update
	    (set-window-point w (point-max)))

	t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rs)
