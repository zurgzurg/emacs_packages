;;
;; ram serial mode
;;

;;
;; pick up functions: serial-read-name serial-speed
(require 'term)

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

(defun rs-send-intr ()
  (interactive)
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

(defun rs-mode (port speed)
  "Major mode to interact with a serial port."
  (interactive (list (serial-read-name) (serial-read-speed)))
  (let (p b)
    (setq p (rs-setup-serial-port port speed))
    (setq b (process-buffer p))

    (set-buffer b)
    (kill-all-local-variables)

    ;; define all the buffer local rs-mode variables
    (make-local-variable 'rs-chunk-number)
    (make-local-variable 'rs-serial-port)
    (make-local-variable 'rs-process)

    (setq rs-chunk-number 0)
    (setq rs-serial-port port)
    (setq rs-process p)

    (setq major-mode 'rs-mode)
    (setq mode-name "RS")
    (if (null rs-mode-map)
	(setq rs-mode-map (rs-make-keymap)))
    (use-local-map rs-mode-map)

    (set-process-filter p 'rs-filter)
    (switch-to-buffer b)))

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
    (write-region nil nil fname)
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
      (delete file f)))

  (setq rs-chunk-number 0)
  (erase-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
    (rs-do-append (process-buffer proc) s)
    (if (and (numberp rs-chunk-size) (>= (point-max) rs-chunk-size))
	(rs-do-chunk-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'rs)
