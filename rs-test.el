(defun rs-test-01 ()
  (interactive)
  (let (b)
    (setq b (generate-new-buffer "rs-test"))
    (set-buffer b)
    (setq rs-chunk-size 400)
    (setq rs-chunk-number 0)
    (setq rs-serial-port "/dev/serial0")
    (rs-do-chunk-output)
    (kill-buffer b)
    t))
    