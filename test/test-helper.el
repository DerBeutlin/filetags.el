;;; test-helper.el --- Helpers for filetags-test.el

(defun create-filetags-file (path lines)
  (with-temp-buffer
    (dolist (line lines)
      (insert line)
      (insert "\n"))
    (write-file path)))

;;; test-helper.el ends here
