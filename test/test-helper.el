;;; test-helper.el --- Helpers for filetags-test.el
(defun ensure-empty-directory (path)
  (progn
    (when (file-directory-p path) (delete-directory path t))
    (make-directory path t)))
;;; test-helper.el ends here
