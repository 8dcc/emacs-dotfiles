;;; custom-functions.el --- Custom 8dcc functions -*- lexical-binding: t; -*-

(provide 'custom-functions)

;; NOTE: Currently not bound since you can use <SPC p c> to compile from project
(defun make-parent (&optional parent-level targets)
  "Compile current project with the make file at N parent levels,
using the specified targets."
  (interactive "nParent directories: \nsTarget: ")
  (if (or (not parent-level)
          (< parent-level 0))
      (setq parent-level 1))
  (if (not targets)
      (setq targets ""))
  (let ((rel-path "./"))
    (dotimes (i parent-level)
      (setq rel-path (concat rel-path "../")))
    (compile (format "make -C %s %s" rel-path targets))))

;;; custom-functions.el ends here
