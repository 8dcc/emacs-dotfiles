;;; custom-functions.el --- Description -*- lexical-binding: t; -*-



(provide 'custom-functions)

;; make-parent: compile current project with the make file at N parent levels,
;; using the specified targets.
(defun make-parent (&optional parent-level targets)
  (interactive "nParent directories: \nsTarget: ")
  (if (or (equal parent-level nil)
          (< parent-level 0))
      (setq parent-level 1))
  (if (equal targets nil)
      (setq targets ""))
  (let ((rel-path "./"))
    (dotimes (i parent-level)
      (setq rel-path (concat rel-path "../")))
    (compile (format "make -C %s %s" rel-path targets))))

;;; custom-functions.el ends here
