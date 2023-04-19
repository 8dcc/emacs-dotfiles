;;; custom-functions.el --- Custom 8dcc functions -*- lexical-binding: t; -*-

(provide 'custom-functions)

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

;; FIXME: Play beep (sound file) when battery% <10.
(defvar my-prev-battery nil)
(defun my-battery-alarm (data)
  (when (and my-prev-battery
             (equal (alist-get ?L data) "off-line")
             (< (string-to-number (alist-get ?p data)) 10)
             (>= (string-to-number (alist-get ?p my-prev-battery)) 10))
    (play-sound-file "~/.beep.wav" 15))
  (setq my-prev-battery data))

;;; custom-functions.el ends here
