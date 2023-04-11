;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Nasm support
(package! nasm-mode)

;; Nov.el (For epubs). Uses doom-variable-pitch-font
(package! nov)

;; Generate doxygen documentation from C code
(package! gendoxy
  :recipe (:host github :repo "mp81ss/gendoxy"))
