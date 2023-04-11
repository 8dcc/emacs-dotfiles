;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Nasm support
(package! nasm-mode :pin "824d11fdceb01dca9ae2757670ee1f24ca2477de")

;; Nov.el (For epubs). Uses doom-variable-pitch-font
(package! nov :pin "6c992c2c5c4ad03a0f432a74fe4d0cde3b6da4bd")

;; Generate doxygen documentation from C code
(package! gendoxy :pin "824d11fdceb01dca9ae2757670ee1f24ca2477de"
  :recipe (:host github :repo "mp81ss/gendoxy"))
