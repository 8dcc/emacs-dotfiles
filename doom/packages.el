;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; NOTE: Packages are pinned to specific commits for making the enviroment as
;; reproducible as possible. Most packages haven't been updated in a long time,
;; so it's not a big deal.
;; Make sure you check the repos/updates from time to time.

;; Nasm support
(package! nasm-mode :pin "824d11fdceb01dca9ae2757670ee1f24ca2477de")

;; Nov.el (For epubs). Uses doom-variable-pitch-font
(package! nov :pin "6c992c2c5c4ad03a0f432a74fe4d0cde3b6da4bd")

;; Generate doxygen documentation from C code
(package! gendoxy :pin "824d11fdceb01dca9ae2757670ee1f24ca2477de"
  :recipe (:host github :repo "mp81ss/gendoxy"))
