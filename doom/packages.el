;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; NOTE: Packages are pinned to specific commits for making the enviroment as
;; reproducible as possible. Most packages haven't been updated in a long time,
;; so it's not a big deal.
;; Make sure you check the repos/updates from time to time.

;; Nasm support. Forked for simpler comment system
(package! nasm-mode :pin "4db384ef241c986444233a298ede25fd7d9ebfe9"
  :recipe (:host github :repo "8dcc/nasm-mode"))

;; Nov.el (For epubs). Uses doom-variable-pitch-font
(package! nov :pin "6c992c2c5c4ad03a0f432a74fe4d0cde3b6da4bd")

;; Org auto tangle
(package! org-auto-tangle :pin "817eabf902e759e96782bdc54d2dab36c4a2c5ab")

;; Generate doxygen documentation from C code. Forked for more compact coments
(package! gendoxy :pin "d81a9fad640db336d590f60d8447b2c6224eb2b1"
  :recipe (:host github :repo "8dcc/gendoxy"))

;; Play chess from emacs. Supports games-board/gnuchess
(package! chess :pin "6c8a34e8befdb7da046d3606ae5a690cfb52cb2d")

;; For disassembling C sources
(package! disaster :pin "16bba9afb92aacf06c088c29ba47813b65a80d87")
