;;; -*- lexical-binding: t -*-
(setq-default lexical-binding t)

;; precompute one big autoloads file, makes loading them faster
(setq package-quickstart t)

;; disable some things that slow down startup
;; there's no need to be stirngent about garbage collection when starting up
(setq gc-cons-threshold 524288000 gc-cons-percentage 0.7)
;; also we dont need to figure out the file-handlers for the files we visit,
;; we're only visiting config files
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
