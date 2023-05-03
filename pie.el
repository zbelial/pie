;;; pie.el --- Package installer for Emacs -*- lexical-binding: t -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: (Emacs)
;; Homepage: https://bitbucket.org/zbelial/pie
;; Keywords: Package Emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'vc)
(require 'cl-generic)
(eval-when-compile
  (require 'cl-macs))

(defgroup pie nil
  "Package installer for Emacs."
  :group 'package
  :version "28.2")

(defcustom pie-vc-heuristic-alist
  `((,(rx bos "http" (? "s") "://"
          (or (: (? "www.") "github.com"
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "codeberg.org"
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: (? "www.") "gitlab" (+ "." (+ alnum))
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "git.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "git." (or "savannah" "sv") "." (? "non") "gnu.org/"
                 (or "r" "git") "/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          (or (? "/") ".git") eos)
     . Git)
    (,(rx bos "http" (? "s") "://"
          (or (: "hg.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "hg." (or "savannah" "sv") "." (? "non") "gnu.org/hgweb/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Hg)
    (,(rx bos "http" (? "s") "://"
          (or (: "bzr." (or "savannah" "sv") "." (? "non") "gnu.org/r/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Bzr))
  "Heuristic mapping URL regular expressions to VC backends."
  :type `(alist :key-type (regexp :tag "Regular expression matching URLs")
                :value-type (choice :tag "VC Backend"
                                    ,@(mapcar (lambda (b) `(const ,b))
                                              vc-handled-backends)))
  :version "28.2")

(defcustom pie-vc-default-backend 'Git
  "Default VC backend used when cloning a package repository.
If no repository type was specified or could be guessed by
`pie-vc-heuristic-alist', this is the default VC backend
used as fallback.  The value must be a member of
`vc-handled-backends' and the named backend must implement
the `clone' function."
  :type `(choice ,@(mapcar (lambda (b) (list 'const b))
                           vc-handled-backends))
  :version "28.2")

(defcustom pie-directory (expand-file-name "pie" user-emacs-directory)
  "The directory used to store packages."
  :type  'directory
  :group 'pie)

(defvar pie-repos-directory (expand-file-name "repos" pie-directory)
  "Location of the repos directory.")

(defvar pie--packages (make-hash-table :test #'equal)
  "key is the package name, value is an instance of `pie-package'")

(cl-defstruct pie-package
  (package) ;; string
  (url) ;; string
  (vc-backend) ;; symbol
  (branch) ;; string
  (rev) ;; string, if provided, rev will override branch
  (build) ;; function
  (deps) ;; list of symbol
  (dir) ;; which directory this package is/will be installed
  )

;;;###autoload
(cl-defun pie (package url &key vc-backend rev branch build deps)
  "Fetch a package and (optionally) build it for using with Emacs.

Usage:

  (pie package-name
     [:keyword [option]]...)

:url             String. The URL of the repository used to fetch the package source.
:vc-backend      Symbol, optionally. Which VC backend to use to clone the repository.
                 If not specified, use `pie-vc-heuristic-alist' to determine the backend
                 (or `pie-vc-default-backend' if no backend can be determined from it).
:branch          String, optionally. Which branch to checkout after cloning the repository.
:rev             String or symbol, optionally. Which revision to clone.
:build           Function, optionally. Specify how to build 
:deps            List of symbol, optionally. 
"
  (let (pp
        (vc-backend vc-backend)
        (rev rev)
        (branch branch)
        (build build)
        (deps deps)
        dir)
    (when (null vc-backend)
      (setq vc-backend (or (alist-get url pie-vc-heuristic-alist
                                      nil nil #'string-match-p)
                           pie-vc-default-backend)))
    (when (not (member vc-backend vc-handled-backends))
      (user-error "Invalid vc-backend %s" vc-backend))
    (setq dir (expand-file-name package pie-directory))
    (setq pp (make-pie-package :package package
                               :url url
                               :rev rev
                               :vc-backend vc-backend
                               :branch branch
                               :build build
                               :deps deps
                               :dir dir))
    (puthash package pp pie--packages)))
;; (pie "pie" "https://bitbucket.org/zbelial/pie" :vc-backend 'Git)
;; (pie "stock" "https://bitbucket.org/zbelial/stock" :vc-backend 'Git)

(defun pie-default-build ()
  "Compile elisp files in a repository. TODO"
  (let ((dir default-directory))
    )
  )

(defun pie--installed-p (pp)
  "Check whether package `pp' has been fetched. `pp' is an instance of `pie-package'"
  (when pp
    (let ((dir (pie-package-dir pp)))
      (and dir
           (file-directory-p dir)
           (not (directory-empty-p dir))))))

(defun pie--install-package-by-name (name)
  (let ((pp (gethash name pie--packages)))
    (if pp
        (progn
          (when (not (pie--installed-p pp))
            (pie--install-package pp)))
      (user-error "No package named %s is defined." name))))

(defun pie--install-package (pp)
  "Fetch package `pp', where `pp' is an instance of `pie-package'."
  (let ((dir (pie-package-dir pp))
        (url (pie-package-url pp))
        (rev (pie-package-rev pp))
        (vc-backend (pie-package-vc-backend pp))
        (branch (pie-package-branch pp))
        (build (pie-package-build pp))
        (deps (pie-package-build pp)))
    ;; install all deps first
    (when deps
      (cl-dolist (dep deps)
        (pie--install-package-by-name dep)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (vc-clone url vc-backend dir
                      (or (and (not (eq rev :last-release)) rev) branch))
      (error "Failed to clone %s from %s" name url))
    (when build
      (let ((default-directory dir))
        (funcall build)))
    (add-to-list 'load-path dir)))

(defun pie-update-package ()
  "Update a package in `pie--packages'."
  (interactive)
  (let (name
        pp
        pp-tmp
        packages
        dir
        dir-tmp)
    (setq packages (hash-table-keys pie--packages))
    (setq name (completing-read "Package Name: " packages))
    (when name
      (setq pp (gethash name pie--packages))
      (if pp
          (progn
            ;; first, clone the package to a tmp directory, then
            ;; delete the original directory and rename the tmp directory
            (setq dir (pie-package-dir pp))
            (setq dir-tmp (concat dir "-tmp"))
            (setq pp-temp (make-pie-package :package (pie-package-package pp)
                                            :url (pie-package-url pp)
                                            :rev (pie-package-rev pp)
                                            :vc-backend (pie-package-vc-backend pp)
                                            :branch (pie-package-branch pp)
                                            :build (pie-package-build pp)
                                            :deps (pie-package-deps pp)
                                            :dir dir-tmp))
            (delete-directory dir-tmp t)
            (pie--install-package pp-tmp)
            (delete-directory dir t)
            (rename-file dir-tmp dir))
        (user-error "No package named %s is defined" name)))))

(defun pie-install-packages ()
  "Fetch all packages in `pie--packages' if they have not been installed. Called after the last `pie' invoking."
  (interactive)
  (cl-dolist (pp (hash-table-values pie--packages))
    (unless (pie--installed-p pp)
      (pie--install-package pp)))
  (message "All packages have been installed."))
;; (pie-install-packages)


(provide 'pie)

;;; pie.el ends here
