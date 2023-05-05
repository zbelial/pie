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
(require 'subr-x)
(require 'autoload)

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

(defcustom pie-git-depth 1
  "If loss percent is higher than this, display in a particular color."
  :type 'integer
  :group 'pie)

(defcustom pie-directory (expand-file-name "pie" user-emacs-directory)
  "The directory used to store packages."
  :type  'directory
  :group 'pie)

(defun pie--builds-directory ()
  (expand-file-name "builds" pie-directory))

(defun pie--repos-directory ()
  (expand-file-name "repos" pie-directory))

(defvar pie--packages (make-hash-table :test #'equal)
  "key is the package name, value is an instance of `pie-package'")

(cl-defstruct pie-package
  (package) ;; string
  (url) ;; string
  (backend) ;; symbol
  (branch) ;; string
  (rev) ;; string, if provided, rev will override branch
  (build) ;; function
  (deps) ;; list of symbol
  (dir) ;; which directory this package is/will be cloned to.
  (build-dir) ;; which directory this package is/will be installed to.
  (lisp-dir) ;; optional, which directory elisp files are in
  )

(defun pie--add-to-packages (pp)
  (let ((package (pie-package-package pp))
        (url (pie-package-url pp)))
    (puthash package pp pie--packages)))

;;;###autoload
(cl-defun pie (package url &key backend rev branch build deps lisp-dir)
  "Fetch a package and (optionally) build it for using with Emacs.

Usage:

  (pie package-name
     [:keyword [option]]...)

:url             String. The URL of the repository used to fetch the package source.
:backend         Symbol, optionally. Can be 'http or a VC backend in `vc-handled-backends'.
                 If not specified, use `pie-vc-heuristic-alist' to determine the backend
                 (or `pie-vc-default-backend' if no backend can be determined).
:branch          String, optionally. Which branch to checkout after cloning the repository.
:rev             String, optionally. Which revision to clone. Has higher priority than :branch.
:build           Function, optionally. Specify how to build the package.
                 If not specified, use `pie-default-build'.
:deps            List of string or a function returning a list of string, optionally.
:lisp-dir        String, optionally. Subdirectory name inside the repository.
"
  (let (pp
        (backend backend)
        (rev rev)
        (branch branch)
        (build build)
        (deps deps)
        (lisp-dir lisp-dir)
        dir build-dir)
    (when (null backend)
      (setq backend (or (alist-get url pie-vc-heuristic-alist
                                   nil nil #'string-match-p)
                        pie-vc-default-backend)))
    (when (and (not (member backend vc-handled-backends))
               (not (eq backend 'http)))
      (user-error "Invalid backend %s" backend))
    (setq dir (expand-file-name package (pie--repos-directory)))
    (setq build-dir (expand-file-name package (pie--builds-directory)))
    (if lisp-dir
        (setq lisp-dir (expand-file-name lisp-dir build-dir))
      (setq lisp-dir build-dir))
    (when (functionp deps)
      (setq deps (funcall deps)))
    (setq pp (make-pie-package :package package
                               :url url
                               :rev rev
                               :backend backend
                               :branch branch
                               :build build
                               :deps deps
                               :dir dir
                               :build-dir build-dir
                               :lisp-dir lisp-dir))
    (pie--add-to-packages pp)))
;; (pie "pie" "https://bitbucket.org/zbelial/pie" :backend 'Git :deps (lambda () '("abc")))
;; (pie "stock" "https://bitbucket.org/zbelial/stock" :backend 'Git)

(defun pie--installed-p (pp)
  "Check whether package `pp' has been fetched. `pp' is an instance of `pie-package'"
  (and pp
       (pie--fetched-p pp)
       (pie--built-p pp)))

(defun pie--fetched-p (pp)
  "Check whether package `pp' has been fetched. `pp' is an instance of `pie-package'"
  (when pp
    (let ((dir (pie-package-dir pp)))
      (and dir
           (file-directory-p dir)
           (not (directory-empty-p dir))))))

(defun pie--built-p (pp)
  "Check whether package `pp' has been fetched. `pp' is an instance of `pie-package'"
  (when pp
    (let ((build-dir (pie-package-build-dir pp)))
      (and build-dir
           (file-directory-p build-dir)
           (not (directory-empty-p build-dir))))))

(defun pie--installed-by-name-p (name)
  "Check whether package `pp' has been fetched. `pp' is an instance of `pie-package'"
  (let ((pp (gethash name pie--packages)))
    (pie--installed-p pp)))

(defun pie--install-package-by-name (name)
  (let ((pp (gethash name pie--packages)))
    (if pp
        (pie--install-package pp)
      (user-error "No package named %s is defined." name))))

(defun pie--git-depth ()
  (let ((depth ""))
    (when (and pie-git-depth
               (> pie-git-depth 0))
      (setq depth (format " --depth %d " pie-git-depth)))
    depth))
;; (pie--git-depth)

(defun pie--git-clone-advice (url dir rev)
  (message "pie--git-clone-advice")
  (if rev
      (vc-git--out-ok "clone"  "--branch" rev (pie--git-depth) "--single-branch" url dir)
    (vc-git--out-ok "clone" (pie--git-depth) url dir)
    dir))

(defun pie--build-package (pp &optional buildp)
  (let ((dir (pie-package-dir pp))
        (build-dir (pie-package-build-dir pp))
        (lisp-dir (pie-package-lisp-dir pp))
        (build (pie-package-build pp))
        (name (pie-package-package pp))
        (deps (pie-package-deps pp)))
    (when (and (pie--fetched-p pp)
               (or (not (pie--built-p pp))
                   buildp))
      (message "build package %s" name)
      (delete-directory build-dir t)
      (make-directory build-dir t)
      (copy-directory dir build-dir t t t)
      (let ((default-directory build-dir))
        (if build
            (funcall build build-dir)
          (pie-default-build pp)))
      (message "Finish building %s" name))))

(defun pie--install-package (pp)
  (let ((dir (pie-package-dir pp))
        (build-dir (pie-package-build-dir pp))
        (lisp-dir (pie-package-lisp-dir pp))
        (build (pie-package-build pp))
        (name (pie-package-package pp))
        (deps (pie-package-deps pp))
        buildp)
    ;; install all deps first
    (when deps
      (cl-dolist (dep deps)
        (pie--install-package-by-name dep)))
    ;; fetch package
    (unless (pie--fetched-p pp)
      (pie--fetch-package pp)
      (setq buildp t))
    ;; build package
    (pie--build-package pp buildp)
    (message "Finish installing %s" name)))

(defun pie--git-clone (url dir rev)
  (let (cmd)
    (if rev
        (setq cmd (concat "git --no-pager clone " (pie--git-depth) " --branch " rev " --single-branch " url " " dir))
      (setq cmd (concat "git --no-pager clone " (pie--git-depth) url " " dir)))
    (call-process-shell-command cmd nil nil)))

(defun pie--download (url dir)
  (let (name
        newname)
    (setq name (substring url (- (length url) (string-search "/" (reverse url)))))
    (setq newname (expand-file-name name dir))
    (url-copy-file url newname)))

(defun pie--fetch-package (pp)
  "Fetch package `pp', where `pp' is an instance of `pie-package'."
  (let ((dir (pie-package-dir pp))
        (url (pie-package-url pp))
        (rev (pie-package-rev pp))
        (backend (pie-package-backend pp))
        (branch (pie-package-branch pp))
        (name (pie-package-package pp)))
    (message "Start to fetch %s" name)
    (unless (file-exists-p dir)
      (make-directory dir t))
    (cond
     ((eq backend 'Git)
      (pie--git-clone url dir (or (and (not (eq rev :last-release)) rev) branch)))
     ((eq backend 'http)
      (pie--download url dir))
     (t
      (vc-clone url backend dir
                (or (and (not (eq rev :last-release)) rev) branch))))
    (if (not (pie--fetched-p pp))
        (error "Failed to clone %s from %s" name url)
      (message "Finish fetching %s" name))))

(defun pie-default-build (pp)
  "Compile elisp files in a repository."
  (let* ((lisp-dir (pie-package-lisp-dir pp))
         (default-directory lisp-dir)
         (files (directory-files lisp-dir t "\\.el$"))
         (autoloads (concat (file-name-nondirectory (directory-file-name lisp-dir)) "-autoloads.el")))
    (add-to-list 'load-path lisp-dir)
    (cl-dolist (file files)
      (when (not (string-suffix-p ".dir-locals.el" file))
        (byte-compile-file file)))
    (make-directory-autoloads lisp-dir (expand-file-name autoloads lisp-dir))))

;;;###autoload
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
                                            :backend (pie-package-backend pp)
                                            :branch (pie-package-branch pp)
                                            :build (pie-package-build pp)
                                            :deps (pie-package-deps pp)
                                            :dir dir-tmp))
            (delete-directory dir-tmp t)
            (pie--install-package pp-tmp)
            (delete-directory dir t)
            (rename-file dir-tmp dir)
            ;; build
            (pie--build-package pp t))
        (user-error "No package named %s is defined" name)))))

;;;###autoload
(defun pie-rebuild-package ()
  (interactive)
  (let (name
        pp
        packages)
    (setq packages (hash-table-keys pie--packages))
    (setq name (completing-read "Package Name: " packages))
    (when name
      (setq pp (gethash name pie--packages))
      (if pp
          (pie--build-package pp t)
        (user-error "No package named %s is defined" name)))))

(defun pie--active-package (pp)
  (let* ((lisp-dir (pie-package-lisp-dir pp))
         (autoloads (expand-file-name (concat (file-name-nondirectory (directory-file-name lisp-dir)) "-autoloads.el") lisp-dir)))
    (when (pie--built-p pp)
      (add-to-list 'load-path lisp-dir)
      (when (file-exists-p autoloads)
        (load-file autoloads)))))

;;;###autoload
(defun pie-install-packages ()
  "Fetch all packages in `pie--packages' if they have not been installed. Called after the last `pie' invoking."
  (interactive)
  (condition-case err
      (progn
        (cl-dolist (pp (hash-table-values pie--packages))
          (pie--install-package pp)
          (pie--active-package pp))
        (message "All packages have been installed."))
    (error
     (message "Error when installing packages: %S" err))))
;; (pie-install-packages)


(provide 'pie)

;;; pie.el ends here
