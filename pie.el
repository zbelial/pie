;;; pie.el --- Package installer for Emacs -*- lexical-binding: t -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
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
(eval-and-compile
  (or (require 'loaddefs-gen nil t)
      (require 'autoload)))
(require 'pie-compat)

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
used as fallback. The value must be a member of
`vc-handled-backends' and the named backend must implement
the `clone' function."
  :type `(choice ,@(mapcar (lambda (b) (list 'const b))
                           vc-handled-backends))
  :version "28.2")

(defcustom pie-git-depth 1
  "Default depth used with git backend.
If no depth is specified explicitly, this value will be used."
  :type 'integer
  :group 'pie)

(defcustom pie-activite-package t
  "Whether activite packages after installing them."
  :type 'boolean
  :group 'pie)

(defcustom pie-repos-directory (expand-file-name "pie/repos" user-emacs-directory)
  "The directory used to store packages' repos."
  :type  'directory
  :group 'pie)

(defcustom pie-builds-directory (expand-file-name "pie/builds" user-emacs-directory)
  "The directory used to store built packages."
  :type  'directory
  :group 'pie)

(defcustom pie-rebuild-package-dwim nil
  "If t, rebuild packages outdated when emacs starting up."
  :type 'boolean
  :group 'pie)

(defvar pie--packages (make-hash-table :test #'equal)
  "Key is the package name, value is an instance of `pie-package'.")

(defvar pie--activate-cache (make-hash-table :test #'equal)
  "Key is the package name.
value is 0 or 1, where 0 means a package has been activated,
but is during rebuilding, 1 means a package has been activeted.")

(cl-defstruct pie-package
  (name) ;; string
  (package) ;; string
  (url) ;; string
  (backend) ;; symbol
  (branch) ;; string
  (rev) ;; string, if provided, rev will override branch
  (depth) ;; used with git clone. If specified, the value should be t (all history) or a positive integer. If omitted, use `pie-git-depth'
  (build) ;; function
  (deps) ;; list of symbol
  (repo-dir) ;; which directory this package is/will be cloned to.
  (build-dir) ;; which directory this package is/will be installed to.
  (lisp-dir) ;; optional, which directory elisp files are in
  (build-type) ;; optional
  (ignore-files) ;; optional, files ignored when building 
  )

(defun pie--add-to-packages (pp)
  "Add package PP to package hash table."
  (let ((package (pie-package-package pp)))
    (puthash package pp pie--packages)
    nil))

;;;###autoload
(cl-defun pie (package url &key backend rev branch depth build deps lisp-dir condition build-type ignore-files)
  "Fetch PACKAGE and (optionally) build it for using with Emacs.

Usage:

  (pie package-name
     [:keyword [option]]...)

:url             String.
                 The URL of the repository used to fetch the package source.
:backend         Symbol, optional.
                 Which BACKEND to clone.
                 Can be 'http or a VC backend in `vc-handled-backends'.
                 If not specified, use `pie-vc-heuristic-alist'
                 to determine the value,
                 (or `pie-vc-default-backend' if no backend can be determined).
:branch          String, optional.
                 Which BRANCH or tag to check out.
:rev             String, optional.
                 Which REV to clone.  Has higher priority than :branch.
:depth           t or a positve nubmer, optional.
                 Only works with git for now.
                 If specified, the value should be t (all history),
                 or a positive integer.
                 If omitted, use `pie-git-depth'
:build           Function, optional.
                 Specify how to build the package.
                 If not specified, use `pie-default-build'.
:deps            List of string or a function returning a list of string.
                 Optional.
:lisp-dir        String, optional.
                 Subdirectory containing elisp files inside the repository.
:build-type      If it's 'repo, then build the package in the repos directory. 
                 Else, build it in the builds directory.
:ignore-files    Files (no directory part) that should be ignored when building with `pie-default-build'
:condition       A function without any parameter, optional.
                 Only when it (if specified) returns t,
                 this package will be installed."
  (let (pp
        repo-dir
        build-dir)
    (when (and condition
               (functionp condition))
      (when (not (funcall condition))
        (message "Package [%s] need not to install." package)
        (cl-return-from pie)))
    (when (null backend)
      (setq backend (or (alist-get url pie-vc-heuristic-alist
                                   nil nil #'string-match-p)
                        pie-vc-default-backend)))
    (when (and (not (member backend vc-handled-backends))
               (not (eq backend 'http)))
      (user-error "Invalid backend %s" backend))
    (setq repo-dir (expand-file-name package pie-repos-directory))
    (if (eq build-type 'repo)
        (setq build-dir repo-dir)
      (setq build-dir (expand-file-name package pie-builds-directory)))
    (if lisp-dir
        (setq lisp-dir (expand-file-name lisp-dir build-dir))
      (setq lisp-dir build-dir))
    (when (functionp deps)
      (setq deps (funcall deps)))
    (setq pp (make-pie-package :name package
                               :package package
                               :url url
                               :rev rev
                               :backend backend
                               :branch branch
                               :depth depth
                               :build build
                               :deps deps
                               :repo-dir repo-dir
                               :build-dir build-dir
                               :build-type build-type
                               :lisp-dir lisp-dir
                               :ignore-files ignore-files))
    (pie--add-to-packages pp)))

(defsubst pie--use-package-concat (&rest elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (macroexp-progn (apply #'list (delete nil (delete (list nil) elems)))))

(defsubst pie--use-package-concat2 (elems)
  "Delete all empty lists from ELEMS (nil or (list nil)), and append them."
  (macroexp-progn (apply #'list (delete nil (delete (list nil) elems)))))

(defun pie--use-package-after (features &rest body)
  (cl-callf nreverse features)
  (let ((result (pie--use-package-concat2 body)))
    (dolist (f features)
      (setq result `(with-eval-after-load ',f ,result)))
    result))
;; (pie--use-package-after '(a b) '(setq ab 1) '(setq bc 2))

(defun pie--use-package-autoloads (autoloads package)
  (cl-callf nreverse autoloads)
  (let ((name (symbol-name package))
        result)
    (dolist (al autoloads)
      (push `(autoload ',al ,name nil t) result))
    (macroexp-progn result)))

(defun pie--use-package-load-path-and-pie (pie-load-path pie package-name)
  (let ((pie-name (cond
                   ((stringp pie) pie)
                   ((and (booleanp pie) pie)
                    (symbol-name package-name))
                   ((and (symbolp pie) (not (booleanp pie)))
                    (symbol-name pie)))))
    (if pie-load-path
        `(add-to-list 'load-path ,load-path)
      (if pie-name
          `(pie--install-package-by-name ,pie-name)
        (when (consp pie)
          `(when ,pie
             (pie--install-package-by-name ,(symbol-name package-name))))))))

(defun pie--use-package-init (init)
  (when init
    init))

(defun pie--use-package-config (config package-name)
  (when config
    `(with-eval-after-load ',package-name
       ,config)))

;;;###autoload
(cl-defmacro pie-use-package (package-name &key disabled when pie load-path init config autoloads after demand)
  "To configure a package by specifying a group of options."
  (declare (indent 1) (debug t))
  (when (and (booleanp disabled)
             (not disabled))
    (let ((autoloads-result (pie--use-package-autoloads autoloads package-name))
          (load-path-and-pie-result (pie--use-package-load-path-and-pie load-path pie package-name))
          (init-result (pie--use-package-init init))
          (demand-result (when demand `(require ',package-name)))
          (config-result (pie--use-package-config config package-name))
          (after-result))
      (if when
          (progn
            (if after
                (progn
                  (setq after-result (pie--use-package-after `,after init-result demand-result config-result))
                  `(when ,when
                     ,(pie--use-package-concat
                       load-path-and-pie-result
                       autoloads-result
                       after-result)))
              `(when ,when
                 ,(pie--use-package-concat
                   load-path-and-pie-result
                   autoloads-result
                   init-result
                   demand-result
                   config-result))))
        (if after
            (progn
              (setq after-result (pie--use-package-after `,after init-result demand-result config-result))
              (pie--use-package-concat
               load-path-and-pie-result
               autoloads-result
               after-result))
          (pie--use-package-concat
           load-path-and-pie-result
           autoloads-result
           init-result
           demand-result
           config-result))))))

(defun pie--installed-p (pp)
  "Check whether package PP has been installed.  PP is an instance of `pie-package'."
  (and pp
       (pie--fetched-p pp)
       (pie--built-p pp)))

(defun pie--fetched-p (pp)
  "Check whether package PP has been fetched.  PP is an instance of `pie-package'."
  (when pp
    (let ((dir (pie-package-repo-dir pp)))
      (and dir
           (file-directory-p dir)
           (not (directory-empty-p dir))))))

(defun pie--built-p (pp)
  "Check whether package PP has been built.  PP is an instance of `pie-package'."
  (when pp
    (let ((build-dir (pie-package-build-dir pp)))
      (and build-dir
           (file-directory-p build-dir)
           (not (directory-empty-p build-dir))))))

(defun pie--installed-by-name-p (name)
  "Check whether package with NAME has been fetched."
  (let ((pp (gethash name pie--packages)))
    (pie--installed-p pp)))

(defun pie--install-package-by-name (name)
  "Install package with name NAME."
  (let ((pp (gethash name pie--packages)))
    (if pp
        (pie--install-package pp)
      (user-error "No package named %s is defined" name))))

(defun pie--git-depth (&optional depth)
  "Determine git depth, use DEPTH if available."
  (let ((depth-str "")
        (depth depth))
    (cond
     ((eq depth t)
      ;; full history
      )
     ((and depth
           (> depth 0))
      (setq depth-str (format " --depth %d " depth)))
     (t
      (when (and pie-git-depth
                 (> pie-git-depth 0))
        (setq depth-str (format " --depth %d " pie-git-depth)))))
    depth-str))

(defun pie--dir-modification-time (dir)
  (time-convert (file-attribute-modification-time (file-attributes dir)) 'integer))

(defun pie--need-to-rebuild (src-dir build-dir)
  (and pie-rebuild-package-dwim
       (file-directory-p src-dir)
       (file-directory-p build-dir)
       (< (pie--dir-modification-time build-dir)
          (pie--dir-modification-time src-dir))))

(defun pie--build-package (pp &optional buildp)
  "Build package PP.  If BUILDP is t, build forcefully."
  (let ((dir (pie-package-repo-dir pp))
        (lisp-dir (pie-package-lisp-dir pp))
        (build-type (pie-package-build-type pp))
        (build-dir (pie-package-build-dir pp))
        (build (pie-package-build pp))
        (name (pie-package-package pp)))
    (when (not (member lisp-dir load-path))
      (add-to-list 'load-path lisp-dir))
    (when (and (pie--fetched-p pp)
               (or buildp
                   (not (pie--built-p pp))
                   (pie--need-to-rebuild dir build-dir)))
      (message "build package %s" name)
      (when (not (eq build-type 'repo))
        (delete-directory build-dir t)
        (make-directory build-dir t)
        (copy-directory dir build-dir t t t))
      (let ((default-directory build-dir))
        (if build
            (funcall build pp)
          (pie-default-build pp)))
      (message "Finish building %s" name))))

(defun pie--install-package (pp)
  "Install(fetch and then build) package PP."
  (let ((name (pie-package-package pp))
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
    (when pie-activite-package
      (pie--activate-package pp))
    (message "Finish installing %s" name)))

(defun pie--git-clone (url dir branch rev depth)
  "Use git to clone a repo from URL, save it to DIR.
If REV is specified, fetch that commit.
If BRANCH is specified, fetch that branch.
DEPTH determine how many commits will be cloned."
  (let (cmd
        (url (shell-quote-argument url))
        (dir (shell-quote-argument dir))
        (branch (when branch (shell-quote-argument branch)))
        (rev (when rev (shell-quote-argument rev))))
    (cond
     (rev
      (if branch
          (setq cmd (concat "git --no-pager clone  --branch " branch " " url " " dir))
        (setq cmd (concat "git --no-pager clone " url " " dir)))
      (when (zerop (call-process-shell-command cmd nil nil))
        (let ((default-directory dir))
          (call-process-shell-command (concat "git checkout " rev)))))
     (branch
      (setq cmd (concat "git --no-pager clone " (pie--git-depth depth) " --branch " branch " " url " " dir))
      (call-process-shell-command cmd nil nil))
     (t
      (setq cmd (concat "git --no-pager clone " (pie--git-depth depth) url " " dir))
      (call-process-shell-command cmd nil nil)))))

(defun pie--git-pull (dir)
  "Use git to clone a repo from URL, save it to DIR.
If REV is specified, fetch that commit. "
  (let (cmd
        (current-dir default-directory)
        (default-directory dir))
    (condition-case err
        (progn
          (cd dir)
          (setq cmd (concat "git pull"))
          (call-process-shell-command cmd nil nil)
          (cd current-dir))
      (error
       (message "Error when pulling package, %S" err)))))

(defun pie--download (url dir)
  "Download a file from URL to DIR."
  (let (name
        newname)
    (setq name (substring url (- (length url) (string-search "/" (reverse url)))))
    (setq newname (expand-file-name name dir))
    (url-copy-file url newname)))

(defun pie--fetch-package (pp)
  "Fetch package PP, where PP is an instance of `pie-package'."
  (let ((dir (pie-package-repo-dir pp))
        (url (pie-package-url pp))
        (rev (pie-package-rev pp))
        (backend (pie-package-backend pp))
        (branch (pie-package-branch pp))
        (depth (pie-package-depth pp))
        (name (pie-package-package pp)))
    (message "Start to fetch %s" name)
    (unless (file-exists-p dir)
      (make-directory dir t))
    (cond
     ((eq backend 'Git)
      (pie--git-clone url dir branch rev depth))
     ((eq backend 'http)
      (pie--download url dir))
     ((and (fboundp 'vc-clone)
           (member backend vc-handled-backends))
      (vc-clone url backend dir branch))
     (t
      (user-error "Unsupported backend %s" backend)))
    (if (not (pie--fetched-p pp))
        (warn "Failed to clone %s from %s" name url)
      (message "Finish fetching %s" name))))

(defun pie--pull-package (pp)
  "Pull package PP, where PP is an instance of `pie-package'."
  (let ((dir (pie-package-repo-dir pp))
        (url (pie-package-url pp))
        (rev (pie-package-rev pp))
        (backend (pie-package-backend pp))
        (name (pie-package-package pp)))
    (message "Start to pull %s" name)
    (unless (file-exists-p dir)
      (make-directory dir t))
    (cond
     ((and (eq backend 'Git)
           (null rev))
      (pie--git-pull dir))
     ((eq backend 'http)
      (pie--download url dir))
     ((and (fboundp 'vc-pull)
           (member backend vc-handled-backends))
      (let ((current-dir default-directory)
            (default-directory dir))
        (condition-case err
            (progn
              (cd dir)
              (vc-pull)
              (cd current-dir))
          (error
           (message "Error when pulling package, %S" err)))))
     (t
      (user-error "Unsupported backend %s" backend)))
    (if (not (pie--fetched-p pp))
        (warn "Failed to pull %s from %s" name url)
      (message "Finish pulling %s" name))))

(defun pie--generate-autoloads (dir output-file)
  (cond
   ((fboundp 'loaddefs-generate)
    (loaddefs-generate dir output-file))
   ((fboundp 'make-directory-autoloads)
    (make-directory-autoloads dir output-file))
   ((fboundp 'update-directory-autoloads)
    (let ((generated-autoload-file output-file))
      (update-directory-autoloads dir))))
  (when-let ((buf (find-buffer-visiting output-file)))
    (kill-buffer buf)))

(defun pie-empty-build (pp)
  "Do nothing.")

(defun pie-autoloads-build (pp)
  "Just generate autoloads files."
  (let* ((name (pie-package-package pp))
         (lisp-dir (pie-package-lisp-dir pp))
         (default-directory lisp-dir)
         (feature (concat name "-autoloads"))
         (autoloads (expand-file-name (concat feature ".el") lisp-dir)))
    (add-to-list 'load-path lisp-dir)
    (pie--generate-autoloads lisp-dir autoloads)))

(defun pie-default-build (pp)
  "Compile elisp files of PP."
  (let* ((name (pie-package-package pp))
         (lisp-dir (pie-package-lisp-dir pp))
         (default-directory lisp-dir)
         (files (directory-files lisp-dir t "\\.el$"))
         (ignore-files (append (list ".dir-locals.el") (pie-package-ignore-files pp)))
         (feature (concat name "-autoloads"))
         (autoloads (expand-file-name (concat feature ".el") lisp-dir)))
    (add-to-list 'load-path lisp-dir)
    (cl-dolist (file files)
      (when (not (member (file-name-nondirectory file) ignore-files))
        (byte-compile-file file)))
    (pie--generate-autoloads lisp-dir autoloads)))

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
            (remhash name pie--activate-cache)
            ;; first, clone the package to a tmp directory, then
            ;; delete the original directory and rename the tmp directory
            (setq dir (pie-package-repo-dir pp))
            (setq dir-tmp (concat dir "-tmp"))
            (setq pp-tmp (make-pie-package :name (pie-package-name pp)
                                           :package (pie-package-package pp)
                                           :url (pie-package-url pp)
                                           :rev (pie-package-rev pp)
                                           :backend (pie-package-backend pp)
                                           :depth (pie-package-depth pp)
                                           :branch (pie-package-branch pp)
                                           :build (pie-package-build pp)
                                           :deps (pie-package-deps pp)
                                           :repo-dir dir-tmp))
            (delete-directory dir-tmp t)
            (pie--fetch-package pp-tmp)
            (if (pie--fetched-p pp-tmp)
                (progn
                  (delete-directory dir t)
                  (rename-file dir-tmp dir)
                  ;; build
                  (pie--build-package pp t)
                  (when pie-activite-package
                    (pie--activate-package pp)))
              (warn "Failed to clone %s" name)))
        (user-error "No package named %s is defined" name)))))

;;;###autoload
(defun pie-update-package-simple ()
  "Update a package in `pie--packages'. Just pull the package, and then build it."
  (interactive)
  (let (name
        pp
        packages)
    (setq packages (hash-table-keys pie--packages))
    (setq name (completing-read "Package Name: " packages))
    (when name
      (setq pp (gethash name pie--packages))
      (if pp
          (progn
            (remhash name pie--activate-cache)
            (pie--pull-package pp)
            (if (pie--fetched-p pp)
                (progn
                  ;; build
                  (pie--build-package pp t)
                  (puthash name 0 pie--activate-cache)
                  (when pie-activite-package
                    (pie--activate-package pp)))
              (warn "Failed to pull %s" name)))
        (user-error "No package named %s is defined" name)))))

;;;###autoload
(defun pie-install-package ()
  "Install a package in `pie--packages'."
  (interactive)
  (let (name
        packages)
    (setq packages (hash-table-keys pie--packages))
    (setq name (completing-read "Package Name: " packages))
    (when name
      (pie--install-package-by-name name))))

;;;###autoload
(defun pie-rebuild-package ()
  "Rebuild the selected package."
  (interactive)
  (let (name
        pp
        packages)
    (setq packages (hash-table-keys pie--packages))
    (setq name (completing-read "Package Name: " packages))
    (when name
      (setq pp (gethash name pie--packages))
      (if pp
          (progn
            (pie--build-package pp t)
            (puthash name 0 pie--activate-cache)
            (when pie-activite-package
              (pie--activate-package pp)))
        (user-error "No package named %s is defined" name)))))

(defun pie--activate-package (pp)
  "Activate package PP."
  (let* ((name (pie-package-package pp))
         (cache (gethash name pie--activate-cache))
         (feature (concat name "-autoloads")))
    (when (and (or (null cache)
                   (= cache 0))
               (pie--built-p pp))
      (let* ((lisp-dir (pie-package-lisp-dir pp))
             (autoloads (expand-file-name (concat feature ".el") lisp-dir)))
        (ignore-errors (unload-feature (intern feature) t))
        (when (not (member lisp-dir load-path))
          (add-to-list 'load-path lisp-dir))
        (when (file-exists-p autoloads)
          (require (intern feature))))
      (puthash name 1 pie--activate-cache))))

;;;###autoload
(defun pie-install-packages ()
  "Fetch all packages in `pie--packages' if they have not been installed.
Called after the last `pie' invoking."
  (interactive)
  (condition-case err
      (progn
        (cl-dolist (pp (hash-table-values pie--packages))
          (pie--install-package pp))
        (message "All packages have been installed."))
    (error
     (message "Error when installing packages: %S" err))))


(provide 'pie)

;;; pie.el ends here
