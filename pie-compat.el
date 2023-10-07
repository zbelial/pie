;;; pie.el --- Package installer for Emacs -*- lexical-binding: t -*-

(when (not (fboundp #'directory-empty-p))
  (defun directory-empty-p (dir)
    "Return t if DIR names an existing directory containing no other files.
Return nil if DIR does not name a directory, or if there was
trouble determining whether DIR is a directory or empty.

Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks."
    (and (file-directory-p dir)
         (null (directory-files dir nil directory-files-no-dot-files-regexp t)))))

(when (not (fboundp #'make-directory-autoloads))
  (defun make-directory-autoloads (dir output-file)
    (let ((generated-autoload-file output-file))
      (update-directory-autoloads dir))))

(provide 'pie-compat)
