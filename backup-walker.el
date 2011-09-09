;;; backup-walker.el --- quickly traverse all backups of a file

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: quickly traverse all backups of a file
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Wed Sep  7 19:38:05 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Sat Sep 10 02:53:55 2011 (+0800)
;;           By: Le Wang
;;     Update #: 91
;; URL: https://github.com/lewang/backup-walker
;; Keywords: backup
;; Compatibility: Emacs 23+

;;; Installation:

;;
;; add to ~/.emacs.el
;;
;;  (require 'backup-walker)
;;
;;   M-x backup-walker-start
;;
;; Of course, you should autoload, and bind the entry function to some key
;; sequence.  But the above gets you going.

;;; Commentary:

;; I never delete backups.  They are versioned in their own directory, happy
;; and safe.  My fingers skip to C-x C-s whenever I pause to think about
;; anything.  Even when I'm working with VCS, I save far more often than I
;; commit.
;;
;; This package helps me traverse those backups if I'm looking for something.
;;
;; The typical workflow is:
;;
;;   1) I'm in a buffer and realize I need to check some backups.
;;
;;        M-x backup-walker-start
;;
;;   2) I press <p> to go backwards in history until I see something
;;      interesting.  Then I press <enter> to bring it up.  OOPs this isn't
;;      it, I go back to the backup-walker window and find the right file.
;;
;;   3) I get what I need from the backup, go back to backup-walker, and press
;;      <q> and kill all open backups.
;;
;;   4) the end.
;;
;; Additionally, note that all the diff-mode facilities are available in the
;; `backup-walker' buffer.
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(provide 'backup-walker)

(require 'diff)

(or (fboundp 'diff-no-select)
    (defun diff-no-select (old new &optional switches no-async)
      (save-window-excursion (diff old new switches no-async))
      (get-buffer-create "*Diff*")))


(defvar backup-walker-ro-map (make-sparse-keymap))
(define-key backup-walker-ro-map [(n)] 'backup-walker-next)
(define-key backup-walker-ro-map [(p)] 'backup-walker-previous)
(define-key backup-walker-ro-map [(q)] 'backup-walker-quit)
(define-key backup-walker-ro-map [(return)] 'backup-walker-show-file-in-other-window)

(define-derived-mode backup-walker-mode diff-mode "{Diff backup walker}"
  "major mode for traversing versioned backups.  Use
  `backup-walker-start' as entry point."
  (run-hooks 'view-mode-hook)           ; diff-mode sets up this hook to
                                        ; remove its read-only overrides.
  (add-to-list 'minor-mode-overriding-map-alist `(buffer-read-only . ,backup-walker-ro-map)))


(defvar backup-walker-minor-mode nil "non-nil if backup walker minor mode is enabled")
(make-variable-buffer-local 'backup-walker-minor-mode)

(defun backup-walker-minor-mode (&optional arg)
  "purposefully made non-interactive, because this mode should only be used by code"
  (setq arg (cond  ((or (null arg)
                        (eq arg 'toggle))
                    (if backup-walker-minor-mode
                        nil
                      t))
                   ((> arg 0)
                    t)
                   ((<= arg 0)
                    nil)))
  (setq backup-walker-minor-mode arg)
  (force-mode-line-update)
  (if backup-walker-minor-mode
      (let ((index (cdr (assq :index backup-walker-data-alist)))
            (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist))))
        (setq header-line-format (backup-walker-get-key-help-common index suffixes))
        (add-to-list 'minor-mode-overriding-map-alist `(buffer-read-only . ,backup-walker-ro-map)))
    (setq header-line-format nil)
    (delq `(buffer-read-only . ,backup-walker-ro-map) minor-mode-overriding-map-alist))
  backup-walker-minor-mode)

(add-minor-mode 'backup-walker-minor-mode " walker" nil nil nil)

(defvar backup-walker-data-alist nil
  "")
(make-variable-buffer-local 'backup-walker-data-alist)

(defsubst backup-walker-get-version (fn &optional start)
  "return version number given backup"
  (if start
      (string-to-int
       (substring fn
                  (string-match "[[:digit:]]+" fn start)
                  (match-end 0)))
    (backup-walker-get-version fn (length (file-name-sans-versions fn)))))

(defun backup-walker-get-sorted-backups (filename)
  "Return version sorted list of backups of the form:

  (prefix (list of suffixes))"
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (file-name-sans-versions
                    (make-backup-file-name (expand-file-name filename))))
         (file (file-name-nondirectory filename))
         (dir  (file-name-directory    filename))
         (comp (file-name-all-completions file dir))
         (prefix-len (length file)))
    (cons filename (mapcar
                    (lambda (f)
                      (substring (cdr f) prefix-len))
                    (sort (mapcar (lambda (f)
                                    (cons (backup-walker-get-version f prefix-len)
                                          f))
                                  comp)
                          (lambda (f1 f2)
                            (not (< (car f1) (car f2)))))))))


(defsubst backup-walker-get-key-help-common (index suffixes)
  (concat
   (if (eq index 0)
       ""
     (concat (propertize "<n>" 'face 'italic)
             " ~"
             (propertize (int-to-string (backup-walker-get-version (nth (1- index) suffixes)))
                         'face 'font-lock-keyword-face)
             "~ "))
   (if (nth (1+ index) suffixes)
       (concat (propertize "<p>" 'face 'italic)
               " ~"
               (propertize (int-to-string
                            (backup-walker-get-version (nth (1+ index) suffixes)))
                           'face 'font-lock-keyword-face)
               "~ ")
     "")
   (propertize "<q>" 'face 'italic)
   " quit "))

(defun backup-walker-refresh ()
  (let* ((index (cdr (assq :index backup-walker-data-alist)))
         (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
         (prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (right-file (concat prefix (nth index suffixes)))
         (right-version (format "%i" (backup-walker-get-version right-file)))
         diff-buf left-file left-version)
    (if (eq index 0)
        (setq left-file (cdr (assq :original-file backup-walker-data-alist))
              left-version "orig")
      (setq left-file (concat prefix (nth (1- index) suffixes))
            left-version (format "%i" (backup-walker-get-version left-file))))
    (setq diff-buf (diff-no-select left-file right-file nil 'noasync))
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert-buffer diff-buf)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (force-mode-line-update)
    (setq header-line-format
          (concat (format "{{ ~%s~ → ~%s~ }} "
                          (propertize left-version 'face 'font-lock-variable-name-face)
                          (propertize right-version 'face 'font-lock-variable-name-face))
                  (backup-walker-get-key-help-common index suffixes)
                  (propertize "<return>" 'face 'italic)
                  " open ~"
                  (propertize (propertize (int-to-string (backup-walker-get-version right-file))
                                          'face 'font-lock-keyword-face))
                  "~"))
    (kill-buffer diff-buf)))

;;;###autoload
(defun backup-walker-start (original-file)
  "start walking with the latest backup

with universal arg, ask for a file-name."
  (interactive (list (if (and current-prefix-arg (listp current-prefix-arg))
                         (read-file-name
                          "Original file: "
                          nil
                          buffer-file-name
                          t
                          (file-name-nondirectory buffer-file-name))
                       (or buffer-file-name
                           (error "buffer has no file name")))))
  (unless (and version-control
               (not (eq version-control 'never)))
    (error "version-control must be enabled for backup-walker to function."))
  (let ((backups (backup-walker-get-sorted-backups original-file))
        alist
        buf)
    (if (null (cdr backups))
        (error "no backups found.")
      (push `(:backup-prefix . ,(car backups)) alist)
      (push `(:backup-suffix-list . ,(cdr backups)) alist)
      (push `(:original-file . ,original-file) alist)
      (push `(:index . 0) alist)
      (setq buf (get-buffer-create (format "*Walking: %s*" (buffer-name))))
      (with-current-buffer buf
        (backup-walker-mode)
        (buffer-disable-undo)
        (setq backup-walker-data-alist alist)
        (backup-walker-refresh))
      (pop-to-buffer buf))))

(defun backup-walker-next (arg)
  "move to a more recent backup
with ARG, move ARG times"
  (interactive "p")
  (cond ((< arg 0)
         (backup-walker-previous (- arg)))
        ((> arg 0)
         (let* ((index-cons (assq :index backup-walker-data-alist))
                (index (cdr index-cons))
                (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
                (new-index (- index arg)))
           (if (< new-index 0)
               (signal 'args-out-of-range (list (format "not enough newer backups, max is %i" index)))
             (backup-walker-move index-cons index suffixes new-index))))))

(defun backup-walker-previous (arg)
  "move to a less recent backup
with ARG move ARG times"
  (interactive "p")
  (cond ((< arg 0)
         (backup-walker-next (- arg)))
        ((> arg 0)
         (let* ((index-cons (assq :index backup-walker-data-alist))
                (index (cdr index-cons))
                (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
                (max-movement (- (1- (length suffixes)) index))
                (new-index (+ index arg)))
           (if (> arg max-movement)
               (signal 'args-out-of-range (list (format "not enough older backups, max is %i" max-movement)))
             (backup-walker-move index-cons index suffixes new-index))))))

;; TODO: We can actually compute the correct new point by diffing and
;;       interpreting results.  So far, it seems overkill.

(defsubst backup-walker-move (index-cons index suffixes new-index)
"internal function used by backup-walker-{next,previous}"
(cond
 ((eq major-mode 'backup-walker-mode)
  (setcdr index-cons new-index)
  (backup-walker-refresh))
 (backup-walker-minor-mode
  (let* ((prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (file-name (concat prefix (nth new-index suffixes)))
         (alist (purecopy backup-walker-data-alist))
         (saved-column (current-column))
         (saved-line (count-lines (point-min) (point)))
         (buf (find-file-noselect file-name)))
    (setcdr (assq :index alist) new-index)
    (with-current-buffer buf
      (setq backup-walker-data-alist alist)
      (backup-walker-minor-mode 1)
      (goto-char (point-at-bol saved-line))
      (move-to-column saved-column))
    (set-window-buffer nil buf)))))

(defun backup-walker-show-file-in-other-window ()
  "open the current backup in another window.

Only call this function interactively."
  (interactive)
  (unless (eq major-mode 'backup-walker-mode)
    (error "this is not a backup walker buffer."))
  (let* ((index (cdr (assq :index backup-walker-data-alist)))
         (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
         (prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (file-name (concat prefix (nth index suffixes)))
         (walking-buf (current-buffer))
         (alist (purecopy backup-walker-data-alist))
         (buf (find-file-noselect file-name)))
    (with-current-buffer buf
      (setq backup-walker-data-alist alist)
      (push `(:walking-buf . ,walking-buf) backup-walker-data-alist)
      (backup-walker-minor-mode 1))
    (pop-to-buffer buf)))

(defun backup-walker-blame (line)
  "find out where a certain line came into existance

show the backup *JUST BEFORE* this line was born, since that is
usually more interesting."
  (interactive (list (read-string "line: " nil 'backup-walker-blame)))
  (cond
   (backup-walker-minor-mode
    (let* ((my-index (cdr (assq :index backup-walker-data-alist)))
           (walking-buf (cdr (assq :walking-buf backup-walker-data-alist))))
      (with-current-buffer walking-buf
        (setcdr (assq :index backup-walker-data-alist) my-index)
        (backup-walker-refresh))
      (pop-to-buffer walking-buf)
      (backup-walker-blame line)))
   ((eq major-mode 'backup-walker-mode)
    (let* ((index-cons (assq :index backup-walker-data-alist))
           (old-index (cdr index-cons))
           (is-unified (member "-u" diff-switches))
           (search-str (format "-%s" line))
           found)
      (condition-case err
          (while (not found)
            (let ((suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
                  (index (cdr (assq :index backup-walker-data-alist))))
              (goto-char (point-min))
              (when (not is-unified)
                (diff-context->unified (point-min) (point-max)))
              (message "searching %s" (nth index suffixes))
              (if (search-forward search-str nil t)
                  (setq found t)
                (backup-walker-previous 1))))
        ('args-out-of-range
         (setcdr index-cons old-index)
         (backup-walker-refresh)
         (message "input was not found in backup history")))))
   (t
    (error "not sure what you want me to do."))))

(defun backup-walker-quit (arg)
  "quit backup-walker session.

Offer to kill all associated backup buffers.

with ARG, also kill the walking buffer"
  (interactive "P")
  (cond (backup-walker-minor-mode
         (pop-to-buffer (cdr (assq :walking-buf backup-walker-data-alist))))
        ((eq major-mode 'backup-walker-mode)
         (let* ((prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
                (prefix-len (length prefix))
                (walking-buf (current-buffer))
                backup-bufs)
           (mapc (lambda (buf)
                   (let ((file-name (buffer-file-name buf)))
                     (when (and file-name
                                (>= (length file-name) prefix-len)
                                (string= prefix (substring file-name 0 prefix-len)))
                       (push buf backup-bufs))))
                 (buffer-list))
           (when (and (not (eq 0 (length backup-bufs)))
                      (y-or-n-p (concat (propertize (int-to-string (length backup-bufs))
                                                    'face 'highlight)
                                        " backup buffers found, kill them?")))
             (mapc (lambda (buf)
                     (kill-buffer buf))
                   backup-bufs))
           (quit-window)
           (when (and arg
                      (listp arg))
             (kill-buffer walking-buf))))
        (t
         (error "I don't know how to quit you."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backup-walker.el ends here
