;;; math-delimiters.el --- Insert math delimiters in TeX, LaTeX and Org buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/oantolin/math-delimiters

;; This program is free software; you can redistribute it and/or modify
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

;; This package provides the command `math-delimiters-insert' to
;; insert TeX/LaTeX math delimiters. This command is meant to be bound
;; to $ in buffers where the user wants to use it. The author, for
;; example, binds it in `LaTeX-mode-map' and in `org-mode-map'.
;;
;; Note that the excellent `cdlatex' includes a `cdlatex-dollar'
;; command that it binds to $.  Users of both packages will probably
;; want to unbind $ in `cdlatex-mode-map'.
;;
;; The delimiters used for inline math and display math by
;; `math-delimiters-insert' are customizable, defaulting to \(...\) and
;; \[...\] respectively.  A command `math-delimiters-toggle' is
;; provided to quickly toggle between $...$ and \(...\) for inline
;; math and between $$...$$ and \[...\] for display math.
;;
;; Finally, a command `math-delimiters-no-dollars' is provided to
;; replace all $...$ and $$...$$ delimiters with \(...\) and \[...\],
;; respectively.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun math-delimiters-no-dollars ()
  "Convert math formulas in buffer from dollars to \\(\\) and \\[\\]."
  (interactive)
  (cl-flet ((replace-all (a b &optional c)
               (goto-char (point-min))
               (while (search-forward a nil t)
                 (replace-match b t t)
                 (when c
                   (search-forward a)
                   (replace-match c t t)))))
    (save-excursion
      (replace-all "\\$" "\\dollar ")
      (replace-all "$$" "\\[" "\\]")
      (replace-all "$" "\\(" "\\)")
      (replace-all "\\dollar " "\\$"))))

(defgroup math-delimiters nil
  "Delimiters for LaTeX math mode."
  :group 'tex)

(defcustom math-delimiters-inline
  '("\\(" . "\\)")
  "Delimiters to use for inline math mode."
  :group 'math-delimiters
  :type '(choice
          (const :tag "Parentheses" ("\\(" . "\\)"))
          (const :tag "Dollars" ("$" . "$"))
          (cons :tag "Other" string string)))

(defcustom math-delimiters-display
  '("\\[" . "\\]")
  "Delimiters to use for display math mode."
  :group 'math-delimiters
  :type '(choice
          (const :tag "Brackets" ("\\[" . "\\]"))
          (const :tag "Dollars" ("$$" . "$$"))
          (cons :tag "Other" string string)))

;;;###autoload
(defun math-delimiters-toggle (arg)
  "Toggle between $...$ and \\(...\\) for inline math.
When ARG is non-nil (interactively, if called with universal
prefix argument), also toggle the display math delimiters between
$$...$$ and \\[...\\]."
  (interactive "P")
  (setf math-delimiters-inline
        (if (equal math-delimiters-inline '("\\(" . "\\)"))
            '("$" . "$")
          '("\\(" . "\\)")))
  (when arg
    (setf math-delimiters-display
          (if (equal math-delimiters-display '("\\[" . "\\]"))
              '("$$" . "$$")
            '("\\[" . "\\]"))))
  (message "%sinline%s and %sdisplay%s"
           (car math-delimiters-inline)
           (cdr math-delimiters-inline)
           (car math-delimiters-display)
           (cdr math-delimiters-display)))

;;;###autoload
(defun math-delimiters-insert ()
  "Insert math delimiters.
If region is active surround it.  When repeated, toggle between
display and inline math.  Also toggle between display and inline
if called from inside empty math delimiters, or just after math
delimeters."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (save-excursion
             (beginning-of-line)
             (looking-at "^\\s-*#\\+TBLFM: ")))
      (insert "$")
    (let ((open (car math-delimiters-inline))
          (close (cdr math-delimiters-inline))
          (display-open (car math-delimiters-display))
          (display-close (cdr math-delimiters-display)))
      (cl-flet ((after-p (close)
                         (looking-back (regexp-quote close)
                                       (- (point) (length close))))
                (flip-after (from-open from-close to-open to-close)
                            (delete-char (- (length from-close)))
                            (insert to-close)
                            (let ((end (point)))
                              (backward-char (length to-close))
                              (search-backward from-open)
                              (delete-char (length from-open))
                              (insert to-open)
                              (goto-char (+ end (- (length to-open)
                                                   (length from-open))))))
                (middle-p (open close)
                          (and (looking-at (regexp-quote close))
                               (looking-back (regexp-quote open)
                                             (- (point) (length open)))))
                (flip-middle (from-open from-close to-open to-close)
                             (delete-char (- (length from-open)))
                             (delete-char (length from-close))
                             (insert to-open to-close)
                             (backward-char (length to-close))))
        (cond
         ((use-region-p)
          (let ((end (region-end)))
            (goto-char (region-beginning))
            (insert open)
            (goto-char (+ end (length open)))
            (insert close)))
         ((middle-p display-open display-close)
          (flip-middle display-open display-close open close))
         ((after-p display-close)
          (flip-after display-open display-close open close))
         ((middle-p open close)
          (flip-middle open close display-open display-close))
         ((after-p close)
          (flip-after open close display-open display-close))
         (t
          (insert open close)
          (backward-char (length close))))))))

(provide 'math-delimiters)
;;; math-delimiters.el ends here
