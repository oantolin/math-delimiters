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
;; By default, inline math is translated into display math with no
;; additional line breaks. This can be modified by setting
;; `math-delimiters-compressed-display-math' to `nil'. For example,
;; expression of the form \(...\) will then be translated to
;; \n\[\n...\n\]\n.
;;
;; When translating inline math to display math, often it is desirable
;; to include punctuation into the display math and exclude it from of
;; the inline math environment. For that purpose, the variable
;; `math-delimiters-include-characters' is available; it takes a list of
;; characters to act upon.  For example, setting it to (list ?.) and
;; then toggling to display math on the expression "\(\eta\)." would
;; turn it into "\[\eta.\]" .
;;
;; Finally, a command `math-delimiters-no-dollars' is provided to
;; replace all $...$ and $$...$$ delimiters with \(...\) and \[...\],
;; respectively.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun math-delimiters-no-dollars ()
  "Convert math formulas in buffer from dollars to \\(\\) and
\\=\\[\\]."
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

(defcustom math-delimiters-include-characters ",.;"
  "Characters to include in display math mode.
If any of them are right behind an inline math equation, slurp
them into the display math environment and barf them back out
when converting to inline math."
  :group 'math-delimiters
  :type 'string)

(defcustom math-delimiters-compressed-display-math t
  "Whether display math should be compressed.
A value of `t' means that, when transforming from inline to
display math, no additional line breaks are inserted.  For
example, we go from

    text-before \\(1 + 1\\) text-after

to

    text-before \\=\\[1 + 1\\] text-after.

A value of `nil', however, would switch between the above inline
math version and

    text-before
    \\=\\[
      1 + 1
    \\]
    text-after."
  :group 'math-delimiters
  :type 'boolean)

(defun math-delimiters--slurp-or-barf-characters (from-close)
  "Slurp or barf characters.
The characters considered are the ones in the string
`math-delimiters-include-characters'.  Thus, include these
characters in display math should they come right after an inline
math statement, and exclude them in the other direction.

The FROM-CLOSE parameter indicates the closing delimiter
currently at point and is used to decide whether to slurp or
barf."
  (if (equal from-close (cdr math-delimiters-inline)) ; from inline math
      (when (cl-find (char-after) math-delimiters-include-characters)
        (forward-char))
    (let ((orig-pos (point)))           ; from display math
      (skip-chars-backward " \t")
      (when (bolp) (backward-char 1) (skip-chars-backward " \t"))
      (cond ((cl-find (char-before) math-delimiters-include-characters)
             (backward-char))
            (math-delimiters-compressed-display-math
             (goto-char orig-pos))))))

(defun math-delimiters--toggle-line-breaks (open close)
  (let* ((to-display (equal (car math-delimiters-display) open)))
    (cl-flet ((toggle-close ()
                (if to-display
                    (progn
                      (newline-and-indent)
                      (search-backward close)
                      (newline-and-indent))
                  (join-line -1)
                  (join-line -1)
                  (backward-char (1+ (length close)))))
              (toggle-open ()
                (if to-display
                    (progn (newline-and-indent)
                           (forward-char (length open))
                           (newline-and-indent))
                  (join-line)
                  (join-line -1)
                  ;; If necessary, like when the inline delimiters are
                  ;; Dollars, fix `join-line's whitespace insertion.
                  (when (equal (char-after) ?\s)
                    (delete-char 1)))))
      (toggle-close)
      (search-backward open)
      (toggle-open)
      (search-forward close))))

;;;###autoload
(defun math-delimiters-toggle (arg)
  "Toggle between $...$ and \\(...\\) for inline math.
When ARG is non-nil (interactively, if called with universal
prefix argument), also toggle the display math delimiters between
$$...$$ and \\=\\[...\\]."
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
delimiters."
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
      (cl-flet* ((after-p (close)
                   (looking-back (regexp-quote close)
                                 (- (point) (length close))))
                 (flip-after (from-open from-close to-open to-close)
                   (delete-char (- (length from-close)))
                   (math-delimiters--slurp-or-barf-characters from-close)
                   (insert to-close)
                   (let ((end (point)))
                     (backward-char (length to-close))
                     (search-backward from-open)
                     (delete-char (length from-open))
                     (insert to-open)
                     (goto-char (+ end (- (length to-open)
                                          (length from-open)))))
                   (unless math-delimiters-compressed-display-math
                     (math-delimiters--toggle-line-breaks to-open to-close)))
                 (middle-p (open close)
                   (and (save-excursion
                          (skip-chars-forward " \t\n")
                          (looking-at (regexp-quote close)))
                        (save-excursion
                          (skip-chars-backward " \t\n")
                          (looking-back (regexp-quote open)
                                        (- (point) (length open))))))
                 (flip-middle (from-open from-close to-open to-close)
                   (search-forward from-close)
                   (flip-after from-open from-close to-open to-close)
                   (backward-char (length to-close))
                   (unless (or math-delimiters-compressed-display-math
                               (equal to-close close))
                     (beginning-of-line)
                     (backward-char 1))))
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
