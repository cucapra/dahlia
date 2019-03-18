;;; packages.el --- custom layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sam Thomas <samthomas@samthomas>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:

;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `custom-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom/pre-init-PACKAGE' and/or
;;   `custom/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defvar fuse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `fuse-mode'.")

(defvar fuse-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comments
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; strings
    ;; (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table for `fuse-mode'.")

(setq fuse-font-lock-keywords
  (let* (
         (fuse-keywords '("decl" "def" "extern" "bank" "unroll" "shrink" "record" "let"
                          "if" "else" "while" "combine" "for"))
         (fuse-types '("bit" "float" "bool" "view"))
         (fuse-bools '("true" "false" "null"))

         (fuse-keywords-regexp (regexp-opt fuse-keywords 'words))
         (fuse-types-regexp (regexp-opt fuse-types 'words))
         (fuse-bools-regexp (regexp-opt fuse-bools 'word)))

    `(
      (,fuse-keywords-regexp . (1 font-lock-keyword-face))
      (,fuse-types-regexp . (1 font-lock-type-face))
      (,fuse-bools-regexp . (1 font-lock-constant-face))
      )))

 ;;; Indentation

(defvar fuse-indent-level 2)

(defun fuse-count-back ()
  (let ((count 0)
        (not-top t))
    (save-excursion
      (end-of-line)
      (forward-char -1)
      (if (looking-at "{")
          (forward-char -1))
      (while not-top
        (if (looking-at "}")
            (setq count (- count 1)))
        (if (looking-at "{")
            (setq count (+ count 1)))
        (forward-char -1)
        (if (bobp)
            (setq not-top nil)))
      count)))

(defun fuse-print-back ()
  (interactive)
  (message "Back: %s" (fuse-count-back)))

(defun fuse-indent-line ()
  (interactive)
  (end-of-line)
  (indent-line-to (* fuse-indent-level (fuse-count-back))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fuse\\'" . fuse-mode))
(add-to-list 'auto-mode-alist '("\\.ifuse\\'" . fuse-mode))

(define-derived-mode fuse-mode prog-mode "Fuse Mode"
  "A major mode for editing Fuse source files."
  :syntax-table fuse-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local font-lock-defaults
              '((fuse-font-lock-keywords)))
  (setq-local indent-line-function 'fuse-indent-line))

(provide 'fuse-mode)

;;; packages.el ends here
