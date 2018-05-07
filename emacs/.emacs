
;; Basic settings

(require 'ido)

(column-number-mode 1)
(line-number-mode 1)
;;(global-linum-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(ido-mode t)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Ident with spaces
(setq-default indent-tabs-mode nil)


;; Show trailing whitespaces

;; (require 'whitespace)

;; (setq whitespace-line-column 80) ;; limit line length
;; (setq whitespace-style '(face lines-tail))
;(setq whitespace-style '(face))

;(add-hook 'prog-mode-hook 'whitespace-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-coq-disabled-features
   '(hello unicode-math-backend block-end-backend reserved-keywords-backend refman-ltac-abbrevs-backend refman-tactic-abbrevs-backend refman-vernac-abbrevs-backend refman-scope-abbrevs-backend pg-backend context-backend modules-backend local-definitions-backend search-results-backend dynamic-tactics-backend))
 '(coq-indent-modulestart 0)
 '(coq-indent-proofstart 0)
 '(custom-safe-themes
   '("f9aa7cd27af5ee129f4dc1e784832ea05a887d04c5b6f0876158650c7302496d" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" "460842488c29d2fabd5a6ad7d12e46463f40a3b870dd8a4943fabb3a4e292efa" "c08a77fa2743c188a826967f1159097e3f0e1a64eb11e1d4de93826e6e435090" "ff7fe28d5cb7dd1ea63568f5e555e07b94c0ab70e851a883763e54dba4532fce" "f1f14f52b02151116f250a5d62a54effdc672a235137872484aa45fab29d6f23" default))
 '(fci-rule-color "#383838")
 '(notmuch-saved-searches
   '((:name "new" :query "tag:inbox and tag:unread" :key "n")
     (:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")))
 '(notmuch-search-oldest-first nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(company-coq redprl notmuch rainbow-delimiters undo-tree highlight-symbol magit auctex haskell-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(proof-splash-time 0)
 '(proof-strict-read-only 'retract)
 '(proof-three-window-enable nil)
 '(send-mail-function 'smtpmail-send-it)
 '(show-trailing-whitespace t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))

;; Backup settings

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Custom commands

(global-set-key (kbd "C-x 8") (lambda () (interactive) (split-window-right 90)))
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") (lambda () (interactive) (setq frame-title-format '((:eval default-directory)))))

;; Package settings

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; Agda

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
;; (add-hook 'agda2-mode-hook
;;           (lambda () (local-set-key (kbd "C-c <C-enter>") 'agda2-load)))
(eval-after-load 'agda2-mode
                 '(define-key agda2-mode-map [(control c) (control return)] 'agda2-load))
;; TODO add C-c C-enter binding to C-c C-l

;; Recompilation binding

(global-set-key (kbd "C-c c") 'compile-again)

(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
 (interactive "p")
 (if (and (eq pfx 1)
	  compilation-last-buffer)
     (progn
       (set-buffer compilation-last-buffer)
       (revert-buffer t t))
   (call-interactively 'compile)))


;; Hide finished compilations

  ;; (defun compilation-exit-autoclose (status code msg)
  ;;   (when (and (eq status 'exit) (zerop code))
  ;;     (bury-buffer)
  ;;     (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;;   (cons msg code))
  ;; (setq compilation-exit-message-function 'compilation-exit-autoclose)


;; Theme settings

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

;; TeX settings

(eval-after-load "tex"
  '(progn
     (setq TeX-view-program-list '(("Apvlv" "apvlv %o")))
     ;(add-to-list 'TeX-command-list '("View" "evince %g" TeX-run-command nil t :help "Run evince on your document"))
     (setq TeX-view-program-selection '((output-pdf "Apvlv")))))


;; Linum theme

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :background "#666" :foreground "#aa4")))))
    (setq linum-format
          (lambda (line)
            (propertize (format
                         (let ((w (length (number-to-string
                                           (count-lines (point-min) (point-max))))))
                           (concat "%" (number-to-string w) "d "))
                         line)
                        'face 'linum)))


;; Custom variables


;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

;; Open .v files with Proof General's Coq mode
(load "~/projects/PG/generic/proof-site")

    ;; ;; Ssreflect support
    ;; (load-file "~/projects/math-comp-mathcomp-1.6.1/mathcomp/ssreflect/pg-ssr.el")

(require 'haskell-mode)


;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)
(put 'company-coq-fold 'disabled nil)

(autoload 'notmuch "notmuch" "notmuch mail" t)

(load-theme 'zenburn t)

(push ".agdai" completion-ignored-extensions)
(push ".dyn_hi" completion-ignored-extensions)
(push ".dyn_o" completion-ignored-extensions)
(push ".v.d" completion-ignored-extensions)
(push ".vo" completion-ignored-extensions)
(push ".aux" completion-ignored-extensions)
(push ".glob" completion-ignored-extensions)

;; Puts the directory with the Agda input method on the load path,
;; among other things.
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Loads the Agda input method.
(require 'agda-input)

;; Enables the Agda input method when text mode is activated.
;; (add-hook 'text-mode-hook
;;           (lambda nil (set-input-method "Agda")))
