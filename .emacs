;;; Windows set-up is a little bit different.
(setq winp (equal system-name "BELLEVUE"))


(global-set-key "\C-xg" (quote goto-line))
(global-set-key "\C-x\r\r" (quote compile))
(global-set-key [f7] (quote compare-windows))
(global-set-key [f5] (quote next-error))
(global-set-key [f6] (quote previous-error))
(global-set-key [f12] 'compile)

;;; backup files
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `((".*" . ,--backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      )

;;; Some text modes I want word wrap in, and kill to end of line should
;;; work as it normally does.
(defun my-rst-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1))
(add-hook 'rst-mode-hook #'my-rst-hook)

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(load-theme 'zenburn t)

;;; ----------------------------------------------------------------------
;;;
;;; Package system for Emacs
;;;
;;; ----------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;;;(add-to-list 'package-archives
;;;             '("melpa" . "http://melpa.org/packages/") t)
;;;(add-to-list 'package-archives
;;;             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (memq window-system '(mac ns))
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path (split-string (getenv "PATH") path-separator))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-ignored-extensions
   '(".hi" ".p_hi" ".o" ".p_o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".dyn_hi" ".dyn_hi-boot" ".dyn_o"))
 '(confirm-kill-emacs 'yes-or-no-p)
 '(exec-path
   '("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin"))
 '(gas-argument-column 22)
 '(git-commit-summary-max-length 50)
 '(global-ethan-wspace-mode t)
 '(haskell-indent-offset 2)
 '(haskell-interactive-mode-collapse nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-load-or-reload-prompt t)
 '(haskell-process-log t)
 '(haskell-process-path-cabal "~/.cabal/bin/cabal")
 '(haskell-process-path-ghci "/usr/local/bin/ghci-8.4.3")
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'cabal-repl)
 '(haskell-program-name "/usr/local/bin/ghci")
 '(haskell-tags-on-save nil)
 '(ignored-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(inferior-haskell-find-project-root t)
 '(menu-bar-mode nil)
 '(mode-require-final-newline nil)
 '(org-agenda-files '("~/projects/org/canada.org" "~/projects/org/company.org"))
 '(package-selected-packages
   '(rust-mode cargo-mode cargo zenburn-theme undo-tree org-pomodoro markdown-mode magit lua-mode haskell-mode go-rename go-guru go-autocomplete exec-path-from-shell ethan-wspace elm-mode))
 '(ps-landscape-mode t)
 '(ps-number-of-columns 2)
 '(ps-print-color-p t)
 '(ps-zebra-stripes t)
 '(safe-local-variable-values
   '((gas-comment-column . 40)
     (gas-argument-column . 24)
     (gas-opcode-column . 16)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(put 'downcase-region 'disabled nil)

;;; ----------------------------------------------------------------------
;;;
;;; Haskell
;;;
;;; ----------------------------------------------------------------------

;;; Turn off automatic indent, this is just annoying. It is not hard to
;;; press TAB to indent, but it is annoying when it changes indentation
;;; when you do not want. Imaging if everyone did this and had different
;;; ways to indent the code, it would be a mess to say the least...
;;; (Rant off)

;;(load "~/emacs/haskell-mode-20140206.757/haskell-mode-autoloads")
;; (load "~/emacs/haskell-mode/haskell-site-file")
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/haskell-mode-20140726.1852"))

(let ((haskell-load (expand-file-name "~/.emacs.d/elpa/haskell-mode-20170704.1445/haskell-mode-autoloads.el")))
  (if (file-exists-p haskell-load)
      (load haskell-load)
      (load "haskell-mode-autoloads.el")))
;;; (load (expand-file-name "~/.emacs.d/elpa/haskell-mode-20150814.1058/haskell-mode-autoloads.el"))

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)
;;(add-hook 'haskell-mode-hook
;;          (progn (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
;;                 (define-key haskell-mode-map (kbd "<f9>") 'haskell-mode-format-imports)))


;;; ----------------------------------------------------------------------
;;;
;;; Org-mode
;;;
;;; ----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'org-mode-hook 'visual-line-mode)
;;;(add-hook 'org-mode-hook (setq-default word-wrap t))  ;; <-- does not work??
;;;(add-hook 'org-mode-hook 'toggle-word-wrap)

(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;; ----------------------------------------------------------------------
;;;
;;; gas-mode has to be manually downloaded at the moment.
;;;
;;; ----------------------------------------------------------------------

;;; These variables are defined to avoid compliants about void variables
(defvar gas-changed nil)
(defvar gas-globals-cache nil)
(defvar gas-line-cache nil)
(defvar gas-local-comment-char nil)
(load (expand-file-name "~/.emacs.d/gas-mode.elc"))
(require 'gas-mode)

(setq auto-mode-alist
  (append
   ;; File name (within directory) starts with a dot.
   '(("\\.s\\'" . gas-mode))
   auto-mode-alist))

;;; ----------------------------------------------------------------------
;;;
;;; ido
;;;
;;; ----------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(setq ido-file-extensions-order '(".hs" ".org" ".rst" ".txt" ".py"
                                  ".emacs" ".c" ".cpp" ".xml" ".s"
                                  ".el"))
(setq ido-ignore-extensions t)

;;; Turn off the annoying auto-indent on RET that was introduced
;;; in emacs 24.4
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(display-time)

;;; ethan whitespace, use tabs in Makefiles
(defun makefile-tabs-are-less-evil ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
(add-hook 'makefile-mode-hook 'makefile-tabs-are-less-evil)


;;; ----------------------------------------------------------------------
;;;
;;; Go
;;;
;;; ----------------------------------------------------------------------

;;;(require 'go-mode)
;;;(require 'go-rename)
;;;(require 'go-guru)

;;;(setq gofmt-command "goimports")

(add-hook 'go-mode-hook
       (lambda ()
         (setq exec-path (cons (expand-file-name "~/go/bin") exec-path))
         ;;(setq compile-command "go build -v && go test -v && go vet")
         (setq compile-command "go install all")
         (define-key (current-local-map) "\C-c\C-c" 'compile)
         (add-hook 'before-save-hook 'gofmt-before-save)
         ;;(setq tab-width 4)
         ;;(setq indent-tabs-mode 1)
         (go-guru-hl-identifier-mode)
         ))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
