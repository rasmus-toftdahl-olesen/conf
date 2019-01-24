;;; First thing to do is set up the package repositories
;;; Skip down to the "INSTALL DESIRED" part for an easy way to install the desired packages

(require 'package) ;; You might already have this line

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
(setq w32-get-true-file-attributes nil)

;;(byte-recompile-directory (expand-file-name "~/.emacs.d"))
;;(extend-load-path "~/.emacs.d")

(when (not package-archive-contents)
  (package-refresh-contents))

(setq desired-packages '(csharp-mode
                         cmake-mode
                         better-defaults
                         magit
                         findr
                         editorconfig
                         iss-mode
                         p4
                         yaml-mode
                         elpy
                         flycheck
                         py-autopep8
                         org
                         atomic-chrome))

;; Missing package for: qmake-mode epg

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      desired-packages)

(require 'better-defaults)

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "RET") 'newline-and-indent)
(defun exit-emacs ()
  "Save buffers and exit emacs (does what C-x C-c would normally do)."
  (interactive)
  (save-buffers-kill-terminal))

;;; From
;;; http://whattheemacsd.com//buffer-defuns.el-01.html
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
before-save-hook, and that might be bad."
  (interactive)
  ;(if (not (eq "makefile-gmake-mode" major-mode))
      ;(progn
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)
;(remove-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
 "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(require 'iss-mode nil t)
(require 'apt-utils nil t)
;;(require 'html-helper-mode nil t)
(require 'automation-mode nil t)
(require 'epg nil t)
(require 'vc-bzr nil t)
(require 'qmake-mode nil t)
                                        ;(if (require 'undo-tree nil t)
                                        ;    (progn
                                        ;      (global-set-key (kbd "C-_") 'undo-tree-undo)
                                        ;      (global-set-key (kbd "C-x u") 'undo-tree-visualize)))

(require 'cc-mode)
(require 'cmake-mode nil t)
(require 'editorconfig)
(require 'p4 nil t)
(load-theme 'tango-dark)
(custom-theme-set-faces
 'tango-dark
 '(font-lock-comment-face ((t (:foreground "#ff4b4b")))))

(setq win32 (string= system-type "windows-nt"))

(prefer-coding-system 'utf-8)

(which-function-mode)

(setq line-number-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hrs-format t)
(setq visible-bell t)
(display-time)

(setq dabbrev-case-fold-search nil)
(global-set-key "\M- " 'dabbrev-expand)

(global-set-key (kbd "M-o") 'ace-window)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

(add-to-list 'auto-mode-alist '("\\.szs\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.SeqZapProject\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.iss\\'" . iss-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.forge\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.autopilot\\'" . python-mode))

(if win32
    (progn
      (add-hook 'sh-mode-hook
                '(lambda () (sh-set-shell "cmd")))
      (set-face-font 'default "Consolas-10")))

                                        ; Use flyspell in html-mode
(add-hook 'html-mode-hook
          '(lambda () (flyspell-mode)))

(setq tab-width 3)
(setq default-tab-width 3)

(setq font-lock-maximum-decoration t)
(setq-default font-lock-maximum-size nil)

(setq w3-default-homepage "http://halfdans.net")
                                        ;(add-hook 'text-mode-hook 'auto-fill-mode)
                                        ;(add-hook 'text-mode-hook 'flyspell-mode)

(defun my-java-project-startup ()
  (setq c-basic-offset 3))


(progn
  (setq tab-width 4)
  (setq default-tab-width 4)
  (setq-default tab-width 4)
  (setq-default c-basic-offset 4)

  (setq c-site-default-style "k&r")
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq-default indent-tabs-mode nil)
  (c-set-offset 'statement-cont 4)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0))

(if (not win32)
    (progn
      (add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)

      (defun my-rcirc-print-hook (process sender response target text)
        (when (and (string-match (rcirc-nick process) text)
                   (not (string= (rcirc-nick process) sender))
                   (not (string= (rcirc-server-name process) sender)))
          (start-process "notify" nil "notify-send" (concat "rcirc: " sender) text)))

      (setq browse-url-browser-function (quote browse-url-generic)
            browse-url-generic-program "chromium-browser"
            debian-changelog-mailing-address "halfdan@halfdans.net"))

  (progn
    (add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)

    (defun my-rcirc-print-hook (process sender response target text)
      (when (and (string-match (rcirc-nick process) text)
                 (not (string= (rcirc-nick process) sender))
                 (not (string= (rcirc-server-name process) sender)))
        (start-process "notify" nil "WinNotify" (concat "rcirc: " sender) text)))))

;; Enhanced syntax highlighting
;; Currently support for []|&!.+=-/%*,()<>{}
(font-lock-add-keywords
 'c++-mode '(("\\(\\[\\|\\]\\|[|!\\.\\+\\=\\&]\\|-\\|\\/\\|\\%\\|\\*\\|,\\|(\\|)\\|>\\ |<\\|{\\|}\\)" 1 font-lock-operator-face )
             ("\\(;\\)" 1 font-lock-end-statement )))

(make-face 'font-lock-operator-face)
(make-face 'font-lock-end-statement)
(setq font-lock-operator-face 'font-lock-operator-face)
(setq font-lock-end-statement 'font-lock-end-statement)

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (and transient-mark-mode mark-active)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(global-set-key (kbd "C-3") 'comment-or-uncomment-current-line-or-region)

(add-hook 'latex-mode-hook (lambda ()
                             (flyspell-mode)))

;;(eval-after-load "outline" '(progn (require 'outline-plus)))
;;(require 'outline-plus)

(global-set-key [(mouse-4)] 'down-slightly)
(global-set-key [(mouse-5)] 'up-slightly)
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))

(defun halfdan-eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'halfdan-eval-end-replace)

(defadvice rcirc-format-response-string (after dim-entire-line)
  "Dim whole line for senders whose nick matches `rcirc-dim-nicks'."
  (when (and rcirc-dim-nicks sender
             (string-match (regexp-opt rcirc-dim-nicks 'words) sender))
    (setq ad-return-value (rcirc-facify ad-return-value 'rcirc-dim-nick))))
(ad-activate 'rcirc-format-response-string)

(setq rcirc-dim-nicks '("dpkg")
      rcirc-startup-channels-alist (quote (("^irc.freenode.net$")))) ; "#debian" "#rcirc"))))

;; ~/.emacs.private should contain something like:
;;           (setq rcirc-authinfo '(("irc.freenode.net" nickserv "<nick>" "<password>")))
(if (file-exists-p "~/.emacs.private")
    (load-file "~/.emacs.private"))

;; (defun trac-wiki-auto-mode-function ()
;;   (if (and (eq major-mode 'text-mode)
;;            (member (file-name-extension (buffer-file-name))
;;                    '("txt" "wiki"))
;;            (re-search-forward "^=+ [^=\n]+ =+\\s *$" nil t))
;;       (tracwiki-mode)))

;; (add-hook 'find-file-hook
;;           'trac-wiki-auto-mode-function)

(require 're-builder)
(setq reb-re-syntax 'string)

(defun xml-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun xml-pretty-print-xml ()
    (xml-pretty-print-xml-region 0 (buffer-size)))

(require 'atomic-chrome)
(atomic-chrome-start-server)

;;;
;;; Python
;;;

; Use elpy
(elpy-enable)
; Combine elpy and flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
; Add py-autopep8 to elpy
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;;
;;; Work
;;;
; Use alternative theme on my windows work laptop to make it easy to
; distinguish it when alt-tab'ing
(if (string= (system-name) "DK-C-KBN-RTOL-1")
    (load-theme 'leuven t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csharp-want-flymake-fixup nil)
 '(csharp-want-imenu nil)
 '(csharp-want-yasnippet-fixup nil)
 '(debian-changelog-mailing-address "halfdan@halfdans.net" t)
 '(display-time-mode t)
 '(findr-skip-file-regexp "^[#\\.]|^.*~")
 '(font-lock-global-modes t)
 '(fortran-tab-mode-default nil)
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(matlab-case-level (quote (4 . 4)))
 '(matlab-fill-code nil)
 '(org-agenda-files (quote ("c:/Users/rtol/Dropbox/Rasmus/todo.org")))
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (ace-window atomic-chrome yaml-mode p4 magit iss-mode findr editorconfig csharp-mode cmake-mode)))
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8))))
 '(show-paren-mode t)
 '(starttls-extra-arguments (quote ("--insecure")))
 '(tags-add-tables (quote ask-user))
 '(visible-bell t))

(if win32
    (progn
                                        ; Emacs sets HOME to %HOMEPATH%\AppData\Roaming for some reason
                                        ;(setenv "HOME" (getenv "HOMEPATH"))
      (custom-set-variables
       '(archive-zipn-extract (quote ("c:\\Program files (x86)\\7-zip\\7z.exe" "e" "-so"))))))

(defun rtol-open-todo ()
    (interactive)(find-file (car org-agenda-files)))

(defun rtol-open-todo-tree ()
  (interactive)
  (rtol-open-todo)
  (call-interactively 'org-show-todo-tree))

(global-set-key (kbd "<f2>") 'rtol-open-todo)
(global-set-key (kbd "<f3>") 'rtol-open-todo-tree)

(server-start)
