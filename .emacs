(defun extend-list (list element)
  "Add an element to a list"
  (setq list (cons element list)))

(defun extend-load-path (element)
  "Use this function to extend the load-path with ELEMENT."
  (setq load-path (cons (expand-file-name element) load-path)))

(byte-recompile-directory (expand-file-name "~/.emacs.d"))
(extend-load-path "~/.emacs.d")
(extend-load-path "~/.emacs.d/el-get/el-get")
(extend-load-path "~/.emacs.d/el-get/package")
(extend-load-path "~/.emacs.d/el-get/rcirc-groups")
(extend-load-path "~/.emacs.d/el-get/csharp-mode")

(setq el-get-sources
      '((:name el-get)
        (:name package)
        (:name css-mode :type elpa)
        (:name findr :type elpa)
        (:name rcirc-groups
               :type emacswiki
               :url "rcirc-groups.el"
               )
        (:name csharp-mode
               :type svn
               :url "http://csharpmode.googlecode.com/svn/trunk/"
               )
        ))

(require 'el-get nil t)
(if (require 'package nil t)
    (package-initialize))

(require 'css-mode nil t)
(require 'findr nil t)
(require 'csharp-mode nil t)
(require 'php-mode nil t)
(require 'html-helper-mode nil t)
(require 'iss-mode nil t)
(require 'cmake-mode nil t)
(require 'automation-mode nil t)
(require 'epg nil t)
(require 'vc-bzr nil t)
(require 'apt-utils nil t)

(require 'cc-mode)

(setq win32 (string= system-type "windows-nt"))
(setq sequanto (string= system-name "RTO"))

(prefer-coding-system 'utf-8)

(tool-bar-mode)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(setq line-number-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hrs-format t)
(setq visible-bell t)
(show-paren-mode 1)
(display-time)

(setq dabbrev-case-fold-search nil)
(global-set-key "\M- " 'dabbrev-expand)

(add-to-list 'auto-mode-alist '("\\.szs\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.SeqZapProject\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.iss\\'" . iss-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

                                        ; Use flyspell in html-mode
(add-hook 'html-mode-hook
          '(lambda () (flyspell-mode)))

                                        ; vtl-mode
(autoload 'turn-on-vtl-mode "vtl" nil t)
(add-hook 'html-mode-hook 'turn-on-vtl-mode t t)
(add-hook 'xml-mode-hook 'turn-on-vtl-mode t t)
(add-hook 'text-mode-hook 'turn-on-vtl-mode t t)

(setq tab-width 3)
(setq default-tab-width 3)

(setq font-lock-maximum-decoration t)
(setq-default font-lock-maximum-size nil)

(setq w3-default-homepage "http://halfdans.net")
                                        ;(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(setq dabbrev-case-fold-search nil)
(global-set-key "\M- " 'dabbrev-expand)

(add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
(defun my-rcirc-print-hook (process sender response target text)
  (when (and (string-match (rcirc-nick process) text)
             (not (string= (rcirc-nick process) sender))
             (not (string= (rcirc-server-name process) sender)))
    (start-process "notify" nil "notify-send" (concat "rcirc: " sender) text)))

(if (require 'mk-project nil t)
    (progn
      (project-def "seqzap.com-trunk"
                   '((basedir          "c:/work/web/seqzap.com/trunk/")
                     (src-patterns     ("*.html"))
                     (ignore-patterns  ("build"))
                     (tags-file        "~/.mk-project/seqzap.com-trunk/TAGS")
                     (file-list-cache  "~/.mk-project/seqzap.com-trunk/files")
                     (open-files-cache "~/.mk-project/seqzap.com-trunk/open-files")
                     (vcs              svn)
                     (compile-cmd      "generate.py")
                     (ack-args         "")
                     (startup-hook     nil)
                     (shutdown-hook    nil)))

      (project-def "activation-server"
                   '((basedir          "c:/work/web/activation_sequanto_com/")
                     (src-patterns     ("*.py" "*.html"))
                                        ;               (ignore-patterns  ("build"))
                     (tags-file        "~/.mk-project/activation-server/TAGS")
                     (file-list-cache  "~/.mk-project/activation-server/files")
                     (open-files-cache "~/.mk-project/activation-server/open-files")
                     (vcs              svn)
                                        ;               (compile-cmd      "generate.py")
                     (ack-args         "")
                     (startup-hook     nil)
                     (shutdown-hook    nil)))

      (project-def "seqzap-trunk"
                   '((basedir          "c:/work/seqzap/trunk/")
                     (src-patterns     ("*.cs"))
                                        ;               (ignore-patterns  ("build"))
                     (tags-file        "~/.mk-project/seqzap-trunk/TAGS")
                     (file-list-cache  "~/.mk-project/seqzap-trunk/files")
                     (open-files-cache "~/.mk-project/seqzap-trunk/open-files")
                     (vcs              svn)
                                        ;               (compile-cmd      "generate.py")
                     (ack-args         "")
                     (startup-hook     nil)
                     (shutdown-hook    nil)))

      (global-set-key (kbd "C-c p c") 'project-compile)
      (global-set-key (kbd "C-c p g") 'project-grep)
      (global-set-key (kbd "C-c p a") 'project-ack)
      (global-set-key (kbd "C-c p l") 'project-load)
      (global-set-key (kbd "C-c p u") 'project-unload)
      (global-set-key (kbd "C-c p f") 'project-find-file) ; or project-find-file-ido
      (global-set-key (kbd "C-c p i") 'project-index)
      (global-set-key (kbd "C-c p s") 'project-status)
      (global-set-key (kbd "C-c p h") 'project-home)
      (global-set-key (kbd "C-c p d") 'project-dired)
      (global-set-key (kbd "C-c p t") 'project-tags)))

(defun my-java-project-startup ()
  (setq c-basic-offset 3))

(defun toggle-php-html-mode ()
  (interactive)
  "Toggle mode between PHP & HTML Helper modes"
  (cond ((string= mode-name "HTML helper")
         (php-mode))
        ((string= mode-name "PHP")
         (html-helper-mode))))

(global-set-key [f5] 'toggle-php-html-mode)

(if sequanto
    (progn
      (setq c-site-default-style "k&r")
      (setq c-basic-offset 3)
      (setq c-brace-imaginary-offset 0)
      (setq-default indent-tabs-mode nil)
      (c-set-offset 'statement-cont 3)
      (c-set-offset 'substatement-open 0)
      (c-set-offset 'inline-open 0))

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
    (c-set-offset 'substatement-open 0)))

(if (not win32)
    (progn
      (add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
      
      (defun my-rcirc-print-hook (process sender response target text)
        (when (and (string-match (rcirc-nick process) text)
                   (not (string= (rcirc-nick process) sender))
                   (not (string= (rcirc-server-name process) sender)))
          (start-process "notify" nil "notify-send" (concat "rcirc: " sender) text)))
      
      (setq browse-url-browser-function (quote browse-url-epiphany)
            browse-url-epiphany-arguments (quote ("-n"))
            browse-url-epiphany-startup-arguments (quote ("-n"))
            debian-changelog-mailing-address "halfdan@halfdans.net"))
  
  (progn 
    (add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
    
    (defun my-rcirc-print-hook (process sender response target text)
      (when (and (string-match (rcirc-nick process) text)
                 (not (string= (rcirc-nick process) sender))
                 (not (string= (rcirc-server-name process) sender)))
        (start-process "notify" nil "WinNotify" (concat "rcirc: " sender) text)))
    
    (setq diff-command "c:/Programmer/GnuWin32/bin/diff.exe"
          ediff-diff-program "c:/Programmer/GnuWin32/bin/diff.exe"
          ediff-diff3-program "c:/Programmer/GnuWin32/bin/diff3.exe"
          ispell-program-name "c:/Programmer/Aspell/bin/aspell.exe"
          python-python-command "c:\\\\python25\\\\pythonw.exe"
          gud-pdb-command-name "c:\\\\python25\\\\pythonw.exe"
          )))

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

(defun trac-wiki-auto-mode-function ()
  (if (and (eq major-mode 'text-mode)
           (member (file-name-extension (buffer-file-name))
                   '("txt" "wiki"))
           (re-search-forward "^=+ [^=\n]+ =+\\s *$" nil t))
      (trac-wiki-mode)))

(add-hook 'find-file-hook
          'trac-wiki-auto-mode-function)

(server-start)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-doc-face ((t (:foreground "DarkRed"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "cyan4"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))))
 '(rcirc-dim-nick ((t (:inherit default :foreground "gray"))))
 '(rcirc-url ((t (:foreground "blue" :underline t :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debian-changelog-mailing-address "halfdan@halfdans.net" t)
 '(display-time-mode t)
 '(findr-skip-file-regexp "^[#\\.]|^.*~")
 '(font-lock-global-modes t)
 '(fortran-tab-mode-default nil)
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8))))
 '(show-paren-mode t)
 '(tags-add-tables (quote ask-user))
 '(visible-bell t))