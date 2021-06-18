(require 'package)
(package-initialize)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PS1")

  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

;;;;
;;(add-to-list 'load-path "~/.emacs.d/")

;;;;image+;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;you need ImageMagick to use this package
;;for image zooming
;;(require 'image+)

;;;;;transpozycja bufforów;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;If you have a window split showing two buffers, you can transpose the two buffers
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; 
(global-set-key "\C-x4t" 'transpose-buffers)

;;ansi color vector customization (for shell mode)
;;when background color is black, blue should be CadetBlue1 for instance
;;otherwise ls dirs color is not well visible
;;;M-x list-colors-display to list available colors

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq ansi-color-names-vector
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"])

;;;;dired;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;When moving to parent directory by `^´,
;;Dired by default creates a new buffer for each movement up.
;;The following rebinds `^´ to use the same buffer.

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;;
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))


;;;;;;no gui;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python-mode settings

;;(require 'pycomplete)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

(setq interpreter-mode-alist(cons '("python" . python-mode)
                                  interpreter-mode-alist))
(setq python-mode-hook
      '(lambda () (progn
                    (set-variable 'py-python-command "/usr/bin/python2.7")
                    (set-variable 'py-indent-offset 4)
                    (set-variable 'py-smart-indentation nil)
                    (set-variable 'indent-tabs-mode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))

;;osx copy/paste
;;https://gist.github.com/the-kenny/267162
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
  
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;;ORG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/share/emacs/23.3/lisp/org")
(require 'org-install)
(require 'org-protocol)

(setq org-directory (expand-file-name "~/org"))

(setq org-todo-keywords
      (quote ((sequence
               "TODO(t)"
               "NEXT(n)"
               "STARTED(s)"
               "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "INBOX"))))

;;
(setq org-export-with-sub-superscripts nil)

;;;;AGENDA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;BABEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-ditaa-jar-path "~/emacs/diagram_tools/ditaa/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/emacs/diagram_tools/plantuml/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (shell . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(setq org-startup-with-inline-images nil)

;; comment
(global-set-key (kbd "C-;") 'comment-dwim)

;;;;TRAMP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'tramp)
;;(setq tramp-default-method "ssh")
;;(setq tramp-shell-prompt-pattern "^[^>$][>$] *")

(add-to-list 'load-path "~/emacs/emacs-23.4/site-lisp/tramp-2.2.6/lisp/")
(require 'tramp)
;;(setq tramp-shell-prompt-pattern "^.*[>$] *")
(setq tramp-shell-prompt-pattern "^[^>$][>$] *")
;;(host user proxy)

;;;;ETAGS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;funkcja tworzaca plik z tagami rekursywnie w zadanym katalogu, dla javy
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.java\" | etags -" dir-name)))

;;;;IDO MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(semantic-mode 1)
(global-ede-mode 1)
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-semantic-idle-summary-mode 1)

(setq ecb-tip-of-the-day nil)  ;;wylacza tip of the day - wyskakiwalo niepotrzebne okienko

;;;;JAVA MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tabulator ma miec 4 a nie 8 spacji (zarowno w nowotworzonym jak i w czytanym kodzie)
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))

(add-hook 'java-mode-hook 'ggtags-mode)
;;(add-hook 'js-mode-hook 'ggtags-mode)
(add-hook 'ruby-mode-hook 'ggtags-mode)
(add-hook 'scheme-mode-hook 'ggtags-mode)

;;;;GOOGLE TRANSLATE;;;;;;;;;;;;;;;;;;;;;;;
;;http://oleksandrmanzyuk.wordpress.com/2011/09/21/using-google-translate-from-emacs/
(defun google-translate (text)
  (interactive
   (list
    (read-from-minibuffer "Translate: ")))
  (with-output-to-temp-buffer "*Google Translate*"
    (set-buffer "*Google Translate*")
    (insert (format "%s" text))
    (facemenu-set-face 'bold (point-min) (point-max))
    (insert (format "\n\n%s"
                    (shell-command-to-string
                     (format "translate \"%s\"" text))))))

;;;;ORG-MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;turn on `org-indent-mode' (clean view) on startup
(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://gist.github.com/tkf/3951163
;; Workaround the annoying warnings:
;; Warning (mumamo-per-buffer-local-vars):
;; Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
	   (>= emacs-minor-version 3))
(eval-after-load "mumamo"
  '(setq mumamo-per-buffer-local-vars
	 (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

;;for following warnings:
;;Warning: `font-lock-beginning-of-syntax-function' is an obsolete variable (as
;;    of 23.3); use `syntax-begin-function' instead.
;;Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;    use `syntax-propertize-function' instead.
;;I changed (renamed) functions in mumamo.el file.

;;;;SQL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;highlight postgres keywords in sql-mode by default
(add-hook 'sql-mode-hook
	  (lambda ()
	    (sql-highlight-postgres-keywords)))

;;;;MULTI-TERM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y"))

(setq term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete)))

;;;;SPELLING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name "aspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(add-hook 'org-mode-hook
	  (lambda()
	    (flyspell-mode 1)))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(global-set-key (kbd "C-c P") 
  (lambda()(interactive)
    (ispell-change-dictionary "pl")
    (flyspell-buffer)))

(setenv "LANG" "en_US.UTF-8")

;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-matching-pair-for-file-path (file-path url-path-pairs)
  ;;if list is not empty
  (while url-path-pairs
      ;;get next pair's first element (path)
      ;;and try to match it against actual file path
      (cond ((string-match (car (car url-path-pairs)) file-path)
	     ;;if matched it will be the pair returned
	     (setq matching-pair (car url-path-pairs)) 
	     (setq url-path-pairs '()))
	    ;;if not matched remove that pair and proceed with left pairs      
	    (t (setq url-path-pairs (cdr url-path-pairs)))))
  matching-pair)

(defun show-file-repo-browser (branch &optional tag)
  "Browse code from buffer on a dedicated repo web site"
  (interactive "sBranch: \nsTag: ")
  ;;file-prefix is local file path prefix which
  ;;will be replaced with url-prefix for remote repo browser,
  ;;rest of path is supposed to be the same
  ;;for both remote browser url and local path
  (let* ((file-path (buffer-file-name))
	 (pair (get-matching-pair-for-file-path file-path
						local-remote-prefixes))
	 (create-url-prefix (lambda (url-prefix branch tag)
	   (concat url-prefix
		   "/" branch
		   (if (not (equal tag ""))
		       (concat "/" tag))))))
      (if pair
	  (shell-command
	   (concat browse-url-generic-program
		   " "
		   (replace-regexp-in-string (car pair)
					     (concat (car (cdr pair))
						     "/" branch
						     (if (not (equal tag ""))
							 (concat "/" tag)))
					     file-path)))
	(message "No local-remote-prefixes pair defined"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(dired) opens file in external app;;;;;;;;;;
;;http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html

(defun open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))
    
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )
    
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wheatgrass))
 '(package-selected-packages
   '(lsp-mode emmet-mode prettier-js add-node-modules-path flycheck indium elixir-mode rjsx-mode graphql-mode php-mode scala-mode speed-type js2-mode exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;;;;;;;;ruby
(setq-default flycheck-disabled-checkers '(ruby-reek))

;;;;;;;;js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (setq indent-tabs-mode nil
                               tab-width 2
			       js2-basic-offset 2))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;;;;;eslint
(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;; Enable eslint checker for rjsx-mode
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;prettier
(require 'prettier-js)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;;;;;;;lsp
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;;;;;;;css-mode
(setq css-indent-offset 2)

;;;;;;;emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes

;;;;;;;;;grep-find
;;(grep-apply-setting 'grep-find-command "find . -type f -not -path './node_modules/*' | xargs grep ''")

(setq ring-bell-function 'ignore)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)

(server-start)
