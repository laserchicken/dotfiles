(require 'package)
(package-initialize)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

;;;emacs dziala jako demon (proces emacs --daemon jako program startowy),
;;;aby przeladowac ten plik nalezy skilowac ten proces i odpalic jeszcze raz

;;;;
(add-to-list 'load-path "~/.emacs.d/")

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

;;;
(setq x-select-enable-clipboard t)

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


;;;;;;wylaczenie gui;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;CHEETAH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  )
(setq auto-mode-alist (cons '( "\\.tmpl\\'" . cheetah-mode ) auto-mode-alist ))

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



;;GROOVY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "~/.emacs.d/groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric "~/.emacs.d/groovy-electric")
             (groovy-electric-mode)))


;;VISUAL BASIC MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'visual-basic-mode "~/.emacs.d/visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                  visual-basic-mode)) auto-mode-alist))



;;CONKEROR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dodajemy conkeror jako domyslna przegladarke (potrzebna w org-mode do otwierania linkow)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "~/bin/conkeror")

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

;;nie chce zeby eksportowalo slowa z podkreslnikami jako dolny indeks
(setq org-export-with-sub-superscripts nil)

;;;;AGENDA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;CAPTURE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-default-notes-file (concat org-directory "/praca.org"))
(define-key global-map "\C-cc" 'org-capture)

;; drugi template wziety ze str. http://emacs-fu.blogspot.com/2010/12/conkeror-web-browsing-emacs-way.html
;; pozwala na capture z conkerora
;; ;; the 'w' corresponds with the 'w' used before as in:
;;   emacsclient \"org-protocol:/capture:/w/  [...]

(setq org-capture-templates
      (quote
       (("t" "todo" entry (file+headline "praca.org" "inbox")
         "* TODO %?%a\n %U\n"
         :clock-in t
         :clock-resume t)
	("w" "" entry (file+headline "praca.org" "inbox")
	 "* %^{Title}\n   %u,\n Source: %c\n\n  %i")
	)))

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
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

;;;;PERL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cperl-mode is preferred to perl-mode                                        
;;; "Brevity is the soul of wit" <foo at acm.org>                               
(defalias 'perl-mode 'cperl-mode)

;;wyłącza podkreślanie spacji
(setq cperl-invalid-face nil)

;; Turns on most of the CPerlMode options
(setq cperl-hairy t)

;; komentarze
(global-set-key (kbd "C-;") 'comment-dwim)

;;;;TRAMP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sluzy do otwierania plikow zdalnie po ssh
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


;;;;JTAGS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;etags dostosowane do kodu javy
;;(add-to-list 'load-path "~/emacs/emacs-23.4/site-lisp")

;;(autoload 'jtags-extras "jtags-extras" "Load jtags-extras.")
;;(add-hook 'java-mode-hook 'jtags-extras)


;;;;VELOCITY MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/emacs/emacs-23.4/site-lisp")
(autoload 'velocity-mode "vtl" nil t)
(add-hook 'html-mode-hook 'velocity-mode t t)
(add-hook 'xml-mode-hook 'velocity-mode t t)
(add-hook 'text-mode-hook 'velocity-mode t t)

;;;;IDO MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;;;ECB;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;emacs code browser
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
;;(load-file "~/emacs/cedet-1.1/common/cedet.el")

;; Enable EDE (Project Management) features
;;(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;;(ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;;(add-to-list 'load-path "~/emacs/emacs-23.4/site-lisp/ecb-snap")
;;(load-file "~/emacs/emacs-23.4/site-lisp/ecb-snap/ecb.el")

;;(global-ede-mode 1)
;;(semantic-load-enable-code-helpers)
;;(global-srecode-minor-mode 1)
;;(require 'ecb)
;;(require 'jde)

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
(add-hook 'js-mode-hook 'ggtags-mode)
(add-hook 'ruby-mode-hook 'ggtags-mode)
(add-hook 'scheme-mode-hook 'ggtags-mode)

;;;;NXHTML;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/nxhtml/autostart.el")


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

;;;;MAVEN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;integracja z mavenem
;;http://walterhiggins.net/blog/posterous-maven-and-emacs
;;http://vastusutra.blogspot.com/2007/06/getting-emacs-and-maven-2-to-play.html
(require 'compile)
(defvar mvn-command-history nil  "Maven command history variable")

(defun mvnfast()  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((dir (file-name-directory fn)))
    (while (and
	    (not (file-exists-p (concat dir "/pom.xml")))
	    (not (equal dir (file-truename (concat dir "/..")))))
      (setq dir (file-truename (concat dir "/.."))))
    (if (not (file-exists-p (concat dir "/pom.xml")))
	(message "No pom.xml found")
      (compile
       (concat "mvn -f " dir "/pom.xml install -Dmaven.test.skip=true")))
    )))
;;(define-key java-mode-map "\C-c\C-x5" 'mvnfast)
;;TODO : powyzsza linijka powoduje problem 

(defun mvn(&optional args)
  "Runs maven in the current project.
Starting at the directoy where the file being vsisited resides, a search is made for pom.xml recsurively.
A maven command is made from the first directory where the pom.xml file is found is then displayed  in the minibuffer.
The command can be edited as needed and then executed.
Errors are navigate to as in any other compile mode"
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((dir (file-name-directory fn)))
      (while (and
	      (not (file-exists-p (concat dir "/pom.xml")))
	      (not (equal dir (file-truename (concat dir "/..")))))
	(setq dir (file-truename (concat dir "/.."))))
      (if (not (file-exists-p (concat dir "/pom.xml")))
	  (message "No pom.xml found")
	(compile (read-from-minibuffer "Command: "(concat "mvn -f " dir "/pom.xml install -Dmaven.test.skip=true")nil nil 'mvn-command-history))))))

;; String pattern for locating errors in maven output.
;; This assumes a Windows drive letter at the beginning
;; (add-to-list 'compilation-error-regexp-alist '("^\\([a-zA-Z]:.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3))

;;;;LOG4J;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;m.in. koloruje i ulatwia filtrowanie plikow z logami log4j
;;problem pojawia sie z wiekszymi plikami (>256KB tzn. tyle ile wynosi default dla (font-lock-maximum-size))
;;poniewaz takie pliki z logami wystepuja rzadko, postanowilem nie ruszac tej zmiennej
(autoload 'log4j-mode "~/.emacs.d/log4j-mode.el" "Major mode for viewing log files." t)

;;akceptuje rowniez pliki typu "server.log.2013-04-12"
;;TODO : wyrazenie nie jest zupelnie poprawne
(add-to-list 'auto-mode-alist '("\\.log\\.?.*\\'" . log4j-mode))

;;;;RAILS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;integruje emacsa (shell) z rvm 
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;;;;TOUCH TYPING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'typing-of-emacs "~/.emacs.d/typing.el" "The Typing Of Emacs, a game." t)
(put 'dired-find-alternate-file 'disabled nil)

;;;;OCTOPRES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://blog.paphus.com/blog/2012/08/01/introducing-octopress-blogging-for-org-mode/
(require 'org-octopress)

(defun save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))

(setq org-publish-project-alist
      '(("blog-org" .  (:base-directory "~/blog/octopress/source/org_posts/"
					:base-extension "org"
					:publishing-directory "~/blog/octopress/source/_posts/"
					:sub-superscript ""
					:recursive t
					:publishing-function org-publish-org-to-octopress
					:headline-levels 4
					:html-extension "markdown"
					:octopress-extension "markdown"
					:body-only t))
	("blog-extra" . (:base-directory "~/blog/octopress/source/org_posts/"
					 :publishing-directory "~/blog/octopress/source/"
					 :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
					 :publishing-function org-publish-attachment
					 :recursive t
					 :author nil
					 ))
	("blog" . (:components ("blog-org" "blog-extra")))
	))

;;I don't use document title in my org posts (post has it title istead).
;;Common headers hierarchy is expected by my octpress theme.
(setq org-export-octopress-toplevel-hlevel 1)


;;;;ORG-MODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;turn on `org-indent-mode' (clean view) on startup
(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://gist.github.com/tkf/3951163
;; Workaround the annoying warnings:
;; Warning (mumamo-per-buffer-local-vars):
;; Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
	   (>= emacs-minor-version 2))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq local-remote-prefixes
  '(("/home/dev/Development/Smyk2.0" "https://subversion.ultimo.pl/trac/projects/browser/Smyk2.0")
    ("/home/dev/git/thefreedictionary-mode" "https://github.com/laserchicken/thefreedictionary-mode/blob")))

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
