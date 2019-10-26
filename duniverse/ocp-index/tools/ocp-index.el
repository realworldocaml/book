;; auto-completion for ocaml using auto-complete
;; (https://github.com/auto-complete/auto-complete) and ocp-index

(provide 'ocp-index)
(require 'cl)

;; Customize defs

(defgroup ocp-index nil
  "ocp-index OCaml completion/doc tool binding configuration"
  :group 'languages)

(defcustom ocp-index-path "ocp-index"
  "*Path to access the ocp-index command"
  :group 'ocp-index :type '(file))

(defcustom ocp-grep-path "ocp-grep"
  "*Path to access the ocp-grep command"
  :group 'ocp-index :type '(file))

(defcustom ocp-index-options "--show=types"
  "*Command-line parameters to add to ocp-index invocations (ex. --show=sigs)"
  :group 'ocp-index :type 'string)

(defcustom ocp-index-override-auto-complete-defaults t
  "*If set, auto-complete defaults will be reset to a sane setting in ocaml
   buffers. Disable if you prefer to configure auto-complete yourself."
  :group 'ocp-index :type 'boolean)

(defcustom ocp-index-auto-complete-workaround t
  "*Fix a bug in auto-complete whith quick-help at EOF in text mode."
  :group 'ocp-index :type 'boolean)

(defcustom ocp-index-extra-completion-sources
  (list 'ac-source-words-in-same-mode-buffers)
  "*Completion sources to enable besides ocp-index completion"
  :group 'ocp-index :type '(repeat symbol))

(defcustom ocp-index-show-help t
  "*If set, show the documentation bubble after completion (otherwise,
   the type is printed in the echo area)."
  :group 'ocp-index :type 'boolean)

(defvar ocp-index-has-auto-complete
  (require 'auto-complete nil t))

(defcustom ocp-index-use-auto-complete ocp-index-has-auto-complete
  "*If set, use `auto-complete' for completion."
  :group 'ocp-index :type 'boolean)

(defcustom ocp-index-use-eldoc nil
  "*If set, automatically enable `eldoc' with ocp-index."
  :group 'ocp-index :type 'boolean)

;; auto-complete bug workaround (complete at EOF in text mode)
(defun ocp-index-enable-ac-workaround ()
  (defun ac-menu-delete ()
    (ac-remove-quick-help)
    (when ac-menu
      (popup-delete ac-menu)
      (setq ac-menu))))

;; Completion aux functions

(defvar ac-ocp-index-current-doc nil)

(defun ocp-index-bounds-of-symbol-at-point ()
  "Matches the fully qualified identifier at point, eg [M1.M2.someval] but
   also somerecord.[M1.M2.somefield]"
  (let ((case-fold-search nil))
    (save-excursion
      (while (looking-back "\\<\\([A-Z][a-zA-Z0-9_']*\.\\)*[a-zA-Z0-9_']*"
                           (line-beginning-position) nil)
        (goto-char (match-beginning 0)))
      (when (looking-at "[a-zA-Z0-9_'.]*[a-zA-Z0-9_']")
        (cons (match-beginning 0) (match-end 0))))))

(defun ocp-index-completion-prefix-start ()
  (car-safe (ocp-index-bounds-of-symbol-at-point)))

(defun ocp-index-symbol-at-point ()
  (let ((bounds (ocp-index-bounds-of-symbol-at-point)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;; override default prefix definition
(defun ac-prefix-symbol ()
  (ocp-index-completion-prefix-start))

(defun ocp-index-column-offset ()
  (save-excursion (let ((pt (point))) (forward-line 0) (- pt (point)))))

(defvar ocp-index-debug nil)

(defun ocp-index-debug-mode ()
  "Display command sent to ocp-index in the *Message* buffer"
  (interactive nil)
  (if ocp-index-debug
      (progn (message "ocp-index debug mode disabled")
             (setq ocp-index-debug nil))
    (message "ocp-index debug mode enabled")
    (setq ocp-index-debug t)))

(defun ocp-index-args (cmd &rest args)
  (let*
      ((file (file-name-nondirectory
                        (file-name-sans-extension (buffer-file-name))))
       (current-module (concat (upcase (substring file 0 1))
                               (substring file 1)))
       (cmd (list* cmd ocp-index-options
                   "--full-open" current-module
                   "--context" ":"
                   args)))
    (when ocp-index-debug
      (message "%s" (mapconcat
                     (lambda (s) (format "%S" s))
                     (list* ocp-index-path cmd) " ")))
    cmd))

(defun ocp-index-run (cmd &rest args)
  (let* ((args (apply 'ocp-index-args cmd args))
         (shell-command (format "exec %s \"$@\"" ocp-index-path)))
    (with-output-to-string
      (let ((ret
             (apply 'call-process-region (point-min) (point)
                    shell-file-name
                    nil (list standard-output nil) nil
                    (list* shell-command-switch shell-command
                           ocp-index-path args))))
        (when (= 127 ret)
          (error "Could not find the Ocp-index program %S" ocp-index-path))))))

(defun ac-ocp-index-candidates ()
  (let* ((output (ocp-index-run "complete" "--sexp" ac-prefix))
         (defs   (car-safe (read-from-string output))))
    (setq ac-ocp-index-current-doc defs)
    (mapcar 'car-safe defs)))

(defun ac-ocp-index-documentation (symbol)
  (let* ((info (cdr (assoc symbol ac-ocp-index-current-doc)))
         (path (cdr (assoc :path info)))
         (kind (cdr (assoc :kind info)))
         (type (cdr (assoc :type info)))
         (doc  (cdr (assoc :doc info))))
    (if doc
        (format "%s %s: %s\n---\n%s" kind path type doc)
      (format "%s %s: %s" kind path type))))

(defun ac-ocp-index-action ()
  (if ocp-index-show-help
      (ac-last-quick-help)
    (let* ((symbol (buffer-substring (ocp-index-completion-prefix-start) (point)))
           (info   (cdr (assoc symbol ac-ocp-index-current-doc)))
           (path   (cdr (assoc :path info)))
           (kind   (cdr (assoc :kind info)))
           (type   (cdr (assoc :type info))))
      (message "%s %s: %s" kind path type))))

(defun ac-ocp-index-init ()
  (setq ac-ocp-index-current-doc nil))

(defvar ac-source-ocp-index
  '((init . ac-ocp-index-init)
    (candidates . ac-ocp-index-candidates)
    (symbol . "o")
    (document . ac-ocp-index-documentation)
    (action . ac-ocp-index-action)
    ))

(defun ocp-index-setup-auto-complete ()
  (require 'auto-complete)
  (auto-complete-mode t)
  (setq ac-sources
        (cons 'ac-source-ocp-index
              ocp-index-extra-completion-sources))
  (when ocp-index-override-auto-complete-defaults
    (set (make-local-variable 'ac-auto-show-menu) t)
    (set (make-local-variable 'ac-auto-start) nil)
    (set (make-local-variable 'ac-delay) 0.0)
    (set (make-local-variable 'ac-expand-on-auto-complete) nil)
    (set (make-local-variable 'ac-ignore-case) nil)
    (set (make-local-variable 'ac-quick-help-delay) 0.2)
    (set (make-local-variable 'ac-trigger-commands) nil))
  (when ocp-index-auto-complete-workaround
    (ocp-index-enable-ac-workaround))
  (add-to-list 'ac-modes 'tuareg-mode)
  (add-to-list 'ac-modes 'caml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get and print indent details ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ocp-index-get-info (path)
  (let*
      ((format "((:kind . \"%k\")(:path . \"%p\")(:type . \"%t\")(:doc . \"%D\")(:parent . \"%e\"))")
       (output (ocp-index-run "print" path format "--separate"))
       (els (concat "(" output ")"))
       (res (read-from-string els)))
    (car-safe res)))

(defun ocp-index-format-info (i)
  "Extracts a formatted info string from an element of the
   structure read from ocp-index (e.g. through
   ocp-index-get-info, or a run with --sexp)"
  (let*
      ((kind (cdr (assoc :kind i)))
       (name (cdr (assoc :path i)))
       (type (cdr (assoc :type i)))
       (doc (cdr (assoc :doc i)))
       (propertize (lambda (a b c) a)))
    (format
     "%s %s: %s%s"
     (propertize kind 'face 'font-lock-keyword-face)
     (propertize name 'face 'font-lock-variable-name-face)
     (propertize type 'face 'font-lock-type-face)
     (if (string= doc "") ""
       (propertize (concat "\n> " doc)
                   'face 'font-lock-doc-face)))))

(defun ocp-index-print-info (ident)
  "Display the type and doc of an ocaml identifier in the echo area using
   ocp-index.
   Call twice to show the enclosing type of field records, variants and methods"
  (interactive (let ((default (ocp-index-symbol-at-point)))
                 (list
                  (read-string
                   (format "type ident (%s): " default) nil nil default))))
  (let*
      ((infos (ocp-index-get-info ident))
       (parents (cl-remove-if (lambda (i) (string= "" (cdr (assoc :parent i))))
                              infos))
       (msg
        (if (not infos) "No definition found"
          (if (and parents (equal last-command this-command))
              (mapconcat (lambda (i) (cdr (assoc :parent i))) parents "\n")
            (mapconcat 'ocp-index-format-info infos "\n")))))
    (display-message-or-buffer msg "*ocp-index*")))

(defun ocp-index-print-info-at-point ()
  (interactive nil)
  (ocp-index-print-info (ocp-index-symbol-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion-at-point support ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ocp-index-completion-data nil
  "The completion data for the last completed prefix.")

(defun ocp-index-completion-candidates (prefix)
  "Sets the data for completion of PREFIX in variable ocp-index-completion-data."
  (let* ((format "(\"%q\" (:path . \"%p\")(:type . \"%t\")(:kind . \"%k\")(:doc . \"%D\")(:loc . \"%l\"))")
         (output (ocp-index-run "complete" "--separate" "--format" format prefix))
         (data (car-safe (read-from-string (concat "(" output ")")))))
    (setq ocp-index-completion-data data)))

(defun ocp-index-completion-exit-function (candidate state)
  "Print the type of CANDIDATE in the echo area."
  (let ((info (cdr-safe (assoc candidate ocp-index-completion-data))))
    (when info (message "%s" (ocp-index-format-info info)))))

(defun ocp-index-completion-company-doc-buffer (candidate)
  (let ((info (cdr-safe (assoc candidate ocp-index-completion-data))))
    (when info
      (company-doc-buffer (ocp-index-format-info info)))))

(defun ocp-index-completion-company-docsig (candidate)
  (let ((info (cdr (assoc candidate ocp-index-completion-data))))
    (when info
      (message "%s" (ocp-index-format-info info)))))

(defun ocp-index-completion-annotation-function (candidate)
  (concat " " (cdr (assoc :kind (cdr (assoc candidate ocp-index-completion-data))))))

(defun ocp-index-completion-company-location (candidate)
  "Return the location of the definition of CANDIDATE as (FILE . LINE)."
  (let ((loc
         (cdr (assoc :loc (cdr (assoc candidate ocp-index-completion-data))))))
    (when (and loc (string-match "^\\(.*\\):\\([0-9]\+\\):\\([0-9]\+\\)$" loc))
      (let ((file (match-string 1 loc))
            (line (string-to-number (match-string 2 loc))))
        (cons file line)))))

(defun ocp-index-completion-table (fun)
  (if (fboundp 'completion-table-with-cache)
      (completion-table-with-cache fun)
    (completion-table-dynamic fun)))

(defun ocp-index-completion-at-point ()
  "Function used for `completion-at-point-functions' in `tuareg-mode' or `caml-mode'.

If `company-mode' has `company-capf' backend enabled (this is the
default in Emacs 24.4) then `company-mode' will automatically use
this function to present completions to the user."
  (let ((bounds (ocp-index-bounds-of-symbol-at-point)))
    (when bounds
      (let* ((start (car bounds))
             (end (point))
             (prefix (buffer-substring start end)))
        (list start end (ocp-index-completion-table 'ocp-index-completion-candidates)
              :exit-function 'ocp-index-completion-exit-function
              :annotation-function 'ocp-index-completion-annotation-function
              :company-doc-buffer 'ocp-index-completion-company-doc-buffer
              :company-location 'ocp-index-completion-company-location
              :company-docsig 'ocp-index-completion-company-docsig)))))

(defun ocp-index-setup-completion-at-point ()
  (add-hook 'completion-at-point-functions
            'ocp-index-completion-at-point nil 'local))

;;;;;;;;;;;;;;
;; grepping ;;
;;;;;;;;;;;;;;

(defun ocp-index-try-expand-symbol-at-point ()
  (interactive nil)
  (let ((ident (ocp-index-symbol-at-point)))
    (when ident
      (let* ((path (ocp-index-run "print" ident "\"%p\""))
             (path (car-safe (car (read-from-string (concat "(" path ")"))))))
        (if (string= path "") ident path)))))

(defun ocp-index-grep (query)
  "Search for an OCaml ident or string using ocp-grep.
Calls ocp-grep to find uses of a qualified ident in the current project.
The default query is extracted from the ident under point, qualified using ocp-index.
If the query is enclosed in double-quotes, it is understood as a POSIX regexp to
be searched within string literals.

The set of files to search in are determined by ocp-grep: it guesses the project root
and greps in any OCaml source files from there. "
  (interactive
   (let ((default (ocp-index-try-expand-symbol-at-point)))
     (list
      (read-string
       (format "grep OCaml code for (%s): " default) nil nil default))))
  (require 'grep)
  (let ((grep-use-null-device nil))
    (if (string-match-p "\".*\"" query)
        (grep (format "%s -e %s" ocp-grep-path query))
      (grep (format "%s %s" ocp-grep-path query)))))

;;;;;;;;;;;;;
;; jumping ;;
;;;;;;;;;;;;;

(defun ocp-index-jump-to-loc (loc other-window)
  (if (string-match "^\\(.*\\):\\([0-9-]\+\\):\\([0-9-]\+\\)$" loc)
      (let ((file   (match-string 1 loc))
            (line   (string-to-number (match-string 2 loc)))
            (column (string-to-number (match-string 3 loc)))
            (last-buffer (current-buffer)))
        (if (not (file-exists-p file))
            (message "Could not find source file %s" file)
          (if other-window (find-file-other-window file) (find-file file))
          (goto-char (point-min))
          (when (>= line 0) (forward-line (1- line)))
          (when (>= column 0) (forward-char column))
          (when other-window (switch-to-buffer-other-window last-buffer))))
    (message "%s" (replace-regexp-in-string "\n\+$" "" loc))))

(defun ocp-index-jump-to-definition (ident sig other-window)
  "Jump to the definition of an ocaml identifier using ocp-index"
  (interactive (let ((default (ocp-index-symbol-at-point)))
                 (list
                  (read-string
                   (format "lookup ident (%s): " default) nil nil default)
                  nil t)))
  (let* ((output (if sig (ocp-index-run "locate" "-i" ident)
                   (ocp-index-run "locate" ident)))
         (locs (split-string output "\n" t)))
    (if locs
        (progn
          (ocp-index-jump-to-loc (car locs) other-window)
          (cdr locs))
      (message "No definition found")
      nil)))

(defun ocp-index-jump (name sig other-window)
  (if (and (eq (car-safe last-command) name)
           (cdr last-command))
      (let* ((locs (cdr last-command)))
        (if locs
            (progn
              (ocp-index-jump-to-loc (car locs) other-window)
              (cdr locs))))
    (let ((next (ocp-index-jump-to-definition (ocp-index-symbol-at-point) sig other-window)))
      (setq this-command (list* name next)))))

(defun ocp-index-jump-to-definition-at-point ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-definition-at-point nil nil))
(defun ocp-index-jump-to-definition-at-point-other-window ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-definition-at-point nil t))
(defun ocp-index-jump-to-sig-at-point ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-sig-at-point t nil))
(defun ocp-index-jump-to-sig-at-point-other-window ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-sig-at-point t t))

(defun ocp-index-complete ()
  (interactive)
  (if ocp-index-use-auto-complete (auto-complete) (completion-at-point)))

(defvar ocp-index-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-index-print-info-at-point)
    (define-key map (kbd "C-c ;") 'ocp-index-jump-to-definition-at-point-other-window)
    (define-key map (kbd "C-c :") 'ocp-index-jump-to-sig-at-point-other-window)
    (define-key map (kbd "C-c C-;") 'ocp-index-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-:") 'ocp-index-jump-to-sig-at-point)
    (define-key map (kbd "C-c /") 'ocp-index-grep)
    (define-key map (kbd "C-c TAB") 'ocp-index-complete)
    map))

(defun ocp-index-setup-completion ()
  (if ocp-index-use-auto-complete (ocp-index-setup-auto-complete))
  (ocp-index-setup-completion-at-point))

;; eldoc
(defvar ocp-index-last-result nil)
(defun display-message-or-buffer-tostring (msg &rest _)
  (setq ocp-index-last-result
        (let ((result (ocp-index-join-string msg)))
          (if (string-match "No definition found\\|keyword\\.*" result)
              ""
            result))))

(defun ocp-index-join-string (str)
  (with-temp-buffer
    (insert str)
    (let ((fill-column 1000))
      (fill-paragraph nil))
    (buffer-string)))

(defun ocp-index-eldoc-function ()
  (condition-case nil
      (cl-letf (((symbol-function 'display-message-or-buffer)
                 #'display-message-or-buffer-tostring))
        (ocp-index-print-info-at-point))
    (error "")))

(define-minor-mode ocp-index-mode
  "OCaml auto-completion, documentation and source browsing using ocp-index"
  :group 'ocp-index
  :keymap ocp-index-keymap
  (if ocp-index-mode
      (progn
        (when ocp-index-use-eldoc
          (add-function :before-until (local 'eldoc-documentation-function)
                        #'ocp-index-eldoc-function))
        (ocp-index-setup-completion))
    (remove-function (local 'eldoc-documentation-function)
                     #'ocp-index-eldoc-function)
    (when ocp-index-use-auto-complete (auto-complete-mode -1))))

(add-hook 'tuareg-mode-hook 'ocp-index-mode t)
(add-hook 'caml-mode-hook 'ocp-index-mode t)
