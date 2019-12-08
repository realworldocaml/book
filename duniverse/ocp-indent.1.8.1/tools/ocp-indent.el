;;; ocp-indent.el --- automatic indentation with ocp-indent
;;
;; Copyright 2012-2013 OCamlPro

;; Keywords: ocaml languages
;; URL: http://www.typerex.org/ocp-indent.html

;; All rights reserved.This file is distributed under the terms of the
;; GNU Lesser General Public License version 2.1 with linking
;; exception.

;; TypeRex is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.
;;

;;; Commentary:

;; Description:
;; ocp-indent is a simple tool and library to indent OCaml code.

;; Installation:
;; You need ocp-indent installed on you system to work.

;; Usage:
;; Eval this file to automatically use ocp-indent on caml/tuareg buffers.

;;; Code:

(require 'cl)

(defgroup ocp-indent nil
  "ocp-indent OCaml indenter binding configuration"
  :group 'languages)

(defcustom ocp-indent-path "ocp-indent"
  "*Path to access the ocp-indent command"
  :group 'ocp-indent :type '(file))

(defcustom ocp-indent-config nil
  "*Ocp-indent config string, as for its --config option.
WARNING: DEPRECATED, this will override any user or project
ocp-indent configuration files"
  :group 'ocp-indent
  :type '(choice (const nil) (string)))

(defcustom ocp-indent-syntax nil
  "*Enabled syntax extensions for ocp-indent (see option --syntax)"
  :group 'ocp-indent
  :type '(repeat string))

(defcustom ocp-indent-allow-tabs nil
  "*Allow indent-tabs-mode in ocaml buffers. Not recommended, won't work well."
  :group 'ocp-indent
  :type '(bool))

(defcustom ocp-indent-untabify nil
  "Send the buffer `untabify'ed to ocp-indent. Allows partial
indent even with tabs present.

Tabs are not replaced in the buffer except on lines getting an
indentation change."
  :group 'ocp-indent
  :type '(bool))

(defun ocp-in-indentation-p ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun ocp-indent-args (start-line end-line)
  (append
   (list "--numeric"
         "--lines" (format "%d-%d" start-line end-line))
   (if ocp-indent-config (list "--config" ocp-indent-config) nil)
   (reduce (lambda (acc syn) (list* "--syntax" syn acc))
           ocp-indent-syntax :initial-value nil)))

(defun ocp-indent-file-to-string (file)
  (replace-regexp-in-string
   "\n$" ""
   (with-temp-buffer (insert-file-contents file)
                     (buffer-string))))

(defmacro ocp-indent--with-untabify (&rest body)
  "If there are tabs and `ocp-indent-untabify', create a
temporary buffer containing the current buffer's contents
untabified and evaluate BODY there like `progn'. See also
`with-temp-buffer'. Otherwise evaluate BODY in the current
buffer."
  (declare (indent 0) (debug t))
  (let ((buf (make-symbol "buf")))
    `(if (not (and ocp-indent-untabify
                   (save-excursion (goto-char (point-min)) (search-forward "\t" nil t))))
         (progn ,@body)
       (let ((,buf (generate-new-buffer " *ocp-indent*")))
         (unwind-protect
             (progn
               (copy-to-buffer ,buf (point-min) (point-max))
               (with-current-buffer ,buf
                 (untabify (point-min) (point-max))
                 (progn ,@body)))
           (and (buffer-name ,buf) (kill-buffer ,buf)))))))

(defun ocp-indent-region (start end)
  (interactive "r")
  (let*
      ((start-line (line-number-at-pos start))
       (end-line (line-number-at-pos end))
       (errfile (make-temp-name (concat temporary-file-directory "ocp-indent-error")))
       (indents-str
        (with-output-to-string
          (ocp-indent--with-untabify
            (if (/= 0
                    (apply 'call-process-region
                           (point-min) (point-max) ocp-indent-path nil
                           (list standard-output errfile) nil
                           (ocp-indent-args start-line end-line)))
                (error "Can't indent: %s returned failure" ocp-indent-path)))))
       (indents (mapcar 'string-to-number (split-string indents-str))))
    (when (file-exists-p errfile)
      (message (ocp-indent-file-to-string errfile))
      (delete-file errfile))
    (save-excursion
      (goto-char start)
      (mapcar
       #'(lambda (indent) (indent-line-to indent) (forward-line))
       indents))
    (when (ocp-in-indentation-p) (back-to-indentation))))

(defun ocp-indent-line ()
  (interactive nil)
  (ocp-indent-region (point) (point)))

(defun ocp-indent-buffer ()
  (interactive nil)
  (ocp-indent-region 0 (buffer-size)))

;;;###autoload
(defun ocp-setup-indent ()
  (interactive nil)
  (let ((buffer-extension (and (buffer-file-name)
                               (file-name-extension (buffer-file-name)))))
    (unless (string= buffer-extension "mly")
      (unless ocp-indent-allow-tabs (set 'indent-tabs-mode nil))
      (when (string= buffer-extension "mll")
        (set (make-local-variable 'ocp-indent-syntax)
             (cons "mll" ocp-indent-syntax)))
      (set (make-local-variable 'indent-line-function) #'ocp-indent-line)
      (set (make-local-variable 'indent-region-function) #'ocp-indent-region))))

;;;###autoload
(defun ocp-indent-caml-mode-setup ()
  (ocp-setup-indent)
  (local-unset-key "\t"))  ;; caml-mode rebinds TAB !

(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)

(add-hook 'caml-mode-hook 'ocp-indent-caml-mode-setup  t)

(provide 'ocp-indent)

;;; ocp-indent.el ends here
