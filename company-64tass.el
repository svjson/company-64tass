
(require 'company)

(defvar 64tass-label-completions '()
  "Variable containing a list of known labels/symbols")

(defun company-64tass-create-temp-file (file-name)
  "Writes the current buffer to a temporary file in location provided by built-in `make-temp-file'"
  (let ((tmp-file (expand-file-name (file-name-nondirectory file-name) (make-temp-file "_64tass" 'directory))))
    (write-region nil nil tmp-file nil 0)
    tmp-file))

(defun company-64tass-export-labels (&optional file-name)
  "Exports all label symbols using the 64tass binary, passing the
location/directory as include path"
  (interactive)
  (let ((src-file (or file-name buffer-file-name)))
    (let ((include-dir (file-name-directory buffer-file-name))
          (labels-file (concat (file-name-directory file-name) (file-name-base src-file) ".labels")))
      (call-process "64tass" nil nil nil
                    "--no-output"
                    (or file-name buffer-file-name)
                    "-I" include-dir
                    "-l" labels-file)
      labels-file)))

(defun company-64tass-read-labels-file (file-name)
  "Returns the contents of the file as string"
  (if (file-exists-p file-name)
      (with-temp-buffer
        (insert-file-contents file-name)
        (buffer-string))
    ""))

(defun company-64tass-parse-labels-file (contents)
  "parse contents of labels file to a list of string label names"
  (butlast (map 'list (lambda (row)
                        (car (split-string row "[[:blank:]=]+")))
                (split-string contents "\n"))))

(defun company-64tass-after-change (beg end len)
  "Dumps all labels and symbols of current file and any included files to `64tass-label-completions'"
  (save-match-data
    (if (and buffer-file-name
             (> len 0))
        (let ((tmp-file (company-64tass-create-temp-file buffer-file-name)))
          (set '64tass-label-completions
               (let ((parsed-labels (company-64tass-parse-labels-file
                                     (company-64tass-read-labels-file
                                      (company-64tass-export-labels tmp-file)))))
                 (or parsed-labels 64tass-label-completions)))
          (delete-directory (file-name-directory tmp-file) t)))))

(defun 64tass-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend '64tass-company-backend))
    (prefix (and (eq major-mode 'paw64-mode)
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      64tass-label-completions))))

(defun company-64tass-setup ()
  "Set up company-64tass and enable `company-mode' in the current buffer"
  (add-hook 'after-change-functions 'company-64tass-after-change)
  (setq-local company-backends '(64tass-company-backend))
  (company-mode))

