;;; quarto-mode-test.el --- Tests for quarto-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT test suite for `quarto-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'quarto-mode)

(defvar poly-markdown+r-mode-map)

(ert-deftest quarto-mode-test-activation-and-keymap ()
  "Test activating `poly-quarto-mode' and keymap inheritance."
  (with-temp-buffer
    (insert "---\ntitle: Test\n---\n\nHello.\n")
    (poly-quarto-mode)
    (should poly-quarto-mode)
    (should (eq major-mode 'markdown-mode))
    (should (eq (keymap-parent poly-quarto-mode-map)
                (if quarto-mode--has-poly-r-p
                    poly-markdown+r-mode-map
                  poly-markdown-mode-map)))
    (should (eq (indirect-variable 'quarto-root-polymode)
                'poly-quarto-polymode))))

(ert-deftest quarto-mode-test-buffer-local-markdown-command ()
  "Test that `quarto-mode' sets `markdown-command' buffer-locally."
  (let ((orig-cmd (default-value 'markdown-command))
        (orig-needs (default-value 'markdown-command-needs-filename)))
    (with-temp-buffer
      (poly-quarto-mode)
      (should (local-variable-p 'markdown-command))
      (should (local-variable-p 'markdown-command-needs-filename))
      (should (eq markdown-command #'quarto-mode-markdown-command))
      (should (eq markdown-command-needs-filename t)))
    (should (equal (default-value 'markdown-command) orig-cmd))
    (should (equal (default-value 'markdown-command-needs-filename) orig-needs))))

(ert-deftest quarto-mode-test-exporter ()
  "Test `quarto-poly-markdown-exporter' registration and helpers."
  (should (memq 'quarto-poly-markdown-exporter
                (eieio-oref poly-quarto-polymode 'exporters)))
  (should (equal (quarto-pm--shell-auto-selector 'doc) "AUTO DETECT"))
  (should (equal (quarto-pm--shell-auto-selector 'command) "quarto render %i"))
  (should (eq (quarto-pm--shell-auto-selector 'output-file)
              #'quarto-pm--output-file-sniffer))
  (with-temp-buffer
    (insert "Output created: foo.html\nOther line\nOutput created: foo.html\n")
    (let ((files (quarto-pm--output-file-sniffer)))
      (should (= (length files) 1))
      (should (string-suffix-p "foo.html" (car files))))))

(ert-deftest quarto-mode-test-process-line ()
  "Test URL extraction from preview output without creating a *debug* buffer."
  (let ((quarto-mode--preview-url nil))
    (when (get-buffer "*debug*")
      (kill-buffer "*debug*"))
    (quarto-mode--process-line "Browse at http://localhost:4200/\n")
    (should quarto-mode--preview-url)
    (should (equal (url-type quarto-mode--preview-url) "http"))
    (should (equal (url-host quarto-mode--preview-url) "localhost"))
    (should (= (url-port quarto-mode--preview-url) 4200))
    (should-not (get-buffer "*debug*"))
    (quarto-mode--process-line "Browse at https://127.0.0.1:8443/index.html\n")
    (should (equal (url-type quarto-mode--preview-url) "https"))
    (should (= (url-port quarto-mode--preview-url) 8443))))

(ert-deftest quarto-mode-test-buffer-in-project ()
  "Test project root detection via _quarto.yml."
  (with-temp-buffer
    (should-not (quarto-mode--buffer-in-quarto-project-p)))
  (let* ((dir (make-temp-file "quarto-proj-" t))
         (subdir (expand-file-name "chapters" dir))
         (qmd (expand-file-name "ch1.qmd" subdir)))
    (unwind-protect
        (progn
          (make-directory subdir t)
          (write-region "project:\n  type: book\n" nil
                        (expand-file-name "_quarto.yml" dir) nil 'silent)
          (with-temp-buffer
            (setq buffer-file-name qmd)
            (should (equal (file-truename
                            (quarto-mode--buffer-in-quarto-project-p))
                           (file-name-as-directory (file-truename dir))))))
      (delete-directory dir t))))

(ert-deftest quarto-mode-test-preview-refresh-complete ()
  "Test refresh callback on HTTP 200, non-200, and nil status codes."
  (let ((restarted 0))
    (cl-letf (((symbol-function 'quarto-preview)
               (lambda () (setq restarted (1+ restarted)))))
      (quarto-mode--preview-refresh-complete
       :data nil :symbol-status 'success :error-thrown nil
       :response (make-request-response :status-code 200))
      (should (= restarted 0))
      (quarto-mode--preview-refresh-complete
       :response (make-request-response :status-code 500))
      (should (= restarted 1))
      (quarto-mode--preview-refresh-complete
       :response (make-request-response :status-code nil))
      (should (= restarted 2)))))

(ert-deftest quarto-mode-test-fill-paragraph ()
  "Test `fill-paragraph' advice inside and outside `poly-quarto-mode'."
  ;; Empty poly-quarto-mode buffer must not error.
  (with-temp-buffer
    (poly-quarto-mode)
    (fill-paragraph))
  ;; Pandoc/Quarto fenced div markers are preserved on their own lines.
  (dolist (opener '("::: {.callout-note}" ":::{.callout-note}"))
    (with-temp-buffer
      (insert opener "\n"
              "Word one two three four five six seven eight nine ten "
              "eleven twelve thirteen fourteen fifteen.\n"
              ":::\n")
      (poly-quarto-mode)
      (goto-char (point-min))
      (forward-line 1)
      (fill-paragraph)
      (goto-char (point-min))
      (should (looking-at (regexp-quote (concat opener "\n"))))
      (goto-char (point-max))
      (forward-line -1)
      (should (looking-at "^:::$"))))
  ;; Advice must not hijack non-quarto buffers.
  (with-temp-buffer
    (text-mode)
    (insert "::: {.callout-note}\nShort line.\n:::\n")
    (goto-char (point-min))
    (forward-line 1)
    (fill-paragraph)
    (should-not (bound-and-true-p poly-quarto-mode))))

(ert-deftest quarto-mode-test-chunk-switching ()
  "Test inner chunk switching for Python (and R when ESS/poly-R is loaded)."
  (let ((buf (generate-new-buffer "test-chunks.qmd")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\ntitle: Chunks\n---\n\n"
                  "```{python}\nx = 1\n```\n\n"
                  "```{r}\ny <- 2\n```\n")
          (poly-quarto-mode)
          (goto-char (point-min))
          (re-search-forward "x = 1")
          (pm-switch-to-buffer)
          (should (derived-mode-p 'python-base-mode))
          (should (eq polymode-eval-region-function
                      #'quarto-mode--python-eval-region))
          (when quarto-mode--has-poly-r-p
            (re-search-forward "y <- 2")
            (pm-switch-to-buffer)
            (should (eq major-mode 'ess-r-mode))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest quarto-mode-test-unload-function ()
  "Test that `quarto-mode-unload-function' removes `fill-paragraph' advice."
  (with-temp-buffer
    (poly-quarto-mode))
  (should quarto-mode-advice-installed)
  (should (advice-member-p #'quarto-mode--fill-paragraph #'fill-paragraph))
  (unwind-protect
      (progn
        (quarto-mode-unload-function)
        (should-not quarto-mode-advice-installed)
        (should-not (advice-member-p #'quarto-mode--fill-paragraph
                                     #'fill-paragraph)))
    ;; Restore advice so the loaded feature remains consistent.
    (unless quarto-mode-advice-installed
      (advice-add #'fill-paragraph :around #'quarto-mode--fill-paragraph)
      (setq quarto-mode-advice-installed t))))

(provide 'quarto-mode-test)
;;; quarto-mode-test.el ends here
