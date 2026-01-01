;;; github-theme-test.el --- Tests for GitHub theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 chaploud

;; This file is part of github-theme.

;;; Commentary:

;; Test suite for github-theme.
;; Run with: emacs --batch -l github-theme-test.el

;;; Code:

(require 'cl-lib)

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'custom-theme-load-path (file-name-directory load-file-name))

;; Test utilities
(defvar github-test-passed 0)
(defvar github-test-failed 0)

(defmacro github-test-assert (form message)
  "Assert that FORM is non-nil, print MESSAGE on failure."
  `(if ,form
       (progn
         (setq github-test-passed (1+ github-test-passed))
         (message "  PASS: %s" ,message))
     (setq github-test-failed (1+ github-test-failed))
     (message "  FAIL: %s" ,message)))

(defun github-test-summary ()
  "Print test summary and exit with appropriate code."
  (message "\n========================================")
  (message "Test Results: %d passed, %d failed" github-test-passed github-test-failed)
  (message "========================================")
  (kill-emacs (if (> github-test-failed 0) 1 0)))

;; Mock display capabilities for consistent testing
(advice-add 'display-color-cells :override (lambda (&rest _) 16777216))

;; Load theme
(message "\nLoading github-theme...")
(require 'github-theme)

;;; Tests

(message "\n--- Test 1: Theme loads successfully ---")
(github-test-assert (boundp 'github-theme-flavor)
                    "github-theme-flavor is defined")
(github-test-assert (boundp 'github-theme-flavor-alist)
                    "github-theme-flavor-alist is defined")
(github-test-assert (listp github-theme-flavor-alist)
                    "github-theme-flavor-alist is a list")

(message "\n--- Test 2: All flavors are defined ---")
(github-test-assert (assoc 'light github-theme-flavor-alist)
                    "light flavor exists")
(github-test-assert (assoc 'light-high-contrast github-theme-flavor-alist)
                    "light-high-contrast flavor exists")
(github-test-assert (assoc 'dark github-theme-flavor-alist)
                    "dark flavor exists")
(github-test-assert (assoc 'dark-dimmed github-theme-flavor-alist)
                    "dark-dimmed flavor exists")

(message "\n--- Test 3: Color retrieval works ---")
(setq github-theme-flavor 'dark)
(github-test-assert (string= "#e6edf3" (github-theme-color 'fg-default))
                    "fg-default for dark is #e6edf3")

(setq github-theme-flavor 'light)
(github-test-assert (string= "#1f2328" (github-theme-color 'fg-default))
                    "fg-default for light is #1f2328")

(setq github-theme-flavor 'dark-dimmed)
(github-test-assert (string= "#adbac7" (github-theme-color 'fg-default))
                    "fg-default for dark-dimmed is #adbac7")

(setq github-theme-flavor 'light-high-contrast)
(github-test-assert (string= "#0e1116" (github-theme-color 'fg-default))
                    "fg-default for light-high-contrast is #0e1116")

(message "\n--- Test 4: Light/dark detection works ---")
(setq github-theme-flavor 'light)
(github-test-assert (github-theme--light-p)
                    "light is detected as light variant")

(setq github-theme-flavor 'light-high-contrast)
(github-test-assert (github-theme--light-p)
                    "light-high-contrast is detected as light variant")

(setq github-theme-flavor 'dark)
(github-test-assert (not (github-theme--light-p))
                    "dark is detected as dark variant")

(setq github-theme-flavor 'dark-dimmed)
(github-test-assert (not (github-theme--light-p))
                    "dark-dimmed is detected as dark variant")

(message "\n--- Test 5: Color utility functions work ---")
(github-test-assert (string= "#ffffff" (github-theme-lighten "#000000" 100))
                    "Lighten black by 100% gives white")

(github-test-assert (string= "#000000" (github-theme-darken "#ffffff" 100))
                    "Darken white by 100% gives black")

(github-test-assert (string= "#808080" (github-theme-lighten "#000000" 50))
                    "Lighten black by 50% gives gray")

(message "\n--- Test 6: Color quantization works ---")
(let ((quantized (github-theme-quantize-color "#1f2328")))
  (github-test-assert (string-match-p "^#[0-9a-f]\\{6\\}$" quantized)
                      "Quantized color is valid hex format"))

(message "\n--- Test 7: Custom color setting works ---")
(setq github-theme-flavor 'dark)
(let ((original (github-theme-color 'fg-default)))
  (github-theme-set-color 'fg-default "#ff0000")
  (github-test-assert (string= "#ff0000" (github-theme-color 'fg-default))
                      "Custom color can be set")
  ;; Restore original
  (github-theme-set-color 'fg-default original))

(message "\n--- Test 8: All required colors exist in each flavor ---")
(dolist (flavor '(light light-high-contrast dark dark-dimmed))
  (setq github-theme-flavor flavor)
  (dolist (color '(fg-default fg-muted canvas-default canvas-subtle
                   border-default accent-fg success-fg danger-fg
                   syntax-comment syntax-keyword syntax-string))
    (github-test-assert (github-theme-color color)
                        (format "%s has %s" flavor color))))

(message "\n--- Test 9: Theme can be loaded ---")
(condition-case err
    (progn
      (setq github-theme-flavor 'dark)
      (load-theme 'github t)
      (github-test-assert (memq 'github custom-enabled-themes)
                          "github theme is enabled"))
  (error
   (github-test-assert nil (format "Theme load failed: %s" err))))

;; Print summary and exit
(github-test-summary)

;;; github-theme-test.el ends here
