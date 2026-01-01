;;; github-theme.el --- GitHub color theme -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Shota Tamura
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/your-username/github-theme-emacs
;; Keywords: faces themes

;; This file is part of github-theme.

;; github-theme is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; github-theme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with github-theme.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GitHub theme for Emacs
;;
;; A faithful port of GitHub's official color schemes to Emacs.
;; Colors extracted from the official GitHub VS Code theme (primer/github-vscode-theme).
;;
;; To select a flavor and enable the theme:
;;
;;     (setq github-theme-flavor 'dark) ; or 'light, 'dark-dimmed, 'light-high-contrast
;;     (load-theme 'github t)
;;
;; Available flavors:
;; - `light'              GitHub Light Default
;; - `light-high-contrast' GitHub Light High Contrast
;; - `dark'               GitHub Dark Default
;; - `dark-dimmed'        GitHub Dark Dimmed

;;; Code:

(eval-when-compile (require 'subr-x))

(deftheme github "GitHub color theme for Emacs.")

;;; Configuration options:

(defgroup github-theme nil
  "GitHub theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom github-theme-flavor 'dark
  "The flavor to use for the GitHub theme.
Must be one of `light', `light-high-contrast', `dark', or `dark-dimmed'."
  :type '(choice
          (const :tag "Light Default" light)
          (const :tag "Light High Contrast" light-high-contrast)
          (const :tag "Dark Default" dark)
          (const :tag "Dark Dimmed" dark-dimmed))
  :group 'github-theme)

(defcustom github-theme-italic-comments nil
  "Use :slant italic for comments."
  :type 'boolean
  :group 'github-theme)

(defcustom github-theme-italic-blockquotes t
  "Use :slant italic for blockquotes in markdown and org."
  :type 'boolean
  :group 'github-theme)

(defcustom github-theme-italic-variables nil
  "Use :slant italic for variables."
  :type 'boolean
  :group 'github-theme)

;;; Flavor definitions:

(defun github-theme--define-flavor (flavor colors)
  "Define a new GitHub flavor named FLAVOR with COLORS."
  (custom-declare-variable (intern (concat "github-" (symbol-name flavor) "-colors"))
    `(funcall ',(lambda () colors))
    (concat "Colors for GitHub " (symbol-name flavor))
    :type '(alist :key-type symbol :value-type string)
    :group 'github-theme))

(defvar github-theme-flavor-alist '()
  "Alist of flavors to alist of names to hex colors.")

(when load-file-name
  ;; Load the flavor definitions
  (with-temp-buffer
    (insert-file-contents (expand-file-name "github-definitions.el"
                            (file-name-directory load-file-name)))
    (setq github-theme-flavor-alist (read (current-buffer))))

  ;; Define flavors
  (let ((flavor #'(lambda (sym) (alist-get sym github-theme-flavor-alist))))
    (github-theme--define-flavor 'light (funcall flavor 'light))
    (github-theme--define-flavor 'light-high-contrast (funcall flavor 'light-high-contrast))
    (github-theme--define-flavor 'dark (funcall flavor 'dark))
    (github-theme--define-flavor 'dark-dimmed (funcall flavor 'dark-dimmed))))

(unless (listp github-theme-flavor-alist)
  (error "Please load with `load' or `require'"))

;;; Internal functions:

(defun github-theme-quantize-color (color)
  "Quantize COLOR to a 256 color palette."
  (let ((i 1)
        (str "#"))
    (while (<= i 5)
      (setq str
            (concat str
                    (format "%02x"
                            (* (round (/ (string-to-number (substring color i (+ i 2)) 16) 17)) 17))))
      (setq i (+ i 2)))
    str))

(defun github-theme--hex-to-rgb (color)
  "Convert a hex COLOR string like \"#rrggbb\" to a list of three integers."
  (mapcar (lambda (i) (string-to-number (substring color i (+ i 2)) 16))
          '(1 3 5)))

(defun github-theme--rgb-to-hex (r g b)
  "Convert R, G, B integers to a hex color string."
  (format "#%02x%02x%02x" r g b))

(defun github-theme--rnd (n)
  "Round N to the nearest integer."
  (round n))

(defun github-theme-lighten (color value)
  "Lighten COLOR by VALUE% (0-100)."
  (let* ((factor (/ value 100.0)))
    (apply #'github-theme--rgb-to-hex
           (mapcar (lambda (v)
                     (github-theme--rnd (min 255 (+ (* (- 255 v) factor) v))))
                   (github-theme--hex-to-rgb color)))))

(defun github-theme-darken (color value)
  "Darken COLOR by VALUE% (0-100)."
  (let* ((factor (/ value 100.0)))
    (apply #'github-theme--rgb-to-hex
           (mapcar (lambda (v)
                     (floor (* (- 1 factor) v)))
                   (github-theme--hex-to-rgb color)))))

(defun github-theme--light-p (&optional flavor)
  "Return t if FLAVOR (or current flavor) is a light variant."
  (memq (or flavor github-theme-flavor) '(light light-high-contrast)))

(defun github-theme-recolor (color value)
  "Darken or lighten COLOR based on the current flavor."
  (if (github-theme--light-p)
      (github-theme-darken color value)
    (github-theme-lighten color value)))

;;; User functions:

(defun github-theme-reload ()
  "Reload the GitHub theme.
Useful after setting custom colors with `github-theme-set-color'."
  (interactive)
  (disable-theme 'github)
  (load-theme 'github t))

(defun github-theme-load-flavor (flavor)
  "Set the GitHub flavor to FLAVOR and reload the theme.
If called interactively, a list of flavors is presented."
  (interactive
   (list
    (intern (completing-read
             "GitHub flavor: "
             '(light light-high-contrast dark dark-dimmed)
             nil t))))
  (setq github-theme-flavor flavor)
  (github-theme-reload)
  (message "GitHub flavor changed to %s" flavor))

(defun github-theme--colors-of (&optional flavor)
  "Return a symbol for the alist containing FLAVOR's colors.
FLAVOR defaults to the value of `github-theme-flavor'."
  (intern-soft (concat "github-" (symbol-name (or flavor github-theme-flavor)) "-colors")))

(defun github-theme-set-color (color value &optional flavor)
  "Set the COLOR of FLAVOR (or current flavor) to VALUE."
  (interactive "SChange color: \nsSet %s to: ")
  (setcdr (assoc color (symbol-value (github-theme--colors-of flavor))) value))

(defun github-theme-color (color &optional flavor)
  "Get the COLOR of FLAVOR or the current flavor."
  (interactive "SColor: ")
  (let ((result (alist-get color (symbol-value (github-theme--colors-of flavor)))))
    (if (called-interactively-p 'interactive)
        (message result)
      result)))

(defalias 'github-theme-get-color 'github-theme-color)

;;; Theme definition:

(let*
    ((colors
      '((undef "#ff00ff" "#ff00ff")
        ;; Foreground colors
        (gh-fg-default (github-theme-color 'fg-default)
                       (github-theme-quantize-color (github-theme-color 'fg-default)))
        (gh-fg-muted (github-theme-color 'fg-muted)
                     (github-theme-quantize-color (github-theme-color 'fg-muted)))
        (gh-fg-subtle (github-theme-color 'fg-subtle)
                      (github-theme-quantize-color (github-theme-color 'fg-subtle)))
        (gh-fg-on-emphasis (github-theme-color 'fg-on-emphasis)
                           (github-theme-quantize-color (github-theme-color 'fg-on-emphasis)))

        ;; Canvas/Background colors
        (gh-canvas-default (github-theme-color 'canvas-default)
                           (github-theme-quantize-color (github-theme-color 'canvas-default)))
        (gh-canvas-overlay (github-theme-color 'canvas-overlay)
                           (github-theme-quantize-color (github-theme-color 'canvas-overlay)))
        (gh-canvas-inset (github-theme-color 'canvas-inset)
                         (github-theme-quantize-color (github-theme-color 'canvas-inset)))
        (gh-canvas-subtle (github-theme-color 'canvas-subtle)
                          (github-theme-quantize-color (github-theme-color 'canvas-subtle)))

        ;; Border colors
        (gh-border-default (github-theme-color 'border-default)
                           (github-theme-quantize-color (github-theme-color 'border-default)))
        (gh-border-muted (github-theme-color 'border-muted)
                         (github-theme-quantize-color (github-theme-color 'border-muted)))

        ;; Neutral colors
        (gh-neutral-emphasis (github-theme-color 'neutral-emphasis)
                             (github-theme-quantize-color (github-theme-color 'neutral-emphasis)))
        (gh-neutral-muted (github-theme-color 'neutral-muted)
                          (github-theme-quantize-color (github-theme-color 'neutral-muted)))
        (gh-neutral-subtle (github-theme-color 'neutral-subtle)
                           (github-theme-quantize-color (github-theme-color 'neutral-subtle)))

        ;; Accent colors
        (gh-accent-fg (github-theme-color 'accent-fg)
                      (github-theme-quantize-color (github-theme-color 'accent-fg)))
        (gh-accent-emphasis (github-theme-color 'accent-emphasis)
                            (github-theme-quantize-color (github-theme-color 'accent-emphasis)))
        (gh-accent-muted (github-theme-color 'accent-muted)
                         (github-theme-quantize-color (github-theme-color 'accent-muted)))
        (gh-accent-subtle (github-theme-color 'accent-subtle)
                          (github-theme-quantize-color (github-theme-color 'accent-subtle)))

        ;; Success colors
        (gh-success-fg (github-theme-color 'success-fg)
                       (github-theme-quantize-color (github-theme-color 'success-fg)))
        (gh-success-emphasis (github-theme-color 'success-emphasis)
                             (github-theme-quantize-color (github-theme-color 'success-emphasis)))

        ;; Attention colors
        (gh-attention-fg (github-theme-color 'attention-fg)
                         (github-theme-quantize-color (github-theme-color 'attention-fg)))
        (gh-attention-emphasis (github-theme-color 'attention-emphasis)
                               (github-theme-quantize-color (github-theme-color 'attention-emphasis)))

        ;; Danger colors
        (gh-danger-fg (github-theme-color 'danger-fg)
                      (github-theme-quantize-color (github-theme-color 'danger-fg)))
        (gh-danger-emphasis (github-theme-color 'danger-emphasis)
                            (github-theme-quantize-color (github-theme-color 'danger-emphasis)))

        ;; Done colors
        (gh-done-fg (github-theme-color 'done-fg)
                    (github-theme-quantize-color (github-theme-color 'done-fg)))

        ;; Syntax colors
        (gh-syntax-comment (github-theme-color 'syntax-comment)
                           (github-theme-quantize-color (github-theme-color 'syntax-comment)))
        (gh-syntax-constant (github-theme-color 'syntax-constant)
                            (github-theme-quantize-color (github-theme-color 'syntax-constant)))
        (gh-syntax-entity (github-theme-color 'syntax-entity)
                          (github-theme-quantize-color (github-theme-color 'syntax-entity)))
        (gh-syntax-keyword (github-theme-color 'syntax-keyword)
                           (github-theme-quantize-color (github-theme-color 'syntax-keyword)))
        (gh-syntax-string (github-theme-color 'syntax-string)
                          (github-theme-quantize-color (github-theme-color 'syntax-string)))
        (gh-syntax-variable (github-theme-color 'syntax-variable)
                            (github-theme-quantize-color (github-theme-color 'syntax-variable)))
        (gh-syntax-tag (github-theme-color 'syntax-tag)
                       (github-theme-quantize-color (github-theme-color 'syntax-tag)))
        (gh-syntax-func (github-theme-color 'syntax-func)
                        (github-theme-quantize-color (github-theme-color 'syntax-func)))
        (gh-syntax-param (github-theme-color 'syntax-param)
                         (github-theme-quantize-color (github-theme-color 'syntax-param)))
        (gh-syntax-invalid (github-theme-color 'syntax-invalid)
                           (github-theme-quantize-color (github-theme-color 'syntax-invalid)))

        ;; Git colors
        (gh-git-added (github-theme-color 'git-added)
                      (github-theme-quantize-color (github-theme-color 'git-added)))
        (gh-git-modified (github-theme-color 'git-modified)
                         (github-theme-quantize-color (github-theme-color 'git-modified)))
        (gh-git-deleted (github-theme-color 'git-deleted)
                        (github-theme-quantize-color (github-theme-color 'git-deleted)))
        (gh-git-untracked (github-theme-color 'git-untracked)
                          (github-theme-quantize-color (github-theme-color 'git-untracked)))
        (gh-git-ignored (github-theme-color 'git-ignored)
                        (github-theme-quantize-color (github-theme-color 'git-ignored)))
        (gh-git-conflict (github-theme-color 'git-conflict)
                         (github-theme-quantize-color (github-theme-color 'git-conflict)))

        ;; Diff colors
        (gh-diff-add-bg (github-theme-color 'diff-add-bg)
                        (github-theme-quantize-color (github-theme-color 'diff-add-bg)))
        (gh-diff-add-fg (github-theme-color 'diff-add-fg)
                        (github-theme-quantize-color (github-theme-color 'diff-add-fg)))
        (gh-diff-remove-bg (github-theme-color 'diff-remove-bg)
                           (github-theme-quantize-color (github-theme-color 'diff-remove-bg)))
        (gh-diff-remove-fg (github-theme-color 'diff-remove-fg)
                           (github-theme-quantize-color (github-theme-color 'diff-remove-fg)))
        (gh-diff-change-bg (github-theme-color 'diff-change-bg)
                           (github-theme-quantize-color (github-theme-color 'diff-change-bg)))
        (gh-diff-change-fg (github-theme-color 'diff-change-fg)
                           (github-theme-quantize-color (github-theme-color 'diff-change-fg)))

        ;; Editor UI
        (gh-cursor (github-theme-color 'cursor)
                   (github-theme-quantize-color (github-theme-color 'cursor)))
        (gh-selection (github-theme-color 'selection)
                      (github-theme-quantize-color (github-theme-color 'selection)))
        (gh-match (github-theme-color 'match)
                  (github-theme-quantize-color (github-theme-color 'match)))
        (gh-line-highlight (github-theme-color 'line-highlight)
                           (github-theme-quantize-color (github-theme-color 'line-highlight)))
        (gh-line-number (github-theme-color 'line-number)
                        (github-theme-quantize-color (github-theme-color 'line-number)))
        (gh-line-number-active (github-theme-color 'line-number-active)
                               (github-theme-quantize-color (github-theme-color 'line-number-active)))
        (gh-whitespace (github-theme-color 'whitespace)
                       (github-theme-quantize-color (github-theme-color 'whitespace)))

        ;; Bracket colors
        (gh-bracket-1 (github-theme-color 'bracket-1)
                      (github-theme-quantize-color (github-theme-color 'bracket-1)))
        (gh-bracket-2 (github-theme-color 'bracket-2)
                      (github-theme-quantize-color (github-theme-color 'bracket-2)))
        (gh-bracket-3 (github-theme-color 'bracket-3)
                      (github-theme-quantize-color (github-theme-color 'bracket-3)))
        (gh-bracket-4 (github-theme-color 'bracket-4)
                      (github-theme-quantize-color (github-theme-color 'bracket-4)))
        (gh-bracket-5 (github-theme-color 'bracket-5)
                      (github-theme-quantize-color (github-theme-color 'bracket-5)))
        (gh-bracket-6 (github-theme-color 'bracket-6)
                      (github-theme-quantize-color (github-theme-color 'bracket-6)))

        ;; ANSI colors
        (gh-ansi-black (github-theme-color 'ansi-black)
                       (github-theme-quantize-color (github-theme-color 'ansi-black)))
        (gh-ansi-red (github-theme-color 'ansi-red)
                     (github-theme-quantize-color (github-theme-color 'ansi-red)))
        (gh-ansi-green (github-theme-color 'ansi-green)
                       (github-theme-quantize-color (github-theme-color 'ansi-green)))
        (gh-ansi-yellow (github-theme-color 'ansi-yellow)
                        (github-theme-quantize-color (github-theme-color 'ansi-yellow)))
        (gh-ansi-blue (github-theme-color 'ansi-blue)
                      (github-theme-quantize-color (github-theme-color 'ansi-blue)))
        (gh-ansi-magenta (github-theme-color 'ansi-magenta)
                         (github-theme-quantize-color (github-theme-color 'ansi-magenta)))
        (gh-ansi-cyan (github-theme-color 'ansi-cyan)
                      (github-theme-quantize-color (github-theme-color 'ansi-cyan)))
        (gh-ansi-white (github-theme-color 'ansi-white)
                       (github-theme-quantize-color (github-theme-color 'ansi-white)))
        (gh-ansi-bright-black (github-theme-color 'ansi-bright-black)
                              (github-theme-quantize-color (github-theme-color 'ansi-bright-black)))
        (gh-ansi-bright-red (github-theme-color 'ansi-bright-red)
                            (github-theme-quantize-color (github-theme-color 'ansi-bright-red)))
        (gh-ansi-bright-green (github-theme-color 'ansi-bright-green)
                              (github-theme-quantize-color (github-theme-color 'ansi-bright-green)))
        (gh-ansi-bright-yellow (github-theme-color 'ansi-bright-yellow)
                               (github-theme-quantize-color (github-theme-color 'ansi-bright-yellow)))
        (gh-ansi-bright-blue (github-theme-color 'ansi-bright-blue)
                             (github-theme-quantize-color (github-theme-color 'ansi-bright-blue)))
        (gh-ansi-bright-magenta (github-theme-color 'ansi-bright-magenta)
                                (github-theme-quantize-color (github-theme-color 'ansi-bright-magenta)))
        (gh-ansi-bright-cyan (github-theme-color 'ansi-bright-cyan)
                             (github-theme-quantize-color (github-theme-color 'ansi-bright-cyan)))
        (gh-ansi-bright-white (github-theme-color 'ansi-bright-white)
                              (github-theme-quantize-color (github-theme-color 'ansi-bright-white)))

        ;; Computed colors
        (gh-current (if (github-theme--light-p)
                        (github-theme-darken (github-theme-color 'canvas-default) 3)
                      (github-theme-lighten (github-theme-color 'canvas-default) 5))
                    (github-theme-quantize-color
                     (if (github-theme--light-p)
                         (github-theme-darken (github-theme-color 'canvas-default) 3)
                       (github-theme-lighten (github-theme-color 'canvas-default) 5))))))
     (faces
      '(
        ;;; Core faces

        ;; default / basic faces
        (cursor :background ,gh-cursor)
        (default :background ,gh-canvas-default :foreground ,gh-fg-default)
        (italic :slant italic)
        (default-italic :slant italic)
        (error :foreground ,gh-danger-fg)
        (success :foreground ,gh-success-fg)
        (warning :foreground ,gh-attention-fg)
        (fringe :background ,gh-canvas-default :foreground ,gh-fg-muted)
        (header-line :inherit mode-line)
        (help-key-binding :background ,gh-canvas-inset :foreground ,gh-accent-fg
                          :box (:line-width (-1 . -1) :color ,gh-border-default :style nil))
        (highlight :foreground ,gh-fg-default :background ,gh-current)
        (hl-line :background ,gh-line-highlight :extend t)
        (hl-todo :foreground ,gh-attention-fg)
        (lazy-highlight :foreground ,gh-fg-default :background ,gh-neutral-muted)
        (link :foreground ,gh-accent-fg :underline t)
        (link-visited :foreground ,gh-done-fg :underline t)
        (linum :inherit default :foreground ,gh-line-number :background ,gh-canvas-default)
        (line-number :inherit default :foreground ,gh-line-number :background ,gh-canvas-default)
        (line-number-current-line :inherit line-number :foreground ,gh-line-number-active)
        (match :background ,gh-match :foreground ,gh-fg-on-emphasis)
        (menu :background ,gh-current :inverse-video nil :foreground ,gh-fg-default)
        (minibuffer-prompt :weight normal :foreground ,gh-fg-muted)
        (mode-line :background ,gh-canvas-inset :foreground ,gh-fg-default)
        (mode-line-inactive :background ,gh-canvas-subtle :foreground ,gh-fg-muted)
        (read-multiple-choice-face :inherit completions-first-difference)
        (region :background ,gh-selection :extend t)
        (shadow :foreground ,gh-fg-muted)
        (tooltip :foreground ,gh-fg-default :background ,gh-canvas-overlay)
        (trailing-whitespace :background ,gh-danger-fg)
        (window-divider :foreground ,gh-border-muted)
        (vertical-border :foreground ,gh-border-muted)
        (whitespace-space :foreground ,gh-whitespace)
        (whitespace-tab :foreground ,gh-whitespace)
        (whitespace-newline :foreground ,gh-whitespace)

        ;; syntax / font-lock
        (font-lock-bracket-face :foreground ,gh-fg-muted)
        (font-lock-builtin-face :foreground ,gh-syntax-constant)
        (font-lock-comment-face ,@(if github-theme-italic-comments
                                      '(:inherit (shadow italic))
                                    '(:inherit shadow)))
        (font-lock-comment-delimiter-face :inherit shadow)
        (font-lock-constant-face :foreground ,gh-syntax-constant)
        (font-lock-delimiter-face :foreground ,gh-fg-muted)
        (font-lock-doc-face :inherit font-lock-comment-face)
        (font-lock-escape-face :foreground ,gh-syntax-keyword)
        (font-lock-function-call-face :foreground ,gh-syntax-func)
        (font-lock-function-name-face :foreground ,gh-syntax-func)
        (font-lock-keyword-face :foreground ,gh-syntax-keyword)
        (font-lock-negation-char-face :foreground ,gh-syntax-keyword)
        (font-lock-number-face :foreground ,gh-syntax-constant)
        (font-lock-operator-face :foreground ,gh-syntax-keyword)
        (font-lock-preprocessor-face :foreground ,gh-syntax-keyword)
        (font-lock-property-name-face :foreground ,gh-syntax-constant)
        (font-lock-reference-face :inherit font-lock-constant-face)
        (font-lock-regexp-grouping-backslash :foreground ,gh-syntax-keyword)
        (font-lock-regexp-grouping-construct :foreground ,gh-syntax-keyword)
        (font-lock-string-face :foreground ,gh-syntax-string)
        (font-lock-type-face :foreground ,gh-syntax-variable)
        (font-lock-variable-name-face :foreground ,gh-fg-default
                                      ,@(when github-theme-italic-variables '(:inherit italic)))
        (font-lock-variable-use-face :foreground ,gh-fg-default
                                     ,@(when github-theme-italic-variables '(:inherit italic)))
        (font-lock-warning-face :inherit warning)

        ;;; Extended faces

        ;; ansi-color
        (ansi-color-black :foreground ,gh-ansi-black :background ,gh-ansi-black)
        (ansi-color-red :foreground ,gh-ansi-red :background ,gh-ansi-red)
        (ansi-color-green :foreground ,gh-ansi-green :background ,gh-ansi-green)
        (ansi-color-yellow :foreground ,gh-ansi-yellow :background ,gh-ansi-yellow)
        (ansi-color-blue :foreground ,gh-ansi-blue :background ,gh-ansi-blue)
        (ansi-color-magenta :foreground ,gh-ansi-magenta :background ,gh-ansi-magenta)
        (ansi-color-cyan :foreground ,gh-ansi-cyan :background ,gh-ansi-cyan)
        (ansi-color-white :foreground ,gh-ansi-white :background ,gh-ansi-white)
        (ansi-color-bright-black :foreground ,gh-ansi-bright-black :background ,gh-ansi-bright-black)
        (ansi-color-bright-red :foreground ,gh-ansi-bright-red :background ,gh-ansi-bright-red)
        (ansi-color-bright-green :foreground ,gh-ansi-bright-green :background ,gh-ansi-bright-green)
        (ansi-color-bright-yellow :foreground ,gh-ansi-bright-yellow :background ,gh-ansi-bright-yellow)
        (ansi-color-bright-blue :foreground ,gh-ansi-bright-blue :background ,gh-ansi-bright-blue)
        (ansi-color-bright-magenta :foreground ,gh-ansi-bright-magenta :background ,gh-ansi-bright-magenta)
        (ansi-color-bright-cyan :foreground ,gh-ansi-bright-cyan :background ,gh-ansi-bright-cyan)
        (ansi-color-bright-white :foreground ,gh-ansi-bright-white :background ,gh-ansi-bright-white)

        ;; avy
        (avy-background-face :foreground ,gh-fg-muted :background ,gh-canvas-default)
        (avy-goto-char-timer-face :foreground ,gh-accent-fg :background ,gh-canvas-subtle)
        (avy-lead-face :foreground ,gh-fg-on-emphasis :background ,gh-syntax-keyword)
        (avy-lead-face-0 :foreground ,gh-fg-on-emphasis :background ,gh-attention-fg)
        (avy-lead-face-1 :foreground ,gh-fg-on-emphasis :background ,gh-fg-muted)
        (avy-lead-face-2 :foreground ,gh-fg-on-emphasis :background ,gh-accent-fg)

        ;; company
        (company-echo-common :foreground ,gh-canvas-default :background ,gh-fg-default)
        (company-preview :inherit shadow)
        (company-preview-common :inherit company-preview :foreground ,gh-success-fg)
        (company-preview-search :inherit company-preview :foreground ,gh-danger-fg)
        (company-tooltip :inherit tooltip)
        (company-tooltip-search :inherit lazy-highlight)
        (company-tooltip-search-selection :inherit match)
        (company-tooltip-selection :background ,gh-neutral-muted :foreground ,gh-fg-default)
        (company-tooltip-mouse :background ,gh-canvas-default)
        (company-tooltip-common :foreground ,gh-fg-default :weight bold)
        (company-tooltip-common-selection :foreground ,gh-fg-default :weight bold)
        (company-tooltip-annotation :foreground ,gh-success-fg)
        (company-tooltip-annotation-selection :foreground ,gh-fg-default)
        (company-tooltip-scrollbar-thumb :background ,gh-neutral-emphasis)
        (company-tooltip-scrollbar-track :background ,gh-canvas-subtle)

        ;; completions (minibuffer.el)
        (completions-annotations :inherit font-lock-comment-face)
        (completions-common-part :foreground ,gh-accent-fg)
        (completions-first-difference :foreground ,gh-fg-default)

        ;; corfu
        (corfu-default :background ,gh-canvas-overlay :foreground ,gh-fg-default)
        (corfu-current :background ,gh-neutral-muted :foreground ,gh-fg-default)
        (corfu-bar :background ,gh-neutral-emphasis)
        (corfu-border :background ,gh-border-default)
        (corfu-annotations :foreground ,gh-fg-muted)

        ;; diff-hl
        (diff-hl-change :background ,gh-git-modified :foreground ,gh-git-modified)
        (diff-hl-delete :background ,gh-git-deleted :foreground ,gh-git-deleted)
        (diff-hl-insert :background ,gh-git-added :foreground ,gh-git-added)

        ;; diff-mode
        (diff-header :foreground ,gh-accent-fg)
        (diff-hunk-header :foreground ,gh-fg-default :background ,gh-canvas-subtle)
        (diff-added :background ,gh-diff-add-bg :foreground ,gh-diff-add-fg)
        (diff-removed :background ,gh-diff-remove-bg :foreground ,gh-diff-remove-fg)
        (diff-indicator-added :foreground ,gh-git-added)
        (diff-indicator-removed :foreground ,gh-git-deleted)
        (diff-refine-added :background ,gh-success-emphasis :foreground ,gh-fg-on-emphasis)
        (diff-refine-removed :background ,gh-danger-emphasis :foreground ,gh-fg-on-emphasis)
        (diff-refine-changed :background ,gh-attention-emphasis :foreground ,gh-fg-on-emphasis)

        ;; dired
        (dired-flagged :foreground ,gh-danger-fg :weight bold)
        (dired-marked :weight bold)
        (dired-mark :inherit dired-marked)
        (dired-header :foreground ,gh-accent-fg :weight bold)
        (dired-ignored :inherit font-lock-comment-face)
        (dired-special :foreground ,gh-attention-fg)
        (dired-symlink :foreground ,gh-done-fg)
        (dired-warning :inherit warning)
        (dired-directory :foreground ,gh-accent-fg)
        (dired-perm-write :foreground ,gh-success-fg)
        (dired-broken-symlink :foreground ,gh-fg-on-emphasis :background ,gh-danger-fg)

        ;; ediff
        (ediff-current-diff-A :background ,gh-diff-remove-bg)
        (ediff-current-diff-B :background ,gh-diff-add-bg)
        (ediff-current-diff-C :background ,gh-diff-change-bg)
        (ediff-fine-diff-A :background ,gh-danger-emphasis :foreground ,gh-fg-on-emphasis)
        (ediff-fine-diff-B :background ,gh-success-emphasis :foreground ,gh-fg-on-emphasis)
        (ediff-fine-diff-C :background ,gh-attention-emphasis :foreground ,gh-fg-on-emphasis)
        (ediff-odd-diff-A :inherit diff-removed)
        (ediff-odd-diff-B :inherit diff-added)
        (ediff-even-diff-A :inherit diff-removed)
        (ediff-even-diff-B :inherit diff-added)

        ;; eshell
        (eshell-ls-archive :foreground ,gh-done-fg)
        (eshell-ls-backup :foreground ,gh-attention-fg)
        (eshell-ls-clutter :foreground ,gh-danger-fg :weight bold)
        (eshell-ls-directory :foreground ,gh-accent-fg :weight bold)
        (eshell-ls-executable :foreground ,gh-success-fg :weight bold)
        (eshell-ls-missing :foreground ,gh-danger-fg :weight bold)
        (eshell-ls-product :foreground ,gh-syntax-variable)
        (eshell-ls-readonly :foreground ,gh-syntax-constant)
        (eshell-ls-special :foreground ,gh-done-fg :weight bold)
        (eshell-ls-symlink :foreground ,gh-accent-fg :weight bold)
        (eshell-prompt :foreground ,gh-accent-fg :weight bold)

        ;; flycheck
        (flycheck-error :underline (:style wave :color ,gh-danger-fg))
        (flycheck-warning :underline (:style wave :color ,gh-attention-fg))
        (flycheck-info :underline (:style wave :color ,gh-accent-fg))
        (flycheck-fringe-error :foreground ,gh-danger-fg)
        (flycheck-fringe-warning :foreground ,gh-attention-fg)
        (flycheck-fringe-info :foreground ,gh-accent-fg)

        ;; flymake
        (flymake-error :underline (:style wave :color ,gh-danger-fg))
        (flymake-warning :underline (:style wave :color ,gh-attention-fg))
        (flymake-note :underline (:style wave :color ,gh-accent-fg))

        ;; git-gutter
        (git-gutter:modified :foreground ,gh-git-modified)
        (git-gutter:deleted :foreground ,gh-git-deleted)
        (git-gutter:added :foreground ,gh-git-added)
        (git-gutter:separator :inherit font-lock-comment-face)
        (git-gutter:unchanged :foreground ,gh-canvas-subtle)
        (git-gutter-fr:modified :inherit git-gutter:modified)
        (git-gutter-fr:deleted :inherit git-gutter:deleted)
        (git-gutter-fr:added :inherit git-gutter:added)

        ;; isearch
        (isearch :foreground ,gh-fg-on-emphasis :background ,gh-attention-emphasis)
        (isearch-fail :foreground ,gh-danger-fg :background ,gh-canvas-default)

        ;; ivy
        (ivy-current-match :background ,gh-neutral-muted :foreground ,gh-fg-default)
        (ivy-minibuffer-match-face-1 :foreground ,gh-accent-fg)
        (ivy-minibuffer-match-face-2 :foreground ,gh-success-fg)
        (ivy-minibuffer-match-face-3 :foreground ,gh-attention-fg)
        (ivy-minibuffer-match-face-4 :foreground ,gh-done-fg)
        (ivy-confirm-face :foreground ,gh-success-fg)
        (ivy-match-required-face :foreground ,gh-danger-fg)
        (ivy-virtual :inherit font-lock-comment-face)
        (ivy-modified-buffer :foreground ,gh-attention-fg)

        ;; lsp-mode
        (lsp-face-highlight-read :background ,gh-neutral-subtle)
        (lsp-face-highlight-textual :background ,gh-neutral-subtle)
        (lsp-face-highlight-write :background ,gh-neutral-subtle)
        (lsp-headerline-breadcrumb-path-face :foreground ,gh-fg-muted)
        (lsp-headerline-breadcrumb-separator-face :foreground ,gh-fg-muted)
        (lsp-headerline-breadcrumb-symbols-face :foreground ,gh-accent-fg)

        ;; magit
        (magit-bisect-bad :foreground ,gh-danger-fg)
        (magit-bisect-good :foreground ,gh-success-fg)
        (magit-bisect-skip :foreground ,gh-attention-fg)
        (magit-blame-heading :foreground ,gh-fg-muted :background ,gh-canvas-subtle)
        (magit-branch-local :foreground ,gh-accent-fg)
        (magit-branch-remote :foreground ,gh-success-fg)
        (magit-branch-current :foreground ,gh-accent-fg :box t)
        (magit-cherry-equivalent :foreground ,gh-done-fg)
        (magit-cherry-unmatched :foreground ,gh-accent-fg)
        (magit-diff-added :inherit diff-added)
        (magit-diff-added-highlight :inherit diff-added :background ,gh-diff-add-bg)
        (magit-diff-removed :inherit diff-removed)
        (magit-diff-removed-highlight :inherit diff-removed :background ,gh-diff-remove-bg)
        (magit-diff-base :foreground ,gh-diff-change-fg :background ,gh-diff-change-bg)
        (magit-diff-base-highlight :foreground ,gh-diff-change-fg :background ,gh-diff-change-bg)
        (magit-diff-context :foreground ,gh-fg-muted)
        (magit-diff-context-highlight :foreground ,gh-fg-default :background ,gh-canvas-subtle)
        (magit-diff-file-heading :foreground ,gh-fg-default :weight bold)
        (magit-diff-file-heading-highlight :foreground ,gh-fg-default :background ,gh-canvas-subtle)
        (magit-diff-file-heading-selection :foreground ,gh-accent-fg)
        (magit-diff-hunk-heading :foreground ,gh-fg-default :background ,gh-canvas-inset)
        (magit-diff-hunk-heading-highlight :foreground ,gh-fg-default :background ,gh-neutral-muted)
        (magit-diff-hunk-heading-selection :foreground ,gh-accent-fg)
        (magit-diff-lines-heading :foreground ,gh-fg-on-emphasis :background ,gh-accent-emphasis)
        (magit-diffstat-added :foreground ,gh-git-added)
        (magit-diffstat-removed :foreground ,gh-git-deleted)
        (magit-dimmed :foreground ,gh-fg-muted)
        (magit-filename :foreground ,gh-fg-default)
        (magit-hash :foreground ,gh-fg-muted)
        (magit-header-line :inherit header-line)
        (magit-log-author :foreground ,gh-syntax-variable)
        (magit-log-date :foreground ,gh-fg-muted)
        (magit-log-graph :foreground ,gh-fg-muted)
        (magit-process-ng :foreground ,gh-danger-fg :weight bold)
        (magit-process-ok :foreground ,gh-success-fg :weight bold)
        (magit-reflog-amend :foreground ,gh-done-fg)
        (magit-reflog-checkout :foreground ,gh-accent-fg)
        (magit-reflog-cherry-pick :foreground ,gh-success-fg)
        (magit-reflog-commit :foreground ,gh-success-fg)
        (magit-reflog-merge :foreground ,gh-success-fg)
        (magit-reflog-other :foreground ,gh-accent-fg)
        (magit-reflog-rebase :foreground ,gh-done-fg)
        (magit-reflog-remote :foreground ,gh-accent-fg)
        (magit-reflog-reset :foreground ,gh-danger-fg)
        (magit-refname :foreground ,gh-fg-muted)
        (magit-section-heading :foreground ,gh-accent-fg :weight bold)
        (magit-section-heading-selection :foreground ,gh-syntax-variable)
        (magit-section-highlight :background ,gh-canvas-subtle)
        (magit-sequence-drop :foreground ,gh-danger-fg)
        (magit-sequence-head :foreground ,gh-accent-fg)
        (magit-sequence-part :foreground ,gh-attention-fg)
        (magit-sequence-stop :foreground ,gh-success-fg)
        (magit-signature-bad :foreground ,gh-danger-fg)
        (magit-signature-error :foreground ,gh-danger-fg)
        (magit-signature-expired :foreground ,gh-attention-fg)
        (magit-signature-good :foreground ,gh-success-fg)
        (magit-signature-revoked :foreground ,gh-done-fg)
        (magit-signature-untrusted :foreground ,gh-attention-fg)
        (magit-tag :foreground ,gh-attention-fg)

        ;; markdown-mode
        (markdown-blockquote-face ,@(if github-theme-italic-blockquotes
                                        '(:inherit italic :foreground ,gh-fg-muted)
                                      '(:foreground ,gh-fg-muted)))
        (markdown-bold-face :inherit bold)
        (markdown-code-face :foreground ,gh-syntax-string :background ,gh-canvas-subtle)
        (markdown-header-delimiter-face :foreground ,gh-fg-muted)
        (markdown-header-face :foreground ,gh-accent-fg :weight bold)
        (markdown-header-face-1 :foreground ,gh-accent-fg :weight bold :height 1.4)
        (markdown-header-face-2 :foreground ,gh-accent-fg :weight bold :height 1.3)
        (markdown-header-face-3 :foreground ,gh-accent-fg :weight bold :height 1.2)
        (markdown-header-face-4 :foreground ,gh-accent-fg :weight bold :height 1.1)
        (markdown-header-face-5 :foreground ,gh-accent-fg :weight bold)
        (markdown-header-face-6 :foreground ,gh-accent-fg :weight bold)
        (markdown-inline-code-face :foreground ,gh-syntax-string :background ,gh-canvas-subtle)
        (markdown-italic-face :inherit italic)
        (markdown-link-face :foreground ,gh-accent-fg)
        (markdown-list-face :foreground ,gh-syntax-variable)
        (markdown-markup-face :foreground ,gh-fg-muted)
        (markdown-pre-face :foreground ,gh-syntax-string :background ,gh-canvas-subtle)
        (markdown-url-face :foreground ,gh-accent-fg :underline t)

        ;; orderless
        (orderless-match-face-0 :foreground ,gh-accent-fg :weight bold)
        (orderless-match-face-1 :foreground ,gh-success-fg :weight bold)
        (orderless-match-face-2 :foreground ,gh-attention-fg :weight bold)
        (orderless-match-face-3 :foreground ,gh-done-fg :weight bold)

        ;; org-mode
        (org-block :background ,gh-canvas-subtle :extend t)
        (org-block-begin-line :foreground ,gh-fg-muted :background ,gh-canvas-subtle :extend t)
        (org-block-end-line :inherit org-block-begin-line)
        (org-code :foreground ,gh-syntax-string :background ,gh-canvas-subtle)
        (org-date :foreground ,gh-accent-fg :underline t)
        (org-document-info :foreground ,gh-fg-muted)
        (org-document-info-keyword :foreground ,gh-fg-muted)
        (org-document-title :foreground ,gh-fg-default :weight bold :height 1.5)
        (org-done :foreground ,gh-success-fg)
        (org-headline-done :foreground ,gh-fg-muted)
        (org-level-1 :foreground ,gh-accent-fg :weight bold :height 1.3)
        (org-level-2 :foreground ,gh-success-fg :weight bold :height 1.2)
        (org-level-3 :foreground ,gh-attention-fg :weight bold :height 1.1)
        (org-level-4 :foreground ,gh-done-fg :weight bold)
        (org-level-5 :foreground ,gh-danger-fg :weight bold)
        (org-level-6 :foreground ,gh-accent-fg :weight bold)
        (org-level-7 :foreground ,gh-success-fg :weight bold)
        (org-level-8 :foreground ,gh-attention-fg :weight bold)
        (org-link :foreground ,gh-accent-fg :underline t)
        (org-priority :foreground ,gh-attention-fg)
        (org-quote ,@(if github-theme-italic-blockquotes
                         '(:inherit italic :foreground ,gh-fg-muted)
                       '(:foreground ,gh-fg-muted)))
        (org-special-keyword :foreground ,gh-fg-muted)
        (org-table :foreground ,gh-fg-default)
        (org-tag :foreground ,gh-fg-muted)
        (org-todo :foreground ,gh-attention-fg)
        (org-verbatim :foreground ,gh-syntax-string :background ,gh-canvas-subtle)

        ;; rainbow-delimiters
        (rainbow-delimiters-depth-1-face :foreground ,gh-bracket-1)
        (rainbow-delimiters-depth-2-face :foreground ,gh-bracket-2)
        (rainbow-delimiters-depth-3-face :foreground ,gh-bracket-3)
        (rainbow-delimiters-depth-4-face :foreground ,gh-bracket-4)
        (rainbow-delimiters-depth-5-face :foreground ,gh-bracket-5)
        (rainbow-delimiters-depth-6-face :foreground ,gh-bracket-6)
        (rainbow-delimiters-depth-7-face :foreground ,gh-bracket-1)
        (rainbow-delimiters-depth-8-face :foreground ,gh-bracket-2)
        (rainbow-delimiters-depth-9-face :foreground ,gh-bracket-3)
        (rainbow-delimiters-unmatched-face :foreground ,gh-danger-fg)

        ;; show-paren
        (show-paren-match :foreground ,gh-fg-on-emphasis :background ,gh-accent-emphasis)
        (show-paren-mismatch :foreground ,gh-fg-on-emphasis :background ,gh-danger-emphasis)

        ;; term
        (term :foreground ,gh-fg-default :background ,gh-canvas-default)
        (term-color-black :foreground ,gh-ansi-black :background ,gh-ansi-black)
        (term-color-red :foreground ,gh-ansi-red :background ,gh-ansi-red)
        (term-color-green :foreground ,gh-ansi-green :background ,gh-ansi-green)
        (term-color-yellow :foreground ,gh-ansi-yellow :background ,gh-ansi-yellow)
        (term-color-blue :foreground ,gh-ansi-blue :background ,gh-ansi-blue)
        (term-color-magenta :foreground ,gh-ansi-magenta :background ,gh-ansi-magenta)
        (term-color-cyan :foreground ,gh-ansi-cyan :background ,gh-ansi-cyan)
        (term-color-white :foreground ,gh-ansi-white :background ,gh-ansi-white)

        ;; tree-sitter
        (tree-sitter-hl-face:attribute :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
        (tree-sitter-hl-face:constant :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:constant.builtin :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:constructor :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:doc :inherit font-lock-doc-face)
        (tree-sitter-hl-face:escape :foreground ,gh-syntax-keyword)
        (tree-sitter-hl-face:function :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:function.builtin :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:function.call :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:function.macro :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:function.method :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:function.method.call :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:function.special :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:keyword :foreground ,gh-syntax-keyword)
        (tree-sitter-hl-face:label :foreground ,gh-fg-default)
        (tree-sitter-hl-face:method :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:method.call :foreground ,gh-syntax-func)
        (tree-sitter-hl-face:number :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:operator :foreground ,gh-syntax-keyword)
        (tree-sitter-hl-face:property :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:property.definition :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:punctuation :foreground ,gh-fg-default)
        (tree-sitter-hl-face:punctuation.bracket :foreground ,gh-fg-muted)
        (tree-sitter-hl-face:punctuation.delimiter :foreground ,gh-fg-muted)
        (tree-sitter-hl-face:punctuation.special :foreground ,gh-syntax-keyword)
        (tree-sitter-hl-face:string :foreground ,gh-syntax-string)
        (tree-sitter-hl-face:string.special :foreground ,gh-syntax-string)
        (tree-sitter-hl-face:tag :foreground ,gh-syntax-tag)
        (tree-sitter-hl-face:type :foreground ,gh-syntax-variable)
        (tree-sitter-hl-face:type.argument :foreground ,gh-syntax-variable)
        (tree-sitter-hl-face:type.builtin :foreground ,gh-syntax-variable)
        (tree-sitter-hl-face:type.parameter :foreground ,gh-syntax-variable)
        (tree-sitter-hl-face:type.super :foreground ,gh-syntax-variable)
        (tree-sitter-hl-face:variable :foreground ,gh-fg-default)
        (tree-sitter-hl-face:variable.builtin :foreground ,gh-syntax-constant)
        (tree-sitter-hl-face:variable.parameter :foreground ,gh-fg-default)
        (tree-sitter-hl-face:variable.special :foreground ,gh-syntax-constant)

        ;; treemacs
        (treemacs-directory-face :foreground ,gh-fg-default)
        (treemacs-file-face :foreground ,gh-fg-default)
        (treemacs-git-added-face :foreground ,gh-git-added)
        (treemacs-git-conflict-face :foreground ,gh-git-conflict)
        (treemacs-git-ignored-face :foreground ,gh-git-ignored)
        (treemacs-git-modified-face :foreground ,gh-git-modified)
        (treemacs-git-renamed-face :foreground ,gh-git-modified)
        (treemacs-git-untracked-face :foreground ,gh-git-untracked)
        (treemacs-root-face :foreground ,gh-accent-fg :weight bold :height 1.2)

        ;; vertico
        (vertico-current :background ,gh-neutral-muted :extend t)
        (vertico-group-separator :foreground ,gh-border-default)
        (vertico-group-title :foreground ,gh-fg-muted)

        ;; which-key
        (which-key-key-face :foreground ,gh-accent-fg)
        (which-key-group-description-face :foreground ,gh-done-fg)
        (which-key-command-description-face :foreground ,gh-fg-default)
        (which-key-local-map-description-face :foreground ,gh-success-fg)
        (which-key-separator-face :foreground ,gh-fg-muted))))
  (apply #'custom-theme-set-faces
         'github
         (let* ((expand-with-func
                 (lambda (func spec)
                   (let (reduced-color-list)
                     (dolist (col colors reduced-color-list)
                       (push (list (car col) (funcall func col))
                             reduced-color-list))
                     (eval `(let ,reduced-color-list
                              (backquote ,spec))))))
                whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216))
                       ,(funcall expand-with-func #'cadr spec))
                      (t
                       ,(funcall expand-with-func #'caddr spec))))
                   whole-theme))
           whole-theme)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'github)
(provide 'github-theme)

;;; github-theme.el ends here
