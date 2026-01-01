;;; github-definitions.el --- The GitHub color palette -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

;; Color palette definitions for the GitHub theme.
;; Colors extracted from the official GitHub VS Code theme (primer/github-vscode-theme).

;;; Code:

((light .
  ;; GitHub Light Default
  ((fg-default . "#1f2328")
   (fg-muted . "#656d76")
   (fg-subtle . "#6e7781")
   (fg-on-emphasis . "#ffffff")

   (canvas-default . "#ffffff")
   (canvas-overlay . "#ffffff")
   (canvas-inset . "#f6f8fa")
   (canvas-subtle . "#f6f8fa")

   (border-default . "#d0d7de")
   (border-muted . "#d8dee4")

   (neutral-emphasis . "#6e7781")
   (neutral-emphasis-plus . "#24292f")
   (neutral-muted . "#eff1f3")
   (neutral-subtle . "#f5f7f9")

   (accent-fg . "#0969da")
   (accent-emphasis . "#0969da")
   (accent-muted . "#bbdfff")
   (accent-subtle . "#ddf4ff")

   (success-fg . "#1a7f37")
   (success-emphasis . "#1f883d")
   (success-muted . "#b7e7c4")
   (success-subtle . "#dafbe1")

   (attention-fg . "#9a6700")
   (attention-emphasis . "#bf8700")
   (attention-muted . "#eedcab")
   (attention-subtle . "#fff8c5")

   (danger-fg . "#cf222e")
   (danger-emphasis . "#cf222e")
   (danger-muted . "#ffcdcd")
   (danger-subtle . "#ffebe9")

   (done-fg . "#8250df")
   (done-emphasis . "#8250df")
   (done-muted . "#e7d5ff")
   (done-subtle . "#fbefff")

   ;; Syntax highlighting (from tokenColors)
   (syntax-comment . "#6e7781")
   (syntax-constant . "#0550ae")
   (syntax-entity . "#8250df")
   (syntax-keyword . "#cf222e")
   (syntax-string . "#0a3069")
   (syntax-variable . "#953800")
   (syntax-tag . "#116329")
   (syntax-func . "#8250df")
   (syntax-param . "#1f2328")
   (syntax-invalid . "#82071e")

   ;; Git decoration
   (git-added . "#1a7f37")
   (git-modified . "#9a6700")
   (git-deleted . "#cf222e")
   (git-untracked . "#1a7f37")
   (git-ignored . "#6e7781")
   (git-conflict . "#bc4c00")

   ;; Diff colors
   (diff-add-bg . "#dafbe1")
   (diff-add-fg . "#116329")
   (diff-remove-bg . "#ffebe9")
   (diff-remove-fg . "#82071e")
   (diff-change-bg . "#ffd8b5")
   (diff-change-fg . "#953800")

   ;; Editor UI
   (cursor . "#0969da")
   (selection . "#eff1f3")
   (match . "#fdf0be")
   (line-highlight . "#f5f7f9")
   (line-number . "#8c959f")
   (line-number-active . "#1f2328")
   (indent-guide . "#e4e4e5")
   (indent-guide-active . "#cacacc")
   (whitespace . "#afb8c1")

   ;; Bracket highlight
   (bracket-1 . "#0969da")
   (bracket-2 . "#1a7f37")
   (bracket-3 . "#9a6700")
   (bracket-4 . "#cf222e")
   (bracket-5 . "#bf3989")
   (bracket-6 . "#8250df")

   ;; Terminal ANSI colors
   (ansi-black . "#24292f")
   (ansi-red . "#cf222e")
   (ansi-green . "#116329")
   (ansi-yellow . "#4d2d00")
   (ansi-blue . "#0969da")
   (ansi-magenta . "#8250df")
   (ansi-cyan . "#1b7c83")
   (ansi-white . "#6e7781")
   (ansi-bright-black . "#57606a")
   (ansi-bright-red . "#a40e26")
   (ansi-bright-green . "#1a7f37")
   (ansi-bright-yellow . "#633c01")
   (ansi-bright-blue . "#218bff")
   (ansi-bright-magenta . "#a475f9")
   (ansi-bright-cyan . "#3192aa")
   (ansi-bright-white . "#8c959f")))

 (light-high-contrast .
  ;; GitHub Light High Contrast
  ((fg-default . "#0e1116")
   (fg-muted . "#0e1116")
   (fg-subtle . "#66707b")
   (fg-on-emphasis . "#ffffff")

   (canvas-default . "#ffffff")
   (canvas-overlay . "#ffffff")
   (canvas-inset . "#ffffff")
   (canvas-subtle . "#e7ecf0")

   (border-default . "#20252c")
   (border-muted . "#88929d")

   (neutral-emphasis . "#66707b")
   (neutral-emphasis-plus . "#0e1116")
   (neutral-muted . "#eef0f2")
   (neutral-subtle . "#e7ecf0")

   (accent-fg . "#0349b4")
   (accent-emphasis . "#0349b4")
   (accent-muted . "#afd1fd")
   (accent-subtle . "#dff7ff")

   (success-fg . "#055d20")
   (success-emphasis . "#055d20")
   (success-muted . "#a8d9b6")
   (success-subtle . "#d2fedb")

   (attention-fg . "#744500")
   (attention-emphasis . "#744500")
   (attention-muted . "#e1ce9c")
   (attention-subtle . "#fcf7be")

   (danger-fg . "#a0111f")
   (danger-emphasis . "#a0111f")
   (danger-muted . "#f8bdbe")
   (danger-subtle . "#fff0ee")

   (done-fg . "#622cbc")
   (done-emphasis . "#622cbc")
   (done-muted . "#ceb7f5")
   (done-subtle . "#faf0fe")

   ;; Syntax highlighting
   (syntax-comment . "#66707b")
   (syntax-constant . "#023b95")
   (syntax-entity . "#622cbc")
   (syntax-keyword . "#a0111f")
   (syntax-string . "#032563")
   (syntax-variable . "#702c00")
   (syntax-tag . "#024c1a")
   (syntax-func . "#622cbc")
   (syntax-param . "#0e1116")
   (syntax-invalid . "#6e011a")

   ;; Git decoration
   (git-added . "#055d20")
   (git-modified . "#744500")
   (git-deleted . "#a0111f")
   (git-untracked . "#055d20")
   (git-ignored . "#66707b")
   (git-conflict . "#873800")

   ;; Diff colors
   (diff-add-bg . "#d2fedb")
   (diff-add-fg . "#024c1a")
   (diff-remove-bg . "#fff0ee")
   (diff-remove-fg . "#6e011a")
   (diff-change-bg . "#ffc67b")
   (diff-change-fg . "#702c00")

   ;; Editor UI
   (cursor . "#0349b4")
   (selection . "#0e1116")
   (selection-fg . "#ffffff")
   (match . "#f8e7a9")
   (line-highlight . "#e7ecf0")
   (line-number . "#88929d")
   (line-number-active . "#0e1116")
   (indent-guide . "#e2e2e3")
   (indent-guide-active . "#c5c6c7")
   (whitespace . "#acb6c0")

   ;; Bracket highlight
   (bracket-1 . "#0349b4")
   (bracket-2 . "#055d20")
   (bracket-3 . "#744500")
   (bracket-4 . "#a0111f")
   (bracket-5 . "#971368")
   (bracket-6 . "#622cbc")

   ;; Terminal ANSI colors
   (ansi-black . "#0e1116")
   (ansi-red . "#a0111f")
   (ansi-green . "#024c1a")
   (ansi-yellow . "#3f2200")
   (ansi-blue . "#0349b4")
   (ansi-magenta . "#622cbc")
   (ansi-cyan . "#1b7c83")
   (ansi-white . "#66707b")
   (ansi-bright-black . "#4b535d")
   (ansi-bright-red . "#86061d")
   (ansi-bright-green . "#055d20")
   (ansi-bright-yellow . "#4e2c00")
   (ansi-bright-blue . "#1168e3")
   (ansi-bright-magenta . "#844ae7")
   (ansi-bright-cyan . "#3192aa")
   (ansi-bright-white . "#88929d")))

 (dark .
  ;; GitHub Dark Default
  ((fg-default . "#e6edf3")
   (fg-muted . "#7d8590")
   (fg-subtle . "#6e7681")
   (fg-on-emphasis . "#ffffff")

   (canvas-default . "#0d1117")
   (canvas-overlay . "#161b22")
   (canvas-inset . "#010409")
   (canvas-subtle . "#161b22")

   (border-default . "#30363d")
   (border-muted . "#21262d")

   (neutral-emphasis . "#6e7681")
   (neutral-emphasis-plus . "#f0f6fc")
   (neutral-muted . "#343941")
   (neutral-subtle . "#171b22")

   (accent-fg . "#2f81f7")
   (accent-emphasis . "#1f6feb")
   (accent-muted . "#1e4273")
   (accent-subtle . "#13233a")

   (success-fg . "#3fb950")
   (success-emphasis . "#238636")
   (success-muted . "#1a4a29")
   (success-subtle . "#12261e")

   (attention-fg . "#d29922")
   (attention-emphasis . "#9e6a03")
   (attention-muted . "#533d11")
   (attention-subtle . "#272215")

   (danger-fg . "#f85149")
   (danger-emphasis . "#da3633")
   (danger-muted . "#6b2b2b")
   (danger-subtle . "#301b1f")

   (done-fg . "#a371f7")
   (done-emphasis . "#8957e5")
   (done-muted . "#493771")
   (done-subtle . "#241f39")

   ;; Syntax highlighting
   (syntax-comment . "#8b949e")
   (syntax-constant . "#79c0ff")
   (syntax-entity . "#d2a8ff")
   (syntax-keyword . "#ff7b72")
   (syntax-string . "#a5d6ff")
   (syntax-variable . "#ffa657")
   (syntax-tag . "#7ee787")
   (syntax-func . "#d2a8ff")
   (syntax-param . "#e6edf3")
   (syntax-invalid . "#ffa198")

   ;; Git decoration
   (git-added . "#3fb950")
   (git-modified . "#d29922")
   (git-deleted . "#f85149")
   (git-untracked . "#3fb950")
   (git-ignored . "#6e7681")
   (git-conflict . "#db6d28")

   ;; Diff colors
   (diff-add-bg . "#10231c")
   (diff-add-fg . "#7ee787")
   (diff-remove-bg . "#2c171b")
   (diff-remove-fg . "#ffa198")
   (diff-change-bg . "#5a1e02")
   (diff-change-fg . "#ffa657")

   ;; Editor UI
   (cursor . "#2f81f7")
   (selection . "#343941")
   (match . "#806f3c")
   (line-highlight . "#171b22")
   (line-number . "#6e7681")
   (line-number-active . "#e6edf3")
   (indent-guide . "#272b31")
   (indent-guide-active . "#41464c")
   (whitespace . "#484f58")

   ;; Bracket highlight
   (bracket-1 . "#79c0ff")
   (bracket-2 . "#56d364")
   (bracket-3 . "#e3b341")
   (bracket-4 . "#ffa198")
   (bracket-5 . "#ff9bce")
   (bracket-6 . "#d2a8ff")

   ;; Terminal ANSI colors
   (ansi-black . "#484f58")
   (ansi-red . "#ff7b72")
   (ansi-green . "#3fb950")
   (ansi-yellow . "#d29922")
   (ansi-blue . "#58a6ff")
   (ansi-magenta . "#bc8cff")
   (ansi-cyan . "#39c5cf")
   (ansi-white . "#b1bac4")
   (ansi-bright-black . "#6e7681")
   (ansi-bright-red . "#ffa198")
   (ansi-bright-green . "#56d364")
   (ansi-bright-yellow . "#e3b341")
   (ansi-bright-blue . "#79c0ff")
   (ansi-bright-magenta . "#d2a8ff")
   (ansi-bright-cyan . "#56d4dd")
   (ansi-bright-white . "#ffffff")))

 (dark-dimmed .
  ;; GitHub Dark Dimmed
  ((fg-default . "#adbac7")
   (fg-muted . "#768390")
   (fg-subtle . "#636e7b")
   (fg-on-emphasis . "#cdd9e5")

   (canvas-default . "#22272e")
   (canvas-overlay . "#2d333b")
   (canvas-inset . "#1c2128")
   (canvas-subtle . "#2d333b")

   (border-default . "#444c56")
   (border-muted . "#373e47")

   (neutral-emphasis . "#636e7b")
   (neutral-emphasis-plus . "#cdd9e5")
   (neutral-muted . "#3c434d")
   (neutral-subtle . "#292e36")

   (accent-fg . "#539bf5")
   (accent-emphasis . "#316dca")
   (accent-muted . "#2e4c77")
   (accent-subtle . "#273549")

   (success-fg . "#57ab5a")
   (success-emphasis . "#347d39")
   (success-muted . "#305339")
   (success-subtle . "#273732")

   (attention-fg . "#c69026")
   (attention-emphasis . "#966600")
   (attention-muted . "#5a4924")
   (attention-subtle . "#37342a")

   (danger-fg . "#e5534b")
   (danger-emphasis . "#c93c37")
   (danger-muted . "#70393a")
   (danger-subtle . "#3f2e32")

   (done-fg . "#986ee2")
   (done-emphasis . "#8256d0")
   (done-muted . "#514376")
   (done-subtle . "#343249")

   ;; Syntax highlighting
   (syntax-comment . "#768390")
   (syntax-constant . "#6cb6ff")
   (syntax-entity . "#dcbdfb")
   (syntax-keyword . "#f47067")
   (syntax-string . "#96d0ff")
   (syntax-variable . "#f69d50")
   (syntax-tag . "#8ddb8c")
   (syntax-func . "#dcbdfb")
   (syntax-param . "#adbac7")
   (syntax-invalid . "#ff938a")

   ;; Git decoration
   (git-added . "#57ab5a")
   (git-modified . "#c69026")
   (git-deleted . "#e5534b")
   (git-untracked . "#57ab5a")
   (git-ignored . "#636e7b")
   (git-conflict . "#cc6b2c")

   ;; Diff colors
   (diff-add-bg . "#253430")
   (diff-add-fg . "#8ddb8c")
   (diff-remove-bg . "#3b2a2f")
   (diff-remove-fg . "#ff938a")
   (diff-change-bg . "#682d0f")
   (diff-change-fg . "#f69d50")

   ;; Editor UI
   (cursor . "#539bf5")
   (selection . "#3c434d")
   (match . "#867647")
   (line-highlight . "#292e36")
   (line-number . "#636e7b")
   (line-number-active . "#adbac7")
   (indent-guide . "#333941")
   (indent-guide-active . "#434a53")
   (whitespace . "#545d68")

   ;; Bracket highlight
   (bracket-1 . "#6cb6ff")
   (bracket-2 . "#6bc46d")
   (bracket-3 . "#daaa3f")
   (bracket-4 . "#ff938a")
   (bracket-5 . "#fc8dc7")
   (bracket-6 . "#dcbdfb")

   ;; Terminal ANSI colors
   (ansi-black . "#545d68")
   (ansi-red . "#f47067")
   (ansi-green . "#57ab5a")
   (ansi-yellow . "#c69026")
   (ansi-blue . "#539bf5")
   (ansi-magenta . "#b083f0")
   (ansi-cyan . "#39c5cf")
   (ansi-white . "#909dab")
   (ansi-bright-black . "#636e7b")
   (ansi-bright-red . "#ff938a")
   (ansi-bright-green . "#6bc46d")
   (ansi-bright-yellow . "#daaa3f")
   (ansi-bright-blue . "#6cb6ff")
   (ansi-bright-magenta . "#dcbdfb")
   (ansi-bright-cyan . "#56d4dd")
   (ansi-bright-white . "#cdd9e5"))))

;;; github-definitions.el ends here
