# GitHub Theme for Emacs

[![CI](https://github.com/your-username/github-theme-emacs/actions/workflows/ci.yml/badge.svg)](https://github.com/your-username/github-theme-emacs/actions/workflows/ci.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![Emacs 27.1+](https://img.shields.io/badge/Emacs-27.1%2B-blueviolet.svg)
<!-- [![MELPA](https://melpa.org/packages/github-theme-badge.svg)](https://melpa.org/#/github-theme) -->
<!-- [![MELPA Stable](https://stable.melpa.org/packages/github-theme-badge.svg)](https://stable.melpa.org/#/github-theme) -->

A faithful port of [GitHub's official color schemes](https://github.com/primer/github-vscode-theme) to Emacs. Colors extracted from the official GitHub VS Code theme using the [Primer Design System](https://primer.style/design/foundations/color/).

## Flavors

| Flavor | Description |
|--------|-------------|
| `light` | GitHub Light Default |
| `light-high-contrast` | GitHub Light High Contrast |
| `dark` | GitHub Dark Default |
| `dark-dimmed` | GitHub Dark Dimmed |

## Installation

### MELPA (Coming Soon)

```elisp
(use-package github-theme
  :ensure t
  :config
  (setq github-flavor 'dark)
  (load-theme 'github t))
```

### Manual Installation

1. Clone this repository:
   ```sh
   git clone https://github.com/your-username/github-theme-emacs.git ~/.emacs.d/themes/github-theme
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/themes/github-theme")
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/github-theme")
   (setq github-flavor 'dark) ; or 'light, 'dark-dimmed, 'light-high-contrast
   (load-theme 'github t)
   ```

### Doom Emacs

Add to `packages.el`:
```elisp
(package! github-theme
  :recipe (:host github :repo "your-username/github-theme-emacs"))
```

Add to `config.el`:
```elisp
(setq github-flavor 'dark)
(setq doom-theme 'github)
```

### Spacemacs

Add `github-theme` to `dotspacemacs-additional-packages`:
```elisp
dotspacemacs-additional-packages '(
  (github-theme :location (recipe :fetcher github :repo "your-username/github-theme-emacs"))
)
```

Set the theme:
```elisp
dotspacemacs-themes '(github)
```

## Configuration

### Set Flavor

```elisp
;; Set before loading the theme
(setq github-flavor 'dark) ; or 'light, 'dark-dimmed, 'light-high-contrast
(load-theme 'github t)
```

### Switch Flavor Interactively

```elisp
M-x github-load-flavor
```

Or programmatically:
```elisp
(github-load-flavor 'light)
```

### Customization Options

```elisp
;; Use italic for comments
(setq github-italic-comments t)

;; Use italic for blockquotes in markdown/org
(setq github-italic-blockquotes t)

;; Use italic for variables
(setq github-italic-variables nil)

;; Reload theme to apply changes
(github-reload)
```

### Custom Colors

Override specific colors:
```elisp
(github-set-color 'accent-fg "#ff0000")
(github-reload)
```

Get a color value:
```elisp
(github-color 'accent-fg) ; => "#0969da" (for light theme)
```

## Supported Packages

The theme provides faces for:

- **Core**: Font-lock, mode-line, fringe, line numbers, whitespace
- **Completion**: Company, Corfu, Ivy, Vertico, Orderless
- **Git**: Magit, Diff-hl, Git-gutter
- **Org**: Org-mode, Markdown-mode
- **File Navigation**: Dired, Treemacs
- **Syntax**: Tree-sitter
- **LSP**: LSP-mode
- **Checking**: Flycheck, Flymake
- **Misc**: Avy, Rainbow-delimiters, Which-key

## Color Palette

### Light Default
| Role | Color |
|------|-------|
| Foreground | `#1f2328` |
| Background | `#ffffff` |
| Accent | `#0969da` |
| Success | `#1a7f37` |
| Danger | `#cf222e` |
| Attention | `#9a6700` |

### Dark Default
| Role | Color |
|------|-------|
| Foreground | `#e6edf3` |
| Background | `#0d1117` |
| Accent | `#2f81f7` |
| Success | `#3fb950` |
| Danger | `#f85149` |
| Attention | `#d29922` |

## Development

### Requirements

- Emacs 27.1 or later

### Running Tests

```sh
emacs --batch -l github-theme-test.el
```

### Building Color Definitions

Colors are extracted from the official GitHub VS Code theme:

```sh
cd github-vscode-theme
npm install
npm run build
# Theme JSON files are generated in themes/
```

## Credits

- [GitHub VS Code Theme](https://github.com/primer/github-vscode-theme) - Official source
- [Primer Design System](https://primer.style/design/foundations/color/) - Color system
- [Catppuccin Emacs](https://github.com/catppuccin/emacs) - Architecture reference

## License

GPL-3.0 - See [LICENSE](LICENSE) for details.
