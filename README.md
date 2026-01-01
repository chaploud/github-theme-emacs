# GitHub Theme for Emacs

[![CI](https://github.com/chaploud/github-theme-emacs/actions/workflows/ci.yml/badge.svg)](https://github.com/chaploud/github-theme-emacs/actions/workflows/ci.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![Emacs 27.1+](https://img.shields.io/badge/Emacs-27.1%2B-blueviolet.svg)

A faithful port of [GitHub's official color schemes](https://github.com/primer/github-vscode-theme) to Emacs. Colors extracted from the official GitHub VS Code theme using the [Primer Design System](https://primer.style/foundations/color).

## Screenshots

| Light | Light High Contrast |
|:-----:|:-------------------:|
| ![Light](screenshots/light.png) | ![Light High Contrast](screenshots/light-high-contrast.png) |

| Dark | Dark Dimmed |
|:----:|:-----------:|
| ![Dark](screenshots/dark.png) | ![Dark Dimmed](screenshots/dark-dimmed.png) |

## Flavors

| Flavor | Description |
|--------|-------------|
| `light` | GitHub Light Default |
| `light-high-contrast` | GitHub Light High Contrast |
| `dark` | GitHub Dark Default |
| `dark-dimmed` | GitHub Dark Dimmed |

## Installation

### use-package with :vc (Emacs 30+)

Emacs 30 has built-in `:vc` support in use-package:

```elisp
(use-package github-theme
  :vc (:url "https://github.com/chaploud/github-theme-emacs" :rev :newest)
  :custom
  (github-theme-flavor 'dark)
  :config
  (load-theme 'github t))
```

### use-package with vc-use-package (Emacs 29)

For Emacs 29, install [vc-use-package](https://github.com/slotThe/vc-use-package) first:

```elisp
;; Bootstrap vc-use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package github-theme
  :vc (:fetcher github :repo "chaploud/github-theme-emacs")
  :custom
  (github-theme-flavor 'dark)
  :config
  (load-theme 'github t))
```

### Manual Installation

1. Clone this repository:

```sh
git clone https://github.com/chaploud/github-theme-emacs.git ~/.emacs.d/themes/github-theme
```

2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/themes/github-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/github-theme")
(setq github-theme-flavor 'dark)
(load-theme 'github t)
```

## Configuration

### Set Flavor

With use-package `:custom`:

```elisp
(use-package github-theme
  :vc (:url "https://github.com/chaploud/github-theme-emacs" :rev :newest)
  :custom
  (github-theme-flavor 'dark)           ; 'light, 'light-high-contrast, 'dark, 'dark-dimmed
  (github-theme-italic-comments t)      ; Use italic for comments
  (github-theme-italic-blockquotes t)   ; Use italic for blockquotes
  (github-theme-italic-variables nil)   ; Use italic for variables
  :config
  (load-theme 'github t))
```

Or with `setq`:

```elisp
(setq github-theme-flavor 'dark)
(setq github-theme-italic-comments t)
(load-theme 'github t)
```

### Switch Flavor Interactively

```
M-x github-theme-load-flavor
```

Or programmatically:

```elisp
(github-theme-load-flavor 'light)
```

### Custom Colors

Override specific colors:

```elisp
(github-theme-set-color 'accent-fg "#ff0000")
(github-theme-reload)
```

Get a color value:

```elisp
(github-theme-color 'accent-fg) ; => "#0969da" (for light theme)
```

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

Colors are extracted from the official [GitHub VS Code Theme](https://github.com/primer/github-vscode-theme):

```sh
git clone https://github.com/primer/github-vscode-theme.git
cd github-vscode-theme
npm install
npm run build
# Theme JSON files are generated in themes/
```

## Credits

- [GitHub VS Code Theme](https://github.com/primer/github-vscode-theme) - Official source
- [Primer Design System](https://primer.style/foundations/color) - Color system
- [Catppuccin Emacs](https://github.com/catppuccin/emacs) - Architecture reference

## License

GPL-3.0 - See [LICENSE](LICENSE) for details.
