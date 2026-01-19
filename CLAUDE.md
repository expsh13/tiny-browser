# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A minimal TUI browser for learning browser rendering. Renders HTML/CSS to the terminal using block layout only.

## Learning Resources

This project follows **"Let's build a browser engine!"** by Matt Brubeck:
https://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html

Reference material for deeper understanding:
- [How Browsers Work (web.dev)](https://web.dev/articles/howbrowserswork?hl=ja)
- [Inside look at modern web browsers (Chrome)](https://developer.chrome.com/blog/inside-browser-part1?hl=ja)

### Tutorial to Module Mapping

| Tutorial Part | Module | Status |
|---------------|--------|--------|
| Part 1: Getting Started | Project setup | ✅ |
| Part 2: HTML | `Parser/Html.hs`, `Dom.hs` | ✅ |
| Part 3: CSS | `Parser/Css.hs` | 🔲 |
| Part 4: Style | `Style.hs` | 🔲 |
| Part 5: Boxes | `Layout.hs` | 🔲 |
| Part 6: Block Layout | `Layout.hs` | 🔲 |
| Part 7: Painting | `Render.hs` | 🔲 |

## Build Commands

```bash
cabal build          # Build the project
cabal run tiny-browser -- index.html style.css  # Run with HTML and CSS files
```

## Architecture

The rendering pipeline follows standard browser architecture:

```
HTML → Parser → DOM Tree
CSS  → Parser → Stylesheet
           ↓
    Style Computation (match selectors to DOM nodes)
           ↓
    Layout Calculation (compute box positions/sizes)
           ↓
    TUI Renderer (output to terminal)
```

### Module Structure

- `src/Dom.hs` - DOM tree data types
- `src/Parser/Html.hs` - HTML parser (hand-written recursive descent)
- `src/Parser/Css.hs` - CSS parser (hand-written recursive descent)
- `src/Style.hs` - Style computation (selector matching)
- `src/Layout.hs` - Block layout algorithm
- `src/Render.hs` - Terminal rendering with ANSI colors
- `app/Main.hs` - CLI entry point

## Supported Features

### HTML Elements
`<html>`, `<body>`, `<div>`, `<p>`

### CSS Properties
- `width`, `height` (px)
- `background-color`, `color` (#hex only, e.g. `#ff0000`)

### CSS Selectors
Tag selectors only (`div`, `p`, etc.)

## Intentionally Unsupported
- JavaScript
- Inline elements (`<span>`, etc.)
- Inline layout / Flexbox / Grid
- float / position
- margin / padding / border
- CSS inheritance
- Relative units (%, em)
- Color names (only #hex supported)
- Class / ID selectors
- `<style>` tag / inline style attribute
