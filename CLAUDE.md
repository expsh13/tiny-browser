# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A minimal TUI browser for learning browser rendering. Renders HTML/CSS to the terminal using block layout only.

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

- `src/DOM.hs` - DOM tree data types
- `src/Parser/HTML.hs` - HTML parser (megaparsec)
- `src/Parser/CSS.hs` - CSS parser (megaparsec)
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
