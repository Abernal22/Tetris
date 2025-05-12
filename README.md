# Tetris Submission for CS 357 (UNM)

This repository contains a Haskell implementation of a Tetris game using the `gloss` library. Follow these instructions to build, run, and submit your project.

---

## 1. Overview

- **Course**: CS 357 (Functional Programming), University of New Mexico
- **Author**: Alex Bernal, Joshua Rivera
- **Description**: A classic Tetris clone featuring:
  - Seven tetromino shapes
  - Hold & next‐piece preview windows
  - Scoring, leveling, and speed adjustment
  - Line clearing and game‐over detection
  - Configurable layout and border (submission‐ready)

---

## 2. Project Structure

```plaintext
project-root/
├── app/
|   ├── Main.hs        -- entry point (calls gameMain)
├── src/
│   ├── Game.hs        -- game loop, rendering, input, updates
│   ├── Board.hs       -- board state, line clearing, collision
│   └── Tetromino.hs   -- shapes, rotations, colors, random generator
├── tetriS.cabal       -- Cabal package file
├── cabal.project 
├── README.md          -- this file
└── .gitignore         -- ignores build artifacts
```

---

## 3. Build & Run Instructions

### With Stack
```bash
cd project-root
stack setup            # download appropriate GHC if needed
stack build            # compile the executable
stack exec tetris      # run the game (named `tetris` in cabal)
```

### With Cabal
```bash
cd project-root
cabal build            # compile
cabal run tetris       # run the executable named `tetris`
```

**Controls**:
- **A / D / S**: move tetromino left/right/down
- **W / Q**: rotate right / left
- **Space**: hard drop
- **Esc**: Pause

---

## 5. Future improvements and known bugs

- **Align Text**: all text in panel could be centered better. Text in game over screen isn't perfectly centered.
- **General UI improvments**: The white on black is not the best for overlaying text for menus etc. We could make the UI better looking in general.
- **Square Rotation Bug**: Square tetromino blocks wobble when rotated and don't react appropriately. Not game breaking but definitely not proper. 

---
