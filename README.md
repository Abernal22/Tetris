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

## 2. Prerequisites

- [GHC 8.10 or later](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/) (recommended) **or** `cabal-install` & `cabal` CLI
- `gloss` library (graphics & input)

Install via Stack:
```bash
stack install gloss
```

Or via Cabal:
```bash
cabal update
cabal install gloss
```

---

## 3. Project Structure

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

## 4. Build & Run Instructions

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

## 5. Sample Auxiliary Files

<details>
<summary><code>Main.hs</code></summary>
```hs
module Main where
import Game (gameMain)

main :: IO ()
main = gameMain
```
</details>

<details>
<summary><code>cs357-tetris.cabal</code></summary>
```cabal
name:                cs357-tetris
version:             0.1.0.0
build-type:          Simple
cabal-version:       >=1.10

executable tetris
  main-is:           Main.hs
  hs-source-dirs:    src
  ghc-options:       -Wall -threaded
  build-depends:
    base >=4.7 && <5,
    gloss >=1.13.2
  default-language:  Haskell2010
```
</details>

<details>
<summary><code>stack.yaml</code></summary>
```yaml
resolver: lts-18.18  # or newer
packages:
- .
extra-deps: []

# for custom setup
flags: {}
extra-package-dbs: []
```
</details>

<details>
<summary><code>.gitignore</code></summary>
```gitignore
.stack-work/
dist/
*.hi
*.o
*.prof
.stack-work/
.cabal-sandbox/
.DS_Store
```
</details>

---
