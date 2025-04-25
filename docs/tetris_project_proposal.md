# CS 361 Functional Tetris — Project Proposal

**Team:** Joshua Rivera & Alex Bernal

## 1. Objective
Deliver a polished, fully‑playable clone of classic *Tetris* written entirely in
Haskell. The game will demonstrate functional design patterns, clean graphics,
and responsive controls while remaining concise and well‑documented.

## 2. Current Progress
We have completed the core gameplay loop:

* **`Tetromino.hs`** – shape definitions, rotations, random generator  
* **`Board.hs`**     – 10×20 grid, collision checks, line‑clear routine  
* **`Game.hs`**      – main loop, gravity, scoring, keyboard control, rendering with Gloss  

The prototype compiles and runs: blocks fall, rotate, merge, clear, and score.

## 3. Remaining Scope
1. **Quality‑of‑Life:** next‑piece preview, holding queue, soft/hard drop flash, ghost at bottom  
2. **UX Polish:** title screen, pause/resume, animated line‑clear glow  
3. **Audio:** SFX & background loop via `gloss‑sound`  
4. **Persistence:** JSON high‑score file, configurable key‑map  
5. **Testing & CI:** QuickCheck properties, GitHub Actions build  

## 4. Work Breakdown

| **Week 1** | Refactor `Game` into pure State monad; implement hold & preview | Design title & pause screens; choose palette & fonts |
| **Week 2** | Line‑clear animation, scoring tiers, JSON persistence | Integrate sound, key‑mapping, adaptive layout |
| **Week 3** | Final polish and presentation preparation |
## 5. Libraries & Tools


## 6. Risks & Mitigation
* **Scope creep:** fixed 4‑week roadmap, weekly demos.  
* **Integration bugs:** shared branch merge every Friday, automated tests.  
* **Performance:** maintain pure state; profile with `criterion` if needed.  

With the foundation already running, the above tasks are realistic within the
allocated timeframe and will showcase advanced functional programming practice.
