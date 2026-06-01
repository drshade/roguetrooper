# Spec: RogueTrooper

---
spec_type: system
status: approved
created: 2026-06-01
updated: 2026-06-01
author: Tom Wells (tom@synthesis.co.za)
---

## Purpose

RogueTrooper is a desktop arcade roguelite built for a 2-month after-hours work game
jam, with a playable demo due at the 1-month mark. It fuses Paratrooper's fixed-turret
defensive tension with Vampire Survivors' build-crafting loop: the player defends a fixed
central tower, steering a slow-seeking auto-firing turret toward the mouse, while the real
depth lives in per-round upgrade choices and persistent cross-run progression. Runs are
finite with a guaranteed ending, so each playthrough is a complete, satisfying arc.

A secondary purpose — and the technical thing the author is most excited about — is to
cleanly **separate game programming from game design** by making all content (enemies,
the turret, waves, and upgrades) authorable as an embedded DSL decoupled from the engine.

## Scope

**In scope**
- Single fixed central tower with a mouse-directed, slow-seeking auto-firing turret.
- Aiming mechanic: a mouse-tracked **aiming box** (crosshair) and a lagging **turret box**
  that seeks toward it; on fire, auto-target the nearest enemy inside the turret box.
- Descending (parachuting) enemy waves that land and advance on the tower, Paratrooper-style.
- Round-based structure with escalating difficulty and a fixed maximum round count (guaranteed ending).
- Per-round upgrade picks (choose 1 of 3) affecting the current run.
- Persistent meta-upgrades between runs, saved to local disk, funded by gems dropped on kills.
- 8-bit pixel-art aesthetic (Vampire Survivors-inspired density/juice).
- Win/lose states and a basic HUD (tower HP, round counter, score = kill count).
- Embedded DSL for enemy behaviours, turret behaviour, wave definitions, and upgrade definitions.

**Out of scope**
- Multiplayer, online leaderboards, networking.
- Player movement, multiple turrets, varied level geometry.
- Audio polish, story/cutscenes, settings menus beyond the minimum.
- Mobile/web ports; cross-platform packaging (should run on Linux/Windows via raylib but untested for the jam).
- External/parsed scripting (files, parser, serialization) — the DSL is embedded Haskell only.
- `effectful` in the per-frame loop.

### Demo milestone (1 month)

Minimum proud-to-show demo: core loop running at 60 FPS — turret + aiming mechanic feeling
good, one enemy type parachuting/landing/advancing, a few escalating rounds, and the
per-round choose-1-of-3 upgrade screen working — with the core loop and first enemy/turret
already authored behind the DSL boundary. Meta-progression (gems, persistence) and the
fuller enemy roster / wave content may land in month 2.

## Functional Requirements

### Turret & Aiming
- **Must**: An aiming box (a shape with a crosshair centre, initially a circle) tracks the mouse position exactly.
- **Must**: A turret box seeks toward the aiming box at a finite seek speed, visibly lagging on both axes (turret turns left/right and elevates up/down to synchronise).
- **Must**: The turret auto-fires on a fixed interval (fire rate); the player never clicks to fire.
- **Must**: On fire, the turret targets the **nearest enemy inside the turret box**, damages/destroys it, and visually marks the locked target (lock-on indicator).
- **Must**: When no enemy is inside the turret box, the turret holds fire (does not fire and does not consume cadence on empty).
- **Must Not**: Fire at or hit enemies outside the turret box.
- **Should**: Seek speed, box size, and box shape (circle → oval → square, …) are upgradeable.

### Enemies
- **Must**: Enemies spawn at the screen edges/top and parachute toward the ground.
- **Must**: On landing, an enemy switches to advancing toward the tower.
- **Must**: An enemy reaching the tower deals 1 HP of tower damage and is removed.
- **Must**: Enemies can be destroyed by turret fire in either air or ground state.
- **Must**: A killed enemy drops a gem that falls to the ground.
- **Should**: At least 2 enemy archetypes exist by jam end (e.g. basic Grunt + a faster/tougher variant).

### Rounds & Difficulty
- **Must**: Play is structured in discrete rounds; a round ends when its spawn quota is cleared.
- **Must**: Difficulty escalates per round (spawn count / rate / speed / mix).
- **Must**: A run has a fixed maximum round count with a defined ending (win state on clearing the final round).

### Upgrades & Progression
- **Must**: Between rounds, the player chooses 1 of 3 offered upgrades, applied for the remainder of the run.
- **Must**: Upgrades modify stats (turret seek speed, fire rate, damage, box size/shape, etc.).
- **Must**: Killed enemies drop gems; gems feed a persistent meta-currency.
- **Must**: Persistent meta-upgrades carry across runs and are saved to local disk.

### Game State & HUD
- **Must**: Display tower HP, current round, and score (kill count).
- **Must**: The run ends in a loss when tower HP reaches 0, and in a win when the final round is cleared.

### Authoring DSL (embedded)
- **Must**: Enemy behaviours are authored as an embedded DSL decoupled from the engine (engine simulates; behaviour describes intent), using a per-tick coroutine that emits a fixed instruction vocabulary (robowars-style).
- **Must**: The turret's seek / target / fire behaviour is authored as a behaviour script using the same pattern as enemies.
- **Must**: Waves/rounds are authored as a declarative embedded DSL (a timeline of spawn directives with escalation), interpreted by the engine.
- **Must**: Upgrades are authored as a data-shaped DSL (stat deltas / effect hooks).
- **Should**: The DSL boundary (engine interprets; content describes intent) is built **upfront** — the first enemy and turret are authored behind it from the start, not as throwaway engine code to be extracted later. The robowars coroutine pattern (`Script = ScriptInput -> (Script, [Instruction])`) is the proven structural skeleton.
- **Should**: The instruction vocabulary grows **minimally and on demand** — add a verb when an enemy/turret/wave actually needs it; do not speculatively design unused instructions. Expect the turret/aiming verbs (what engine→script feedback the box-targeting needs) to be the part most likely to move.
- **Must Not**: Couple content authoring to engine internals (rendering, collision, raylib IO) such that adding a new enemy/wave/upgrade requires engine changes.

## Non-Functional Requirements

### Tech & Build
- **Must**: Haskell + Raylib via the `h-raylib` binding, desktop target (macOS dev machine).
- **Must**: Cabal-based (no Stack, no hpack), `cabal-version: 2.4`, `default-language: GHC2024`.
- **Must**: `rerebase` as the Prelude replacement using the custom `src/Prelude.hs` + `mixins` pattern; curated default-extensions set.
- **Must**: `cabal build` and `cabal run` succeed from a clean checkout with documented prerequisites (ghcup-installed GHC/Cabal; raylib system dependency).

### Performance
- **Must**: Sustain 60 FPS with ~200 simultaneous enemies plus their bullets and gems on the dev Mac.
- **Should**: Keep the per-frame update/render loop in plain `IO` with strict/unboxed structures; avoid `effectful` and minimise per-frame allocations to limit GC pauses.

### Persistence & Reliability
- **Must**: Meta-progression (gems / unlocks) is saved to a local file and survives restarts; a missing or corrupt save degrades gracefully to defaults.
- **Should**: Saves are written temp-then-rename so a crash mid-run cannot corrupt the persistent save.
- **Should**: The simulation is deterministic given a fixed RNG seed (aids debugging and wave testing).

### Input & Accessibility
- **Should**: The core loop is fully playable mouse-only.
- Key rebinding and broader accessibility passes are out of scope for the jam.

## Acceptance Criteria

- Given the game is running, when the mouse moves, then the aiming box tracks it exactly and the turret box accelerates toward it at the current seek speed, visibly lagging.
- Given an enemy is inside the turret box and the fire timer elapses, when the turret fires, then it targets the nearest in-box enemy, shows a lock-on indicator, and damages it.
- Given no enemy is inside the turret box, when the fire timer elapses, then the turret does not fire.
- Given an enemy finishes parachuting, when it lands, then it switches to advancing toward the tower.
- Given an advancing enemy reaches the tower, then tower HP decreases by 1 and the enemy is removed.
- Given an enemy is killed, then score increases by 1 and a gem drops and falls to the ground.
- Given a round's spawn quota is cleared, when the round ends, then the player is offered 3 upgrades and play resumes after selection.
- Given a selected upgrade modifies a stat, when the next round begins, then the change is in effect for the remainder of the run.
- Given tower HP reaches 0, then the run ends in a loss; given the final round is cleared, then the run ends in a win.
- Given a completed run, when the game restarts, then persisted meta-progression is loaded from disk; given a corrupt/missing save, then defaults are loaded without crashing.
- Given the same RNG seed and identical inputs, when a run is replayed, then wave composition is identical.
- Given a new enemy archetype defined purely in the DSL, when the game runs, then it spawns and behaves correctly with no changes to engine code.

## Open Questions

- [ ] **Gem-collection mechanic** — how are grounded gems collected? Candidate: a deployable collection jeep that gathers gems but must be protected from advancing enemies. — Owner: Tom, Target: before month-2 meta work
- [ ] **Meta-upgrade economy** — confirm meta-upgrades are gem-currency-priced (leaning this way) vs milestone-unlocked. — Owner: Tom, Target: before month-2 meta work
- [ ] **Run length** — what is the fixed maximum round count, and is there a final boss/finale or just a cleared-final-round win? — Owner: Tom, Target: during core-loop tuning
- [ ] **Enemy roster** — confirm the 2+ archetypes and their distinguishing behaviours for the jam. — Owner: Tom, Target: after first enemy proves the DSL verbs
- [ ] **DSL surface detail** — the full instruction vocabulary and wave-DSL shape, to be documented in `docs/design-edsl.md` as it stabilises (built upfront, grown minimally). — Owner: Tom, Target: as the loop stabilises
