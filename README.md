# RogueTrooper

A small Haskell + Raylib arcade game (Paratrooper-meets-Vampire-Survivors: defend a fixed
tower against parachuting troopers). The game itself is a jam project; this README is about the
**engine ideas inside it**, because they transfer to anything that needs lots of independent
scripted actors — game or simulation — and they're worth stealing.

The whole thing is built on one bet: **game programming and game design should be separable.**
The engine is a generic substrate that knows about velocity, gravity, and entities. It knows
*nothing* about flamethrowers, parachutes, or wave timing. All of that is content, written in an
embedded DSL that can't reach into the engine. Adding an enemy or a weapon is writing a script,
never editing the simulation.

If you're here to build something similar, the three ideas to take are:

1. **A free-monad DSL** for describing actor behaviour as data.
2. **Resumable continuations** so one script can span many frames.
3. **An events-and-fold engine** so scripts describe intent and the engine owns all mutation.

## The three ideas

### 1. Behaviour as a free-monad DSL

`RogueTrooper.Script` defines a tiny instruction set and wraps it in a free monad
(`Script = Free ScriptF`). A behaviour is then just an ordinary monadic Haskell value — you get
`do`-notation, `forever`, `when`, local helpers, recursion — but it evaluates to *data
describing intent*, not to side effects.

```haskell
data ScriptF next
  = GetAimPos (Vector2 -> next)        -- query: read world state into the script
  | GetEnemies ([(EntityId, Vector2)] -> next)
  | DoSteer Float Vector2 next         -- command: ask the engine to ease my velocity
  | Fire Vector2 ProjectileType next   -- effect: ask the engine to spawn something
  | Yield next                         -- suspension: resume me next frame
  | …
  deriving Functor
```

Three flavours of instruction, and that split is the whole design:

- **Queries** read live world state *into* the script (`getAimPos`, `getEnemies`,
  `getEntityPos id`, `getMyPos`, `getTargetInBox`, `getGravity`, `getDt`).
- **Commands / effects** ask the engine to act (`steer`, `setVel`, `setRadius`, `push`, `fire`,
  `spawnEnemyAt`, `damage`, `despawn`).
- **Suspension** (`yield`) hands control back until the next frame.

Why a free monad and not, say, a plain `World -> World` update function or a flat coroutine?
Because queries *and* suspension together let a script **decide based on live state** *and*
**choreograph timed sequences** — and a function-to-function update can't suspend mid-decision
while a coroutine can't cleanly read fresh world state at each step. The free monad gives you
both, and keeps behaviour as inspectable data the engine interprets however it likes.

Keep the vocabulary minimal. Add a verb when a behaviour actually needs it, never
speculatively — the set above is everything six weapons, two actor types and a wave director
turned out to need.

### 2. Resumable continuations (multi-frame scripts)

Each entity stores its *current continuation* (`script :: Script ()`). Every frame the
interpreter runs that continuation until it hits `yield` (or finishes), then stores the
*resumed* continuation to run next frame. The script's local variables and call stack survive
across frames for free — that's the continuation.

This makes two very different behaviour shapes coexist with no special engine support:

```haskell
-- Reactive: re-decide every frame from live queries (the turret, enemies, companions)
forever $ do
  aim <- getAimPos
  seekAt scanSpeed aim
  yield

-- Sequential: run straight through, with timed gaps (the wave director)
dropTroopers n
wait betweenRounds      -- 'wait' is just getDt + yield in a countdown loop
runRounds rest
```

`wait` isn't an engine feature — it's `getDt`/`yield` in a recursive countdown, and the
half-finished countdown *is* the stored continuation. Timers, state machines, and multi-step
choreography all fall out of "store the continuation, resume it next frame."

### 3. Events and fold (the engine owns all mutation)

Scripts never mutate anything. Running one entity's script for a frame is a **pure function to a
list of `Event`s**. The engine then folds those events into the world. `step` is the whole loop:

```haskell
step world gs =
  let events = concatMap (runEntityFrame world) (Map.elems gs.entities)  -- gather intent
      folded = applyEvents world events gs                               -- fold it in
   in resolveTowerHits (integrate gravity ground dt folded)             -- one physics pass
```

1. **`runEntityFrame`** interprets an entity's continuation against a read-only `World`. Queries
   are answered from the world; commands/effects become `Event`s (`Steer`, `SetVel`, `Impulse`,
   `Resize`, `Spawn`, `Damage`, `Despawn`); `yield`/completion emits a `SetScript` carrying the
   resumed continuation. The entity is never touched here.
2. **`applyEvents`** folds the frame's events into `GameState` with `foldl'` — the *one* place
   mutation happens: velocities eased/set/impulsed, continuations stored, projectiles/enemies
   spawned with fresh ids, damage applied (kill + score at 0 HP), entities removed.
3. **`integrate`** is a single physics pass over everyone (gravity on airborne physical
   entities, position += velocity·dt, landed enemies clamped). Velocity persists between frames.

The payoff: **there is no per-projectile collision or lifetime code in the engine.** A bullet
despawns itself because *its own script* queries `getEnemies`, checks overlap, and calls
`despawnSelf`. The engine carries `mkProjectile`/`mkEnemy` factories *supplied by content*, so
it never imports a behaviour module — the dependency only ever points content → engine.

```
  CONTENT (describes intent)            ENGINE (carries it out)
  Behaviours.Weapon / Companion  ──┐
  Behaviours.Enemy / Mission     ──┼──► Script (the DSL) ◄── Engine (interpret + fold + physics)
  Behaviours.Common / Art        ──┘                          Aim   (pure geometry)
                                                               Types (pure data)
```

## What the separation buys you: six weapons, zero engine edits

The clearest proof the boundary holds is the weapon roster. Every weapon is a script, and they
differ along two axes — **how a script picks a target** and **how its projectile flies** —
without the engine knowing any of it. Each weapon below was a new script plus, at most, a new
`ProjectileType` variant.

**Targeting** is encoded per-weapon, not as a shared engine rule:

| Weapon | How its script acquires a target |
|---|---|
| Main turret (bullet / flamethrower) | Player-aimed: reads `getAimPos`, fires where it points, **no auto-lock** |
| Homing-missile companion | Random live enemy (`randIndex` over `getEnemies`) |
| Shotgun companion | Closest enemy (`nearestEnemy`) |
| Tesla companion | Random enemy within a long range |
| Repulsor / boomerang companions | Untargeted — fire whenever enemies exist |

**Flight model** is encoded per-projectile, and between them they exercise every motion verb the
DSL has:

| Projectile | Flight, expressed in the DSL |
|---|---|
| Straight bullet | Solves a ballistic firing solution (`launchToHit`, using `getGravity`) so it *lands on the aim point*; gravity-curved; knockback on hit |
| Flame particle | Kinematic `setVel` straight line (no droop); randomised cone spread with an RNG seed threaded through the continuation; expires by distance |
| Homing missile | `steer`s toward a queried target id every frame; on target loss keeps thrusting along its heading instead of stalling |
| Repulsor wave | Stationary effect: `setRadius` grows a hit ring; shoves each newly-reached enemy outward once (the "already hit" set lives in the continuation) |
| Boomerang | Drives a closed-form out-and-back S-curve by emitting the path's derivative as kinematic velocity each frame |
| Tesla bolt | Damages instantly, `linger`s, then spawns a successor bolt to the nearest not-yet-struck enemy in range — chaining up to 2 bounces, re-evaluated live so dead targets are skipped |

A flat data-table of weapon stats couldn't express any of this; a script can, because it's a
real program with state and control flow. That's the argument for behaviour-as-DSL in one table.

The same boundary covers actors: `enemyBehaviour` is a `forever` loop with a position-driven
two-state machine (parachute-descent while airborne via `steer`; walk to the tower once
`onLand`), and `missionDirector` is the sequential counterpart — drop N troopers at
deterministic-random positions, `wait` between drops and rounds, escalate, idle.

## Reusing the platform's primitives

`RogueTrooper.Aim` composes `h-raylib`'s own vector math and collision checks into game logic
(`boxContains` for circle/rect/oval regions, `nearestInBox` with a defined tie-break) rather
than re-implementing geometry. The one exception is point-in-ellipse (raylib has no native
test), done with the normalised ellipse inequality. Lesson worth copying: don't unit-test or
re-write the library's primitives; only test the *composition* you wrote.

## Testing: test the pure core, eyeball the rest

The whole architecture is built to make the interesting logic **pure and therefore testable**,
and to keep the untestable parts (rendering, feel) small and isolated. So the test policy is the
test pyramid applied honestly — *not* a blanket coverage target.

`hspec` (`test/Spec.hs`) covers, deterministically:

- **Geometry / targeting** — `boxContains`, `nearestInBox` tie-breaking, the ballistic
  `launchToHit` solution, tesla `chainTarget` selection.
- **Engine** — `integrate` (gravity, landing, non-physical entities), `applyEvents` (every
  event variant including damage-and-score), `resolveTowerHits`.
- **Behaviours, end-to-end through `step`** — the turret's player-aimed no-auto-lock firing, the
  enemy parachute→land→advance machine, every projectile and every companion's target
  acquisition and hold-fire, and sequential mission scripting.

Because `step` is pure, a behaviour test is just "build a `GameState`, run `step` N times,
assert on the result" — no window, no mocking, no IO. What's deliberately *not* tested:
`h-raylib` primitives, rendering, window lifecycle, and game-feel tuning (seek speed, fire rate,
difficulty) — those are verified by running the game.

## Build & run

`ghcup`-installed GHC/Cabal plus the **raylib** system library. Cabal-only (no Stack/hpack),
`GHC2024`, `rerebase` as the Prelude replacement.

```sh
cabal build
cabal run
cabal test
```

## Module map

| Module | Role |
|---|---|
| `RogueTrooper.Script` | The free-monad DSL: instruction set + smart constructors |
| `RogueTrooper.Engine` | Interpret (`runEntityFrame`) → fold (`applyEvents`) → physics (`integrate`, `step`) |
| `RogueTrooper.Types` | Pure data: `Entity`, `Event`, `GameState`, `Box`/`BoxShape` |
| `RogueTrooper.Aim` | Pure geometry composed from raylib primitives |
| `RogueTrooper.Behaviours.Weapon` | Turret + all projectile scripts |
| `RogueTrooper.Behaviours.Companion` | Autonomous companion scripts |
| `RogueTrooper.Behaviours.Enemy` | Paratrooper state machine |
| `RogueTrooper.Behaviours.Mission` | Sequential wave director |
| `RogueTrooper.Behaviours.Common` | Shared helpers: `wait`, `seekAt`, `launchToHit`, RNG |
| `RogueTrooper.Art` | Procedural pixel-art sprites |

See [`SPEC.md`](SPEC.md) for the full design contract.
