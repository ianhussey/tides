# Plan: near-analytic certification for `brimmest()`

Status: **Phase A shipped in 0.4.9** (`R/certify-sandwich.R`,
`tests/testthat/test-certify-sandwich.R`); Phase B still open. Companion to the
manuscript sections "Certifying one report without enumerating every other",
"Replacing the sweep with arithmetic and a witness" and "An open problem: the
near-closed-form certificate" in `manuscript.qmd`.

## What shipped, and how it differs from the plan below

Steps 1, 2 and 4a collapsed into one object rather than three. A search over
**non-increasing** value sequences is already a search over the partitions of
`S`, so Step 2's partition enumeration is not a separate routine with its own
`e <= T` threshold — it is what the Step 4a search *does* when the wall is
near, and the `y -> W - y` reflection is applied once at entry rather than as
a special case for the top wall. That removed the need for `.q_set_near_wall()`
and for a measured threshold on `e`.

The search is also stronger than the greedy the plan specified. A greedy walk
upward from the clustered configuration is success-only: it certifies
possibility when it reaches the target and concludes nothing when it stalls.
Running the same construction as a depth-first search with the Step 1 sandwich
re-applied at every node, and with children ordered by how centrally the
surviving window sits inside each child's own sandwich, keeps the greedy dive
as its first path while making **exhaustion a proof of impossibility**. The
verdict is therefore exact in both directions whenever the search terminates,
and the node budget — not the absence of a theorem — is the only thing that
sends a report to the corridor DP.

Measured on 2,719,364 reporting-grid cells across six designs, two precisions
and two rounding rules: zero disagreements with the lattice route, and the
budget was never reached. 0-63 at n = 50 fell from 82 s to 0.006 s.

**Not done from Phase A.** `.target_states()` was not rewritten on exact
integers; the fast path consumes its output and converts to integers in
`.k_window()`, so the `1e-9` epsilons still live in candidate-sum generation.
The witness is computed and tested but not surfaced in `brimmest()`'s return
value, because the lattice route cannot produce one and the column would be
silently absent for grid-sized workloads.

## Context

`brimmest()` currently has two routes, both exact: the full attainable lattice
(cheap for small designs, cached, refused above `max_cells`) and the targeted
corridor DP (`R/attainable-target.R`; ~9% of the lattice's cells, ~2x wall
clock, and the only route on wide scales — 0–63 at n = 50 certifies in ~105 s
where the lattice refuses outright). Both sweep a state space whose size is set
by the design, not by the report. This plan replaces most of that sweep with
arithmetic, enumeration whose cost is set by the report's position in the
umbrella, and a constructive witness search — leaving the corridor DP as a
fallback for one well-defined sliver.

A scope decision fixed before this plan: **granularity tests stay bounds-free.**
GRIM and GRIMMER are deferred to `scrutiny` verbatim and will not gain scale
limits; everything scale-aware — including all of the below — lives in
`strait`'s certification layer. (An instrumented analysis of GRIMMER's
component tests lives in
`tides_article/grimmer_component_test_redundancy/`; its exact integer oracle
is the arithmetic ported in Step 1, but no change to GRIMMER itself is in
scope.)

Terminology used throughout, on the shifted scale `y = x − l ∈ {0..W}`,
`W = u − l`:

- `S` — the sample sum; the mean's rounding interval admits only integers
  `S ∈ [n·(m_lo − l), n·(m_hi − l)]`, typically 1–5 candidates (the GRIM
  condition read as a candidate generator).
- `Q` — the sum of squares; each candidate `S` pins it to a narrow window via
  `Q = S²/n + (n−1)·sd²`.
- `q_min(S)` — smallest achievable `Q`: the clustered configuration (all
  values on the two integers around the mean). **Identical to the
  `sd_min_integer()` floor the package already exports** (verified
  numerically this session); the plan reuses that identity rather than
  introducing a second name for the same bound.
- `q_max(S)` — largest achievable `Q`: the Structure-S configuration
  (`nu` values at `W`, remainder `rem`, rest at 0): `nu·W² + rem²`.
  The integer form of `sd_max_structure_s()`.
- parity — `Q ≡ S (mod 2)` always, since `y² ≡ y (mod 2)`.
- wall distance — `e = min(S, nW − S)`; how far the sum sits from the nearer
  extreme configuration.

---

## Step 1 — exact integer sandwich screen — **SHIPPED (0.4.9)**

**What.** Per candidate `S`, an O(1) test: the report is impossible unless the
`Q` window intersects `[q_min(S), q_max(S)]` in an integer of the right
parity. One direction only — it proves impossibility, never possibility.

**Where it comes from.**
- Candidate-sum generation and interval endpoints in pure integer arithmetic:
  port `floor_div` / `ceil_div` / `grim_sum_range` from
  `tides_article/grimmer_component_test_redundancy/grimmer_exact_oracle.R`,
  which also verified the endpoint-inclusivity semantics per rounding rule
  against `scrutiny`'s own `unround()`.
- `q_min` is the oracle's `q_min_exact()` (its "test 4"); `q_max` is new to
  the oracle but is just Structure-S in integers.

**Implementation.** Extend `R/attainable-target.R` (or a sibling
`R/certify-sandwich.R`): rewrite `.target_states()` on exact integers,
eliminating the current `1e-9` epsilons — a robustness win independent of
speed. The two traps already regression-tested (`sd = 0` unrounding to a
negative-endpoint interval; `seq.int()` counting down on an empty candidate
range) must be preserved in the integer rewrite.

**Effect.** Everything below the floor, above the ceiling, outside the scale
range, or parity-impossible is decided in microseconds. This includes a large
share of the umbrella's exterior that the corridor DP currently sweeps for.

---

## Step 2 — wall-distance partition enumeration — **SHIPPED, subsumed by Step 4a**

**What.** When the wall distance `e` is small, the complete achievable `Q` set
at that `S` is exactly the sums of squares of the partitions of `e` into parts
`≤ W` (at most `n` parts — automatic when `e ≤ n`, with the `y → W − y` mirror
handling the top wall). Enumerate it outright; the verdict is exact in both
directions, and the cost depends only on `e` and `W`, **not on n**.

**Where it comes from.** This generalises `top_ss_ladder()` in
`validation/validate_sd_bounds_functions.qmd` (chunk `gap-ladder`, section
"How close to the ceiling can a false pass hide?"), which best-first-searches
inward unit transfers from the Structure-S configuration and is "complete for
the top rungs". The partition form is the same object computed non-lazily,
valid at both walls. Part 3 of the same document established that every
observed blind-spot cell sits in the top fringe — i.e. precisely in this
regime — so this step alone decides the cells the closed forms leak.

Worked example (this session): the blind-spot cell (mean 1.3, sd 0.9, n = 9,
1–5) has one candidate `S`, `e = 3`, achievable `Q ∈ {3, 5, 9}` from the three
partitions of 3, and the window wants 7 — impossible, three configurations
examined, versus a corridor sweep.

**Implementation.** Internal `.q_set_near_wall(e, W)`, memoised per `(e, W)`;
threshold on `e` chosen by measurement (partition counts: p(40) ≈ 37k is
trivial, p(60) ≈ 966k is the ceiling of comfort). Move/adapt the ladder code
from the validation doc into the package rather than duplicating it.

---

## Step 3 — the resolution cutoff for large n — **OPEN (Phase B)**

**What.** The top gap between adjacent achievable SDs is
`g₁ = 2(R−1)/(R·√(n(n−1)))` (validation doc, same section — derived, not
fitted). When `g₁ < 10^(−d)` — i.e. `n ≳ 2·10^d` — no reported window can fall
cleanly inside a gap, so any window overlapping `[floor, ceiling]` contains an
achievable value: certification collapses to Step 1's sandwich.

**The missing lemma.** This is rigorous only if the *largest* gap anywhere in
the ladder is the top gap (gap sizes monotone down the ladder). Empirically
true in every design examined; not proven. The n = 2 counterexample-flavoured
case (W = 6, achievable `Q ∈ {18, 20, 26, 36}` — two wide gaps) shows small-n
ladders are deep, so any use of this step must be gated on n being past the
cutoff, never near it.

**Why it matters.** This regime — large n, wide scales — is exactly where the
corridor DP is slowest. The DP's cost grows with n while this step's validity
improves with n; they are complements, not competitors.

**Implementation.** Behind the same assert-safety pattern the validation doc
uses for its `C = 3` band ("ladder band retains every false pass", chunk
`closure-cert`): adopt only alongside a per-design calibration assertion, or
after the lemma is proven in the manuscript.

---

## Step 4 — witness construction — **4a SHIPPED (as exhaustive DFS); 4b OPEN**

**4a (sound today).** A greedy builder: start from the clustered configuration
(`Q = q_min`), walk `Q` upward in +2 steps by single-unit spreads
(`m, m → m−1, m+1`) toward the target `k`. If it reaches `k`, the
configuration in hand **is a proof of possibility** — witness included. If it
fails, nothing is concluded and the router falls through. Success-only
semantics make this shippable now with no theorem: greedy success is
self-certifying.

Bonus: `brimmest()` can return the witness (a frequency vector) for most
possible verdicts, removing CLOSURE's remaining niche for
one-witness-is-enough uses. (CLOSURE remains the tool for *all* witnesses and
their distribution — `unsum::closure_generate()`.)

**4b (the open problem).** Prove the greedy reaches every parity-admissible
`k ∈ [q_min, q_max − fringe(W)]` — the contiguity theorem already framed in
`manuscript.qmd` ("An open problem: the near-closed-form certificate").
Proving it makes Step 5's fallback empty, retires `max_cells`, and belongs in
the manuscript, not the package. Supporting evidence to cite: Part 2 of the
validation doc shows floor and ceiling are *attained* at every mean
(sharpness), and Part 3's exhaustive sweeps found no interior gap anywhere
below the top fringe.

---

## Step 5 — the routing composite — **SHIPPED, simplified**

Order matters; the wall-distance check must precede any band logic, because
near the walls the whole ladder is a few rungs and "top C·g₁ band" degenerates
(the e = 3 example: *everything* is fringe).

Planned:

```
per candidate S (usually 1-5 of them):
  1. sandwich + parity (Step 1)          -> impossible, or continue
  2. wall distance e <= T (Step 2)       -> exact verdict, done
  3. greedy witness reaches window (4a)  -> possible (+ witness), done
  4. [Phase B] n past cutoff (Step 3)    -> possible, done
  5. window within ladder's enumerated
     top rungs (existing top_ss_ladder)  -> exact verdict, done
  6. corridor DP (existing)              -> exact verdict (fallback sliver)
grid-sized workloads keep the cached-lattice route unchanged.
```

As shipped, because 2, 3 and 5 all turned out to be the same search:

```
per candidate S (usually 1-5 of them):
  1. .k_window(): sandwich + parity      -> impossible, or continue
  2. .witness_search(): reflect to the
     nearer wall, DFS over non-increasing
     samples with the sandwich re-applied
     at every node
       leaf reached  -> possible (+ witness), done
       tree exhausted -> impossible, done
       budget reached -> no verdict
  3. .attainable_target(): corridor DP   -> exact verdict (fallback sliver)
grid-sized workloads keep the cached-lattice route unchanged.
```

The fallback sliver after Phase A is: mid-scale sums, greedy failure, window
straddling the un-enumerated part of the ladder — expected to be rare, and
every verdict outside it is proven rather than assumed.

---

## Validation and tests

- **Equivalence harness.** Extend the existing cell-for-cell agreement run
  (1.75M cells, six designs x two precisions x two rounding rules, currently
  asserting corridor == lattice) to assert router == corridor == lattice.
  Keep the reduced version in `tests/testthat/test-brimmest.R`; heavy sweeps
  stay in the validation doc.
- **Named regressions to carry over:** `sd = 0` sign trap; empty candidate
  range; endpoint exclusivity per rounding rule; `W = 1` (binary) degeneracy;
  the n = 2, W = 6 deep-ladder case as a guard against band logic at tiny n.
- **Benchmarks to report** (before/after, in the validation doc and NEWS):
  the blind-spot cell (currently corridor, target: partition route, ~µs);
  0–63 n = 50 single report (105 s -> target: greedy witness, ~ms);
  0–100 n = 100 (currently untimed; measure).
- **Docs.** New section in `validation/validate_sd_bounds_functions.qmd`
  asserting the router's equivalence; update `manuscript.qmd` "Making it
  fast" with measured numbers and promote the open-problem section's status
  if 4b lands.

## Phasing

- **Phase A** (all rigorous, no conjectures): Steps 1, 2, 4a, routing, tests,
  benchmarks. Version bump + NEWS entry. **Done in 0.4.9**, less the two items
  listed under "Not done from Phase A" above.
- **Phase B** (conditional on the lemma / theorem): Step 3, Step 4b, retire
  `max_cells`, manuscript update. **Open.** Note that Phase A has changed what
  Phase B would buy: Step 3's cutoff was motivated by the corridor DP being
  slowest at large n, and the corridor DP now runs for essentially no report.
  Step 4b's contiguity theorem remains worth proving, but for the manuscript's
  sake rather than the package's — proving it would replace a search that
  already terminates in microseconds with arithmetic that provably always
  does.
