# `trade` refactor architecture

The `refactor` branch separates model identity, baseline calibration, fitted
state, and policy simulation while retaining the existing trade S4 classes and
model-specific economics.

## Registry and fitted state

`model_spec()` normalizes historical demand, conduct, variant, and policy
names. `supportedModels()` is generated from the single registry in
`R/TradeModelRegistry.R`; entries point to complete legacy constructors and
record calibration, supplied-parameter, tariff, and quota capabilities.

`TradeFit` is a lightweight S4 wrapper around the existing calibrated trade
object. It stores the normalized specification, legacy S4 state, structural
parameters, observed baseline inputs, and diagnostics. `simulate()` clones the
legacy object and returns its established result class.

## Calibration and specification

`calibrate()` stores a canonical baseline call and delegates to the selected
legacy constructor. The constructor remains responsible for identification,
tariff treatment, cost recovery, equilibrium equations, solvers, and
normalizations. Post-policy arguments are rejected at this boundary.

`specify()` loads parameters through the historical `sim()` path where that
path is complete. It is not exposed for models where optimizer starts are the
only available inputs.

## Updating and respecifying

`update(fit, ...)` replaces baseline data or model-specification arguments and
calls `calibrate()` again. It is a genuine recalibration and never a class
coercion. Specified-only fits reject `update()` because they do not contain an
observed-data identifying call.

`respecify(fit, ...)` uses an explicit transition registry. Same-demand
Bertrand↔monopolistic-competition transitions retain portable primitives and
rebuild the target state through `specify()`. Flat Logit↔CES translations are
supported for Bertrand and monopolistic competition, but require target
curvature (`gamma` or `alpha`) explicitly. They preserve baseline prices,
quantities, and trade accounting, match target baseline shares analytically,
and rebuild the target state without using source margins to recalibrate
demand. This is a local demand translation, not a global equivalence claim
between price-level Logit and log-price CES. Trade has no validated nested
demand systems or supplied-parameter Cournot path, so those transitions remain
unsupported and should use `update()` or a separately reviewed model-specific
loader.

## Policy boundary

`counterfactual()` is a lightweight reusable container for post-calibration
policy changes. Trade currently maps `tariff`, `quota`, and product `exit`
through the registry capabilities; ownership, cost, capacity, bargaining,
leadership, and other fields fail early unless a complete trade model entry
supports them. `simulate(fit, cf)` applies all supplied fields to one copied
legacy model and invokes the existing tariff/quota equilibrium machinery.
Legacy `tariffPost`, `quotaPost`, and `subset` arguments remain supported.
Counterfactual metadata is attached to results and fits are not mutated.

Tariffs and quotas remain model-specific policy state. Tariff scenarios are
translated into the legacy tariff slots and marginal-cost conventions;
homogeneous Cournot retains its plant-by-product tariff matrix. Quotas use the
existing capacity-constrained paths. Ownership, cost, exit/subset, and other
post-state controls are passed only where the selected legacy class supports
them. A fit can be simulated repeatedly without mutating its baseline.

## Relationship to `antitrust`

Both packages use the same conceptual vocabulary and lightweight fitted-state
pattern, but trade retains policy-aware S4 classes and tariff/quota mechanics.
Common future infrastructure could include normalized model identities,
registries, fit provenance, and counterfactual validation. Demand, conduct,
tariff incidence, quota rationing, and welfare formulas remain package-specific
until a neutral `iopolicy` design can represent them without imposing a
generic economic decomposition.
