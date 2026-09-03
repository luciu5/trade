# `trade` refactor migration report

## Current status

The branch contains the registry, normalized model specification, `TradeFit`,
`calibrate()`, `specify()`, `simulate()`, compatibility `sim()` routing, and
explicit `update()`/`respecify()` operations. Existing tariff and quota S4
classes remain the result API.

## Registered paths

| Demand | Conduct | Policy | Calibration | Specify | Simulation |
|---|---|---|---:|---:|---:|
| Logit | Bertrand | tariff | yes | yes | yes |
| CES | Bertrand | tariff | yes | yes | yes |
| AIDS | Bertrand | tariff | yes | no | yes |
| Logit/CES | monopolistic competition | tariff | yes | yes | yes |
| Logit | Cournot | tariff | yes | no | yes |
| Linear/LogLin | homogeneous Cournot | tariff | yes | no | yes |
| Logit | second-score auction | tariff | yes | yes | yes |
| Logit/CES | bargaining | tariff | yes | Logit only | yes |
| Logit | Bertrand | quota | yes | no | yes |

## Tests and compatibility

Behavioral oracle tests cover representative legacy tariff, quota, and supplied
parameter calls. Architecture tests compare calibrated/specified simulation
results with the original constructors and verify repeated simulations do not
mutate the fit. The compatibility `sim()` wrapper routes only registered
supplied-parameter paths and preserves the legacy implementation elsewhere.
The full test suite and `R CMD check --no-manual --no-vignettes` pass for the
current phase, with only the repository's pre-existing vignette-output
warnings.

`update()` is tested for no-op and changed-margin recalibration. `respecify()`
is tested for Logit Bertrand↔monopolistic-competition parameter retention,
target-state reconstruction, flat Logit↔CES local translations, unsupported
Cournot/nested transitions, and source-fit immutability. Flat Logit↔CES
translations match target baseline shares analytically and report local
elasticity-distance diagnostics; they are not treated as a global demand
equivalence.

## Preserved or unsupported behavior

`Counterfactual` and `combine_counterfactuals()` now provide a reusable policy
boundary. Tariff, quota, and exit fields are validated against the registered
trade model before translation to legacy policy slots. Unsupported ownership,
cost, capacity, bargaining, and leadership fields fail explicitly. Legacy
policy arguments remain compatible, simultaneous fields are applied in one
model-specific solve, and result metadata identifies the supplied scenario.

Tariff incidence, marginal-cost recovery, quota capacity treatment, bargaining
conventions, and all solver behavior remain delegated to the existing model
implementations. Nested-demand and Cournot respecification remain unsupported
because trade currently lacks a validated common supplied-parameter path for
those conversions. They require `update()` or a separately reviewed
model-specific loader.
