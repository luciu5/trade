# `trade` refactor migration report

## Current status

This source snapshot contains the registry, normalized model specification, `TradeFit`,
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
The extended test suite and local package check pass on this staged `trade`
tree with antitrust commit `20f0923fc560295e04310aed5615c19dadf672cb` and
coordination version `0.0.0.9006`. The package check reports `Status: OK`.
The `--as-cran` incoming-feasibility phase could not run because the sandbox
could not resolve CRAN; the ordinary local check completed all package,
documentation, example, and test checks. The tests include the retained
physical-profit Cournot conditions and the tariff-based import classification
and weighted reporting regression.

The CI development bootstrap still resolves coordination from `main` and
records that commit SHA; this default is not release certification for the
new helper API. After the coordinated changes are committed, pass the full
updated coordination SHA through `coordination_ref`. CI checks the required
exports before building trade and reports an actionable error if the selected
source does not contain them. The antitrust baseline and the other toolbox
dependencies remain pinned to the tested immutable commits.

`update()` is tested for no-op and changed-margin recalibration. `respecify()`
is tested for Logit Bertrand↔monopolistic-competition parameter retention,
target-state reconstruction, flat Logit↔CES local translations, unsupported
Cournot/nested transitions, and source-fit immutability. Flat Logit↔CES
translations require explicit target curvature, match target baseline shares
analytically, and report deterministic local elasticity diagnostics; they are
not treated as a global demand equivalence. A respecified fit retains the
source calibration call only as provenance and has no current calibration
call, so `update(respecified_fit)` fails instead of recalibrating from source
margins.

The target `TariffCES` supplied-parameter path inherits the ordinary
output-market `CESALM` convention, whose admissible curvature region is
`gamma > 1`. Trade `respecify(..., demand = "ces")` now validates this domain
explicitly. The legacy `sim()` path is unchanged; it historically accepts
nonnegative `gamma` values while using the supplied value directly, so this
stricter check is limited to deterministic output-market respecification.

## Preserved or unsupported behavior

`Counterfactual` and `combine_counterfactuals()` now provide a reusable policy
boundary. Tariff, quota, and exit fields are validated against the registered
trade model before translation to legacy policy slots. Unsupported ownership,
cost, capacity, bargaining, and leadership fields fail explicitly. Legacy
policy arguments remain compatible, simultaneous fields are applied in one
model-specific solve, and result metadata identifies the supplied scenario.

Tariff incidence, marginal-cost recovery, quota capacity treatment, bargaining
conventions, and all solver behavior remain delegated to the existing model
implementations. The CES bargaining-tariff path is documented and tested as
new substantive functionality: its revenue-share conversion, tariff-adjusted
ownership, marginal-cost wedge, baseline-price, and zero-tariff behavior have
an independent regression/oracle check. Nested-demand and Cournot respecification remain unsupported
because trade currently lacks a validated common supplied-parameter path for
those conversions. They require `update()` or a separately reviewed
model-specific loader.
