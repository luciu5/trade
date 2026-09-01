# Trade refactor implementation plan

## Baseline and branch

The refactor is being developed on the local `refactor` branch, created from
`master` at `e621956` after confirming that `master` and `origin/master` were
identical. The checkout already contained uncommitted work in `DESCRIPTION`,
`R/TariffClasses.R`, `R/bargaining_tariff.R`, and `man/bargaining_tariff.Rd`;
those edits are preserved and are not treated as architectural changes in
this plan. Existing generated build artifacts are also left untouched.

The current test baseline is the repository's `tests/testthat` directory,
loaded with `pkgload::load_all()`: all existing tariff-regression tests pass
on this working tree.

## Actual package inventory

Trade extends `antitrust` S4 demand and conduct classes with policy-aware
classes. The principal calibrated-and-simulated legacy paths are:

| Policy/game path | Legacy entry point | Existing result class/family |
|---|---|---|
| tariff, differentiated Bertrand | `bertrand_tariff()` | `TariffLogit`, `TariffCES`, `TariffAIDS` |
| tariff, monopolistic competition | `monopolistic_competition_tariff()` | `TariffMonComLogit`, `TariffMonComCES` |
| tariff, differentiated Cournot | `logit_cournot_tariff()` | `TariffLogitCournot`, `TariffLogitCournotALM` |
| tariff, homogeneous Cournot | `cournot_tariff()` | `TariffCournot` |
| tariff, second-score auction | `auction2nd_tariff()` | `Tariff2ndLogit` |
| tariff, bargaining | `bargaining_tariff()` | `TariffBargainingLogit`, plus the local CES extension |
| quota, differentiated Bertrand | `bertrand_quota()` | `QuotaLogit` |
| supplied demand parameters | `sim()` | tariff result classes selected by demand/conduct |

Tariffs are not a generic supply module. In the existing implementation an
ad valorem tariff can simultaneously scale rows of the conduct/ownership
matrix and change the marginal-cost delta. Homogeneous Cournot additionally
uses plant-by-product tariff matrices inside its model-specific demand and
quantity equations. Quotas are represented as capacity changes in the
quota-specific Logit-cap or Cournot machinery. These distinctions will be
preserved.

## Refactor sequence

1. Add this repository-specific plan and capture parity values for
   representative tariff and quota paths without changing production code.
2. Add one normalized model specification and one internal registry describing
   only complete implemented trade models and their capabilities. Expose
   `supportedModels()` for inspection.
3. Add a lightweight `TradeFit` S4 wrapper around an existing calibrated trade
   S4 object. The wrapper will hold the model spec, legacy object, observed
   inputs, supplied/recovered parameters when available, and diagnostics.
4. Migrate the simplest differentiated Bertrand tariff paths first: Logit and
   CES. Implement `calibrate()` by delegating to existing constructors with no
   post-policy change, and `simulate()` by cloning the calibrated object and
   invoking existing class-specific methods after applying the requested
   tariff scenario.
5. Add `specify()` for the supplied-parameter Logit/CES Bertrand paths and
   adapt `sim()` only after parity is established.
6. Migrate additional complete families one at a time: moncom, differentiated
   Cournot, homogeneous Cournot, second-score, bargaining, and quota. A
   specialized registry entry will be used whenever a policy changes the
   equilibrium game rather than merely its state.
7. Update documentation and migration reporting only after focused parity,
   independent invariants, full tests, and package checks pass for each
   family.

## Guardrails

The legacy constructors and S4 methods remain the economic implementation.
No demand, conduct, calibration, cost-recovery, welfare, solver, tolerance,
normalization, or fallback behavior will be changed for architectural
reasons. `calibrate()` will reject post-policy arguments where practical;
`simulate()` will accept them and apply all simultaneous policy changes to one
post-policy equilibrium. Unsupported demand/conduct/policy combinations will
fail clearly instead of being manufactured by generic composition.

## Dependency and future-policy audit

Trade reuses public/re-exported `antitrust` classes, constructors, methods,
and diagnostics. The first implementation will continue using those stable
interfaces and will not depend on undocumented antitrust internals unless a
specific compatibility boundary is documented. The final architecture
report will distinguish shared infrastructure from package-specific economic
code and assess which assumptions would obstruct a future neutral `iopolicy`
layer.
