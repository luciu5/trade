# Promoting an `antitrust` fit to `trade`

`as_trade_fit()` turns an already fitted or specified
`antitrust::AntitrustFit` into a `trade::TradeFit` when the two packages have
the same normalized demand, conduct, and variant in their registries. The
promotion layer changes the policy environment represented by the object. It
does not recalibrate demand, infer structural parameters, or solve a baseline
or post-policy equilibrium.

The public interface is an S4 generic:

```r
as_trade_fit(object, policy = "tariff", ...)
```

The primary method accepts an `AntitrustFit`. A `StructuralFit` for which
there is no registered promotion route raises an informative error. The
generic lives in `trade`; `antitrust` has no dependency on `trade`.

This interface requires `antitrust >= 0.99.40`. Its ALM class validation
distinguishes fitted demand state from calibration inputs, so the trade
wrappers can retain a known outside share without replacing the source
normalization with a calibration marker.

## Eligibility is registry based

Promotion starts with `source <- object@spec`, then looks up the target with
`trade::model_spec(source$demand, source$conduct, source$variant, policy)`. A
model is eligible only when the normalized source triple is present in both
registries and the requested trade policy has a complete implementation. The
overlap can be inspected directly:

```r
antitrust_models <- antitrust::supportedModels()
trade_models <- trade::supportedModels()

overlap <- merge(
  antitrust_models[c("demand", "conduct", "variant", "class")],
  trade_models[c("demand", "conduct", "variant", "policy", "class")],
  by = c("demand", "conduct", "variant"),
  suffixes = c("_antitrust", "_trade")
)
subset(overlap, policy == "tariff")
subset(overlap, policy == "quota")
```

On the current `refactor` branches, the exact normalized overlap is the
following. The `antitrust` class is the source model class and the `trade`
class is the policy-aware wrapper constructed by promotion.

| demand | conduct | variant | policy | antitrust class | trade class |
|---|---|---|---|---|---|
| Logit | Bertrand | standard | tariff | `Logit` | `TariffLogit` |
| CES | Bertrand | standard | tariff | `CES` | `TariffCES` |
| AIDS | Bertrand | standard | tariff | `AIDS` | `TariffAIDS` |
| BLP | Bertrand | standard | tariff | `LogitBLP` | `TariffLogitBLP` |
| BLP | Cournot | standard | tariff | `CournotBLP` | `TariffCournotBLP` |
| BLP | second-score auction | standard | tariff | `Auction2ndBLP` | `TariffAuction2ndBLP` |
| BLP | bargaining | standard | tariff | `BargainingBLP` | `TariffBargainingBLP` |
| Logit | monopolistic competition | standard | tariff | `MonComLogit` | `TariffMonComLogit` |
| CES | monopolistic competition | standard | tariff | `MonComCES` | `TariffMonComCES` |
| Logit | Cournot | standard | tariff | `LogitCournot` | `TariffLogitCournot` |
| Logit | Cournot | ALM | tariff | `LogitCournotALM` | `TariffLogitCournotALM` |
| Linear | Cournot | standard | tariff | `Cournot` | `TariffCournot` |
| LogLin | Cournot | standard | tariff | `Cournot` | `TariffCournot` |
| Logit | second-score auction | standard | tariff | `Auction2ndLogit` | `Tariff2ndLogit` |
| Logit | bargaining | standard | tariff | `BargainingLogit` | `TariffBargainingLogit` |
| CES | bargaining | standard | tariff | `BargainingCES` | `TariffBargainingCES` |
| Logit | Bertrand | standard | quota | `Logit` | `QuotaLogit` |

The table is descriptive of the live registries, rather than a second
promotion table maintained by hand. For example, antitrust's nested demand,
capacity, PCAIDS, CES-Cournot, and Stackelberg entries are not promoted where
the requested trade policy has no exact registered counterpart. The AIDS
tariff row is eligible because `trade` implements that model even though its
ordinary `specify()` route is unavailable.

## What promotion carries over

The source `AntitrustFit` is the source of truth. The target model is built by
retaining the source model state and adding the policy slots required by the
registered trade class. Consequently, the following baseline state is copied
from the source wherever the corresponding slot exists:

| State | Treatment during promotion |
|---|---|
| `object@spec` | Normalized `demand`, `conduct`, and `variant` are retained; the target `policy` is the caller's requested policy. |
| Structural parameters | `object@parameters` and model-native `slopes`, `intercepts`, price coefficients, `alpha`, `gamma`, mean utilities, and normalization state are copied. They are never estimated from margins again. |
| Demand integration | BLP random-coefficient draws, nodes, weights, `nDraws`, characteristic and demographic parameters, and fitted mean utilities are retained where present. BLP contraction is not rerun. |
| Observations and baseline equilibrium | Product prices, `pricePre`, quantities or shares, `quantityPre`, and the fitted baseline output are copied. The source baseline is used directly; no baseline equilibrium solve is performed. |
| Ownership and labels | `ownerPre` and product labels are copied, including ownership vectors or matrices in the source representation. A supplied tariff does not replace the fitted source ownership. |
| Scale and normalization | `insideSize`, `mktSize` where present, outside-good price, weights, and normalization indices are carried over with the source demand convention. CES revenue scale and AIDS/quantity scale are not silently interchanged. |
| Conduct primitives | Bargaining power (`bargpowerPre`), capacities, product maps, plant-level quantities, and cost-function state (`mcPre`, `mcfunPre`, `vcfunPre`, and related fields) are carried over where the target class uses them. |
| Fit metadata | Observed inputs, structural parameters, and diagnostics are retained with promotion metadata identifying the source fit and target policy route. |

The same contract applies to each registered family as follows. “Structural
slots” means the state copied from the source model; “target defaults” means
state added only to satisfy the trade wrapper. No row recalculates a
structural parameter.

| Registered overlap | Structural slots copied | Target-only state added or derived |
|---|---|---|
| Logit / Bertrand → `TariffLogit` | `alpha`, `meanval`, `weights`, `shareInside`, `insideSize`, `priceOutside`, prices, shares, ownership, and `mcPre` | Product-level `tariffPre` and `tariffPost`; policy-adjusted cost state is produced only for a later changed tariff. |
| CES / Bertrand → `TariffCES` | `gamma`, CES mean utility/normalization, revenue shares, `insideSize`, `mktSize`, `priceOutside`, prices, ownership, and costs | Product-level tariff slots; CES revenue scale is retained and no new CES parameter is inferred. |
| AIDS / Bertrand → `TariffAIDS` | AIDS slope matrix and market elasticity, `insideSize`, price state, quantities/shares, ownership, and costs | Product-level tariff slots; `priceDelta` is initialized to zero as baseline bookkeeping and computed during policy simulation; demand parameters remain fixed. |
| BLP / Bertrand, Cournot, auction, or bargaining → the four `Tariff*BLP` classes | Fitted mean utilities, price heterogeneity, characteristics/demographic parameters, integration draws/nodes/weights and `nDraws` where present, ownership, costs, and conduct state; bargaining also copies `bargpowerPre` | Product-level tariff slots. The selected BLP conduct wrapper may recompute policy-adjusted costs or ownership only during a changed post-policy simulation; BLP contraction is not rerun. |
| Logit or CES / monopolistic competition → `TariffMonComLogit` or `TariffMonComCES` | Corresponding Logit/CES demand parameters, mean utility/normalization, shares, scale, outside-good price, ownership, and costs | Product-level tariff slots and policy bookkeeping; MonCom demand and fitted baseline are retained. |
| Logit / Cournot, standard or ALM → `TariffLogitCournot` or `TariffLogitCournotALM` | Logit parameters, mean utility, product shares, market size, ownership, fitted constant costs, and ALM normalization/calibration controls where present | Product-level tariff slots; the ALM variant is retained exactly and no alternative calibration is selected. |
| Linear or LogLin / Cournot → `TariffCournot` | Plant-by-product quantities, linear/loglinear slopes and intercepts, product maps, ownership, capacities, cost functions, and baseline prices/costs | Plant-by-product `tariffPre` and `tariffPost` matrices, with dimensions taken from the fitted quantities; only policy-adjusted Cournot state is derived for a changed tariff. |
| Logit / second-score auction → `Tariff2ndLogit` | Logit auction demand state, fitted mean utility, weights, ownership, prices, quantities/shares, and cost state | Product-level tariff slots; auction-specific fitted primitives are copied and no auction demand calibration is repeated. |
| Logit or CES / bargaining → `TariffBargainingLogit` or `TariffBargainingCES` | Demand parameters and normalization, prices/shares, ownership, costs, and `bargpowerPre` | Product-level tariff slots; pre-policy bargaining power is copied, while a changed post-policy bargaining power remains a simulation argument. |
| Logit / Bertrand → `QuotaLogit` | Logit demand state, prices, shares, ownership, `insideSize`, outside-good price, fitted costs, and mean utility | Product-level `quotaPre` and `quotaPost` (unconstrained `Inf` by convention); capacities are mechanically derived from source baseline output and the supplied quota state. |

This family table describes state transfer, not a promise that every target
constructor has a public `specify()` route. Promotion is the route for
already fitted state, including registered trade models whose ordinary
parameterized constructor is unavailable.

Fields that are present only because the target is a trade object are supplied
as policy state. For tariffs, `tariffPre` is a product vector for ordinary
models and a plant-by-product matrix for the registered Cournot tariff class
that requires that shape. The default is zero policy state with the shape
inferred from the target model (a vector or, where required, a matrix). The
baseline state is initialized with `tariffPost <- tariffPre`; this assignment
records the state and does not solve anything. For a promoted fit with a nonzero
`tariffPre`, the source fitted costs and ownership remain in place. During a
later simulation, trade applies the post-versus-pre policy change using its
relative tariff wedge, preserving the source baseline when post and pre are
the same.

For quotas, `quotaPre` is a product vector for the registered `QuotaLogit`
route and follows trade's unconstrained quota convention (`Inf` means no
quota). Promotion sets `quotaPost <- quotaPre` as baseline bookkeeping. The
quota capacity slots are mechanically formed from the source baseline output
and the supplied quota state. A finite pre-quota that would exclude the
observed source output is incompatible with exact baseline preservation and
is rejected. Negative quotas are always rejected; a quota below one is
feasible for a product with zero fitted output. An unconstrained quota gives
an infinite capacity even when fitted output is zero. No quota equilibrium
is solved during promotion.

The model wrapper may inherit a neighboring antitrust class for historical
implementation reasons. This does not mean that promotion creates a new
demand model: the inherited demand and conduct slots remain the source
slots. The trade policy slots are the only new economic state at the
promotion boundary. If an inherited ALM wrapper requires `parmsStart` and
the source has no such slot, promotion supplies the fitted price coefficient
and zero as unused optimizer controls. These controls do not replace fitted
parameters or outside-share normalization. Policy-adjusted costs, ownership wedges, quota
capacities, and equilibrium prices or quantities are recomputed only when
`simulate()` is called for a changed post-policy state.

## Baseline identity and simulation

With a zero tariff, the promoted fit must reproduce the source baseline to
numerical tolerance:

```r
source_fit <- antitrust::specify(
  demand = "logit", conduct = "bertrand",
  prices = c(2, 2.2, 2.5),
  parameters = list(alpha = -1.5, meanval = c(1, 0.8, 1.2)),
  ownerPre = diag(3),
  insideSize = 100,
  priceOutside = 0,
  labels = c("A", "B", "C")
)

trade_fit <- trade::as_trade_fit(
  source_fit,
  policy = "tariff",
  tariffPre = c(0, 0, 0)
)

stopifnot(
  isTRUE(all.equal(trade_fit@model@pricePre,
                   source_fit@model@pricePre, tolerance = 1e-8)),
  isTRUE(all.equal(trade_fit@model@shares,
                   source_fit@model@shares, tolerance = 1e-8))
)

# Promotion has recorded the baseline only. Simulation solves the requested
# changed policy state using the retained structural model.
tariff_result <- trade::simulate(
  trade_fit,
  tariffPost = c(0.10, 0, 0)
)
```

The same pattern applies when `source_fit` comes from
`antitrust::calibrate()`: pass the completed fit to `as_trade_fit()` and then
use `simulate()` for a changed `tariffPost` or `quotaPost`. For a baseline
identity check, compare the promoted model's stored `pricePre`, shares,
quantities, costs, and other available baseline outputs directly with the
source model. Promotion itself does not call an equilibrium solver. If a
caller explicitly invokes `simulate()` with `tariffPost` equal to
`tariffPre` (or `quotaPost` equal to `quotaPre`), that is still an ordinary
simulation request and may run the target solver.

Promotion is therefore distinct from `trade::calibrate()`. Calibration takes
observed prices, shares, quantities, and margins and estimates or recovers a
trade model. Promotion takes the already fitted structural state and adds
policy-state bookkeeping, so calling `calibrate()` on the source observations
would defeat the purpose of this interface.
