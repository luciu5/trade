# trade

Calibrate tariff/quota models, specify structural parameters, or apply policy
changes to an existing fitted game through the antitrust structural-fit lifecycle.
`calibrate()` estimates from observed inputs; `specify()` uses supplied parameters;
`as_trade_fit()` promotes a compatible fit without recalibration; `simulate()`
solves a policy scenario. `supportedModels()` lists implemented model operations.

## Install

Install the current compatible public stack, including trade's dependencies:

```r
source("https://raw.githubusercontent.com/luciu5/competitiontoolbox/master/tools/install_iovrse.R")
install_iovrse(packages = "trade")
```

The [installer guide](https://github.com/luciu5/competitiontoolbox/tree/master/tools)
explains supported options. Ordinary deterministic tariff simulations do not
require CmdStan, Bayesian fitting, a private repository, or release certification.

## A tariff on a fitted multi-product game

```r
library(trade)

# Demand and costs are already fitted/specified; promotion does not refit them.
source_fit <- antitrust::specify(
  "logit", "bertrand", variant = "standard",
  prices = c(2, 2.2, 2.4, 2.6), ownerPre = c("A", "A", "B", "C"),
  insideSize = 100, labels = c("a", "b", "c", "d"),
  parameters = list(alpha = -1.5, meanval = c(3, 2.5, 2, 1.5))
)
policy_fit <- as_trade_fit(source_fit, tariffPre = 0,
  cost_basis = "effective", margin_basis = "net_revenue")
# tau is a fraction of consumer price: statutory25% corresponds to tau=.20.
scenario <- simulate(policy_fit, tariffPost = c(.20, .20, 0, 0))
accounts <- tariff_accounts(scenario)
accounts[, c("product", "price", "q", "producer_profit", "government_revenue")]
stopifnot(max(abs(accounts$producer_profit + accounts$government_revenue -
  (accounts$price - accounts$physical_cost) * accounts$q)) < 1e-8)
tariff_welfare(scenario)
```

Tariffs use the fraction of **consumer price**, with `tau=t/(1+t)` for a
producer-value statutory rate `t`. Physical costs and net seller revenue are
reported separately. At a nonzero baseline tariff, declare the effective-cost and
net-revenue margin bases explicitly; attaching a wedge to an incompatible
likelihood or prior does not repair it. See [fitted-game reuse](doc/tariff_game_reuse.md)
and `?as_trade_fit` for supported models, baselines and posterior reuse conditions.

Ordinary supported games can also use sequential ownership, cost, entry and exit
paths through `counterfactual()` and `add_step()`. Native Stackelberg/core-fringe
paths require explicit role IDs when ownership changes and do not support entry.
The registry identifies promotion-only routes; they are not new calibration APIs.

For the interactive horizontal/vertical/tariff/quota interface, install
`install_iovrse(packages = "competitiontoolbox")` and run
`competitiontoolbox::ct_shiny()`. The deprecated `trade_shiny()` forwards to that compatible interface and gives
an installation command when it is missing. Advanced R routes may extend beyond the GUI's supported
model selection. Start with these task guides (source links remain available until the rendered site is published):

| Question | Guide |
| --- | --- |
| What changes under a tariff, and who receives the money? | [Tariff scenario](vignettes/Reference.Rmd) |
| Does an import quota bind? | [Binding quota](vignettes/quotas.Rmd) |
| What if firms choose quantities? | [Quantity competition](vignettes/cournot-tariffs.Rmd) |

The longer [mathematical reference](doc/tariff_game_math.md) retains the model background.
