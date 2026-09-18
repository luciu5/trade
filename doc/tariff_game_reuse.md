# Reusing a fitted game for firm-uniform tariffs

This is an output-market extension of existing fitted structural games. It
adds no demand/conduct equation and performs no inference or recalibration.
Supported games are ordinary Logit/CES Bertrand, Cournot and MonCom and native
coordination Logit/CES core-fringe and noncooperative Stackelberg games.

## Exact cost transformation

Consumer prices include the policy wedge. trade's product tariff parameter is
\(\tau_j<1\), a fraction of consumer price, not a producer-value ad valorem rate
\(t_j\). The conversion is \(\tau=t/(1+t)\). Retention is \(r=1-\tau>0\).
For each active firm, retention must be constant across its owned products and
independent of its actions. Then

\[
\pi_f^\tau(z)=\sum_{j\in f}(r_fp_j(z)-c_j)q_j(z)
=r_f\sum_{j\in f}(p_j(z)-\kappa_j)q_j(z),\qquad
\kappa_j=c_j/r_f.
\]

Multiplication by a positive constant preserves the firm's entire best-response
set, not just a local FOC. For followers, write the untaxed FOC system as
\(F_F(z_F,z_L;\kappa)=0\). Its tariff version is \(D_rF_F=0\), where \(D_r\)
repeats a follower firm's retention on its action coordinates. Where the
implicit function exists,

\[
-[(D_rF_F)_{z_F}]^{-1}(D_rF_F)_{z_L}
=-[F_{F,z_F}]^{-1}F_{F,z_L}.
\]

Thus the follower equilibrium and reaction derivative agree. A leader's own
reduced profit is multiplied by its own retention; the first-stage best
response remains the same. This applies to several simultaneous noncooperative
leaders. It does not replace their individual objectives with joint profit.
No additional Logit/CES approximation is required. Any existence, interiority
or equilibrium-selection limitation of the underlying solver still applies.

The observed net seller-revenue margin must satisfy

\[
m_j=\frac{r_fp_j-c_j}{r_fp_j}=\frac{p_j-\kappa_j}{p_j}.
\]

For saved fitted effective costs and a proportional physical-cost shock \(d\),

\[
c_j^{pre}=r_j^{pre}\kappa_j^{pre},\qquad
c_j^{post}=c_j^{pre}(1+d_j),\qquad
\kappa_j^{post}=\kappa_j^{pre}(1+d_j)
                  \frac{r_j^{pre}}{r_j^{post}}.
\]

These calculations use immutable original costs on every scenario, including
repeated simulations. An omitted cost shock retains the current scenario
shock; explicit zero restores original physical costs. Repeating an unchanged
scenario, including an existing cost shock, returns stored equilibrium state. A merger may combine firms with distinct pre tariffs,
but the active merged firm must have uniform post retention. Caller-provided
post leader/core identities determine role changes. Inactive products have
zero quantity and zero monetary accounts.

## Conditions for posterior reuse

Known action-independent wedges, consumer-price demand inputs, compatible
net-margin likelihood, effective-cost interpretation and constant marginal
costs are required. An existing prior/equation on *physical* costs, a different
margin reporting/error scale or linked physical costs over time can invalidate
reuse. A noisy likelihood fitted to gross-cost margins cannot be repaired by
rescaling posterior costs. At zero baseline tariff, physical/effective costs
and the usual output margin bases coincide.

The initial percentage-cost adapter requires finite, strictly positive
baseline and post costs. Zero/negative-cost states are rejected explicitly;
this is an implementation restriction, not a requirement of the payoff-scaling
proof. Retain failed posterior draw identities and failure rates rather than
silently conditioning reported policy distributions on accepted costs.

Demand parameters, outside price, size/budget and realized quality are held
fixed across a draw's pre/post scenarios. Entry, endogenous quality, capacity,
quota, income feedback, input markets, auction/bargaining and within-firm
heterogeneous retention are outside this initial extension. Unsupported cases
fail explicitly. No optimizer tolerance or underlying economic equation changes.

## Lifecycle and public methods

`as_trade_fit()` returns a `TariffGameFit` extending `TradeFit` through the common
`StructuralFit` lifecycle. Native coordination models dispatch directly.
Ordinary `AntitrustFit` objects enter this route when a basis argument is
supplied; historical ordinary promotion calls retain their old tariff adapter.
At nonzero baseline tariff, both bases must be explicitly declared:

```r
policy_fit <- trade::as_trade_fit(
  fitted_game, tariffPre = tau_pre,
  cost_basis = "effective", margin_basis = "net_revenue"
)
scenario <- trade::simulate(policy_fit, tariffPost = tau_post,
                            mcDelta = .10)
trade::tariff_accounts(scenario)
trade::tariff_welfare(scenario)
```

Promotion copies baseline state and makes zero equilibrium solves. An unchanged
policy likewise returns stored equilibrium state. Changed policy is delegated
only through public antitrust/coordination simulators using transformed costs.
New registry entries are promotion/simulation-only, not new calibration APIs.
The source fit and posterior draw provenance remain unchanged.

## Physical accounting

`calcMC()` reports physical production costs. `calcMargins()` reports net
seller-revenue margins; `level=TRUE` reports physical unit profit \(rp-c\).
Accounts separately report effective markup \(p-\kappa\), gross/net revenue,
physical producer profit and importing-government tariff revenue:

\[
PS=\sum_j(r_jp_j-c_j)q_j,\qquad T=\sum_j\tau_jp_jq_j.
\]

For all included producers, \(PS+T=\sum_j(p_j-c_j)q_j\). This is a transfer
identity, not a nationality or exact welfare assertion. Source effective-cost
profit generally differs from physical profit when baseline tariffs are
nonzero. An unchanged policy has zero *changes*, while baseline tariff revenue
can be positive. Consumer variation uses the existing antitrust convention:
positive CV is compensation for a price increase, so monetary consumer change
is \(-CV\). Consumer, producer and government changes remain separate;
no exact national/global total or automatic revenue rebate is asserted.

## Validation provenance

Independent primitive payoff/FOC/best-response checks, production simulations
and retained failed attempts are reported in antitrustBayes's companion
`doc/tariff_posterior_reuse_results.md` and compact validation CSVs. Large raw
posterior fits, local package libraries and full logs are excluded from Git.
