# Existing-posterior tariff games: mathematical contract

The fitted-game extension covers constant-cost output Logit/CES games. Consumer
prices are tariff inclusive. Product tariff tau is the fraction of consumer
price collected by government; seller retention r=1-tau is known, positive,
and fixed with respect to choices. A producer-value rate t instead implies
r=1/(1+t). Finite negative tariffs (subsidies) are allowed; tau >= 1 and
nonfinite tariffs are rejected. Ownership remains the actual ownership matrix;
retention is separate state exposed by `antitrust::getRetention()`.

## Mixed-retention equilibrium equations

Let O_ij be ownership, J_ji=dq_j/dp_i, and K_ji=dp_j/dq_i. With effective
cost kappa_j=c_j/r_j, simultaneous output games solve

\[
\text{Bertrand: } O_{ii}r_iq_i +
  \sum_j O_{ij}r_j(p_j-\kappa_j)J_{ji}=0,
\]
\[
\text{Cournot: } O_{ii}r_i(p_i-\kappa_i)+
  \sum_j O_{ij}r_jq_jK_{ji}=0.
\]

Thus an action-row matrix uses O_ij r_j/r_i. Legacy method orientations
differ, so there is no universal ownership row-scaling correction. All active
firms' choices are solved, including untariffed domestic products. In native
Stackelberg games, follower FOCs and their implicit response derivatives are
retention-weighted, as are each leader's own reduced-profit gradient. Core
firms have retained-revenue strategic incentives; the fringe keeps its
specified atomistic conduct. Monopolistic competition likewise keeps its
perceived elasticity rather than becoming a strategic oligopoly.

Domestic Bertrand prices can rise when import tariffs weaken competition.
With several remaining domestic firms, high tariffs approach a domestic-only
oligopoly, not necessarily monopoly. An atomistic model need not raise domestic
prices: domestic quantities can respond even when its markup is fixed.

Differentiated Cournot solvers require a valid interior solution for the
selected active products. They reject failed or nonpositive price solutions;
automatic zero-output portfolio selection is not implied by mixed-retention
support. Product exit can be specified explicitly. Homogeneous Cournot handles
nonnegative output and capacity corners through its KKT system.

## Effective-cost equivalence

Require r_j=r_f within every firm in the decision cell. With physical cost
c_j and effective cost kappa_j=c_j/r_f,

\[
\pi_f^{tar}(z)=\sum_{j\in f}(r_f p_j(z)-c_j)q_j(z)
=r_f\sum_{j\in f}(p_j(z)-\kappa_j)q_j(z)
=r_f\pi_f^{eff}(z).
\]

Positive constant scaling preserves each firm's entire optimization problem,
not just a candidate FOC. This applies to own prices or quantities, multiple
products, simultaneous leaders, and the audited core/fringe objectives.

For strategic followers, let F_F be the stacked own-profit gradients and R_F
be the diagonal matrix repeating each follower firm's retention for its action
coordinates. Then F_F^tar=R_F F_F^eff and

\[
-J_{FF}^{tar,-1}J_{FL}^{tar}
=-(R_FJ_{FF}^{eff})^{-1}(R_FJ_{FL}^{eff})
=-J_{FF}^{eff,-1}J_{FL}^{eff}.
\]

Thus follower response derivatives are unchanged wherever the implicit system
is nonsingular. Each leader's reduced profit and its gradient are multiplied
by its own retention. The same noncooperative leader equilibrium follows; no
joint-profit objective is introduced. Distinct firms can have different
retentions. The proof does not depend on Logit/CES functional form, but support
is limited to the audited package games and their existing existence/domain
conditions.

## Estimation and margin basis

The original no-tariff margin prediction can be reused only for net-revenue
margins and their existing statistical error model:

\[
m_j^{net}=\frac{r_fp_j-c_j}{r_fp_j}=\frac{p_j-\kappa_j}{p_j}.
\]

Each posterior structural draw therefore implies physical costs
c_pre=r_pre*kappa_pre. No new HMC parameter, prior or density transformation is
introduced into estimation by this post-processing. The same Stan/bridge
target remains applicable if the supplied data and priors already satisfy the
stated effective-cost interpretation. A distinct density over physical costs
would require the appropriate change-of-variable Jacobian if requested; this
extension does not silently introduce a physical-cost prior.

The raw production-cost Lerner margin M=(p-c)/p satisfies
M=tau+(1-tau)m_net. Profit divided by consumer revenue satisfies
profit/p=(1-tau)m_net. These are different reporting scales. Algebraic
conversion of noiseless margins does not prove equality of Gaussian/Student-t
likelihoods on their noisy versions. Nonzero-baseline reuse requires explicit
net-revenue provenance; an incompatible historical fit needs corrected
estimation rather than cost scaling after fitting.

## Policy state and mergers

Keep physical pre costs immutable. For a proportional physical cost shock d,

\[
c_j^{post}=c_j^{pre}(1+d_j),\qquad
\kappa_j^{post}=\kappa_j^{pre}(1+d_j)
\frac{1-\tau_j^{pre}}{1-\tau_j^{post}}.
\]

The equilibrium uses the resulting effective costs. Antitrust's public
`mcDelta` is a physical cost shock and `revenueRetentionPost` supplies the
retention adjustment automatically (auction cost shocks are additive physical
levels). Coordination's native API instead takes an effective-cost shock plus
the explicit retention vector. Trade performs this boundary conversion once.
Check retention uniformity under POST ownership before using this cost-only reduction.
A merger can combine differing pre retentions provided its post retention is
uniform: physical cost recovery uses each original product's own retention.
If the merged firm's post tariffs remain heterogeneous, pure cost adjustment
is invalid. The implemented retained-revenue equations above handle those
post-policy portfolios for simultaneous and native sequential games.

Promotion does not solve or recalibrate. Its baseline retention must equal the
source retention up to positive firm-wide factors (atomistic products are
separate decision units). Otherwise `trade_tariff_incompatible_baseline` is
raised. Calibrate/specify the source with `revenueRetentionPre`, or simulate
the policy from its existing baseline instead. Homogeneous Linear/LogLin
Cournot stores physical cost functions: promotion scales both MC and variable
cost functions consistently, leaving fitted choices unchanged. Its solver
enforces nonnegative output and capacity complementarity.

For example, in multiproduct output Logit Bertrand with positive alpha, define
T_f=sum_{j in f}(r_j p_j-c_j)s_j. A product's true tariff FOC is

\[
r_i-\alpha(r_ip_i-c_i)+\alpha T_f=0.
\]

The untaxed effective-cost FOC would instead be
1-alpha(p_i-c_i/r_i)+alpha*sum_{j in f}(p_j-c_j/r_j)s_j=0.
Dividing the true FOC by r_i gives a product-specific T_f/r_i term, which
generally differs from the untaxed common weighted sum. This supplies a direct
counterexample to naive heterogeneous tariff cost-only simulation.

## Bargaining and auction boundaries

Logit/BLP bargaining uses product-removal disagreement. For each integration
draw d, s^-i_i,d=0 and s^-i_j,d=s_j,d/(1-s_i,d), j != i. Buyer surplus is
C_i=sum_d w_d log(1-s_i,d)/alpha_d, and seller gain is
D_i=sum_j O_ij r_j(p_j-kappa_j) sum_d w_d(s_j,d-s^-i_j,d).
The Nash objective integrates these gains before taking logs or optimizing;
substituting aggregate shares into draw-level diversion is incorrect. With
buyer bargaining power b_i, the margin system has
A_ij=O_ij r_j[J_ji - b_i/(1-b_i) q_i Deltaq_i,j/C_i] and
A(p-kappa)=-diag(O) r q. Homogeneous price coefficients reduce to the Logit
formula. CES bargaining retains its documented experimental conduct shortcut,
`(1-bargaining power) * Bertrand margin`; it is not asserted to be this exact
product-removal Nash model.

Native second-score auctions retain their existing one-offer-per-firm rules.
Uniform retention within each active bidding portfolio allows exact effective
cost adjustment. Mixed retention changes optimal product selection before
rivals' scores are known, requiring a separate bidding-equilibrium derivation.
It is explicitly rejected for Logit and BLP auctions; the implementation does
not assume firms can switch products after observing rival scores.

## Quality, role and accounting state

Demand, outside-price/market-size normalization and each posterior quality/k
realization remain fixed across pre/post scenarios. Tariff revenue is not
rebated into demand automatically. A later policy's payoff does not require
re-estimating that baseline. Pre-existing unknown or endogenous tariffs,
physical-cost restrictions, income feedback, entry, capacity constraints or
changes to quality require an explicit separate model.

Report gross expenditure p*q, net seller revenue r*p*q, physical production
cost c*q, producer profit (r*p-c)*q and government revenue tau*p*q separately.
Their accounting identity is producer profit + government revenue =
(p-c)*q when every relevant producer/government is included. National welfare
requires stating the included producer jurisdictions.

For nonzero pre tariffs, equilibrium choices match the effective-cost source
game; physical producer profit need not match its unadjusted profit report.
For an unchanged policy, prices/shares and all consistently computed welfare
changes are zero even if baseline government revenue is nonzero. Preserve the
package's established CV monetary convention rather than invent a new CES
consumer-surplus formula or treat compensation and utility as interchangeable.
