# Existing-posterior tariff games: mathematical contract

The initial extension is for constant-cost output Logit/CES games. Consumer
prices are tariff inclusive. Product tariff tau is the fraction of consumer
price collected by government; seller retention r=1-tau is known, positive,
and fixed with respect to choices. A producer-value rate t instead implies
r=1/(1+t). Input procurement, auctions, bargaining and quotas are not covered.

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

The solver receives the corresponding proportional effective-cost shock.
Check retention uniformity under POST ownership before using this reduction.
A merger can combine differing pre retentions provided its post retention is
uniform: physical cost recovery uses each original product's own retention.
If the merged firm's post tariffs remain heterogeneous, pure cost adjustment
is invalid. A generic tariff payoff solver can still consume the same valid
baseline posterior, but is outside this initial implementation.

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
