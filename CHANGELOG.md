# Candidate change log

## 0.8.9.9001 candidate

- Admit only standard output Logit `auction2nd` to the explicit
  `TariffGameFit` effective-cost/net-revenue contract.
- Translate proportional effective tariff changes to additive `kappa` shocks
  only for `Auction2ndLogit`; retain the existing proportional convention for
  Bertrand, Cournot, and MonCom.
- Correct the legacy `Tariff2ndLogit` recalc ordering so the converted level
  cost shock is stored before `mcPost` and post-price calculation.
- Retain physical-cost, quantity, margin, and public auction-CV accounting;
  no auction FOC is copied into mergerBayes.
