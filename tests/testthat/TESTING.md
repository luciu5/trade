# trade test tiers

`TRADE_TEST_TIER` selects `fast`, `extended`, or `nightly`. An unset variable
selects `fast` for local runs. The trade workflow runs `fast` on pushes,
`extended` on pull requests, and `nightly` on the schedule. Manual runs can
select `extended` or `nightly`. Higher tiers include lower-tier tests.

- **fast:** direct tariff/Cournot FOCs, tariff accounts, numerical oracles,
  public lifecycle, representative promotion, sequential policy, and deterministic
  BLP integration-state checks.
- **extended:** exhaustive registered promotion routes and output-game policy
  matrices, plus BLP calibration against supplied integration points and weights.
- **nightly:** both Gauss-Hermite and Monte Carlo BLP parameter-recovery runs,
  plus the migration-only supplied-parameter parity matrix. Revisit the parity
  matrix once `refactor` is canonical.

The prior all-tests baseline was 143 `test_that()` blocks and 113.7 seconds
under local source loading; two BLP calibration blocks consumed 82.6 seconds.
Tiering preserves these statistical and integration checks while making routine
failures faster to detect. `testthat` skips tiered blocks before constructing
fixtures or solving.

The workflow records the selected tiers and resolved Git SHAs for both
repositories in its job summary and source-provenance artifact. A reproducible
dependency run can be started manually with a full antitrust commit SHA in the
`antitrust_ref` input.
