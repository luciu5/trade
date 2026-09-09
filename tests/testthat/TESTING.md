# trade test and CI provenance

Trade's suite is small and has no significant slow duplication, so every tier
runs the complete package test path. The prior full-suite 419-expectation
evidence remains the reference for this package; tier routing is applied to the
larger antitrust dependency suite rather than removing trade coverage.

`fast` is the default on pushes and pull requests. Scheduled runs select
`nightly`, while manual dispatch accepts `extended` or `nightly`. Each job
records the selected tier and the resolved `git rev-parse HEAD` values for both
repositories in the job summary and a source-provenance artifact.

Push and pull-request jobs intentionally follow the mutable `refactor` branch
by default for compatibility. A reproducible dependency run must be started
manually with a full antitrust commit SHA in the workflow's `antitrust_ref`
input; the resolved SHA is authoritative if the branch moves afterward.
