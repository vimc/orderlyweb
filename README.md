## orderlyweb

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/vimc/orderlyweb/workflows/R-CMD-check/badge.svg)](https://github.com/vimc/orderlyweb/actions)
[![codecov.io](https://codecov.io/github/vimc/orderlyweb/coverage.svg?branch=master)](https://codecov.io/github/vimc/orderlyweb?branch=master)
<!-- badges: end -->

> R client support for OrderlyWeb

The R package [`orderly`](https://github.com/vimc/orderly) has a [web interface called OrderlyWeb](https://github.com/vimc/orderly-web); this package provides support for `orderly` to interact with `OrderlyWeb` in order to download reports from a remote server and run them remotely

### Testing

For end-to-end testing, we need a copy of orderlyweb running.  This is most easily set up using the provided configuration ([`inst/config`](inst/config)) and running `orderly-web start --pull inst/config` before running the tests.  The test suite will make changes within the configuration and will rely on the existence of particular reports.

To install `orderly-web` (the command line tool) use `pip3 install orderly-web`

A token is also needed to login with github; that can be found in the `vimc` vault as `secret/vimc-robot/vault-token` and should be available as the environment variable `ORDERLYWEB_TEST_TOKEN` (this is available on travis as an encrypted environment variable).

To stop the server, use `orderly-web stop inst/config --volumes --kill`

## Installation

```r
# install.packages("drat")
drat:::add("vimc")
install.packages("orderly")
```

## License

MIT © Imperial College of Science, Technology and Medicine
