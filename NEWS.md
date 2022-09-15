# orderlyweb 0.1.14

* Update report run wait handling to use latest queue status format (orderly.server v0.3.25)

# orderlyweb 0.1.13

* Expose report kill endpoint

# orderlyweb 0.1.10

* Update to work with orderly.server v0.2.6
   * Report run status now returns queue info in a separate section
   * Report run output now returns 1 list
   * Update arg to report run has been removed

# orderlyweb 0.1.9

* Add support for bundles (VIMC-4453)

# orderlyweb 0.1.8

* Can return orderly run metadata (`orderly_run.rds`) from OrderlyWeb (VIMC-3793)

# orderlyweb 0.1.3

* The ability to run reports has been restored (VIMC-3138)

# orderlyweb 0.1.2

* `orderlyweb_remote` now implements `url_report`, as expected by `orderly` >= 0.8.2 (VIMC-2421)
