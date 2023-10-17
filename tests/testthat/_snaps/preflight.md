# renv warns when snapshotting missing dependencies

    Code
      snapshot()
    Output
      The following required packages are not installed:
      - oatmeal  [required by breakfast]
      Consider reinstalling these packages before snapshotting the lockfile.
      
    Condition
      Error in `renv_snapshot_validate_report()`:
      ! aborting snapshot due to pre-flight validation failure

