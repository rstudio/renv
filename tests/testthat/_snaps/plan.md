# plan() prints a dependency tree

    Code
      plan(project = project)
    Output
      The following dependency tree will be encoded in the lockfile:
      
      .
      ├─ breakfast 1.0.0
      │  ├─ oatmeal 1.0.0
      │  └─ toast 1.0.0
      │     └─ bread 1.0.0
      └─ renv 1.0.0
      - Lockfile written to "<wd>/renv.lock".

