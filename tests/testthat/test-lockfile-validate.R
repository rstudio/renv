test_that("a known-good full lockfile passes validation", {
  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  lockfile <- '
{
  "R": {
    "Version": "4.4.3",
    "Repositories": [
      { "Name": "CRAN", "URL": "https://cloud.r-project.org" }
    ]
  },
  "Python": {
    "Name": ".venv",
    "Type": "virtualenv",
    "Version": "3.10.12"
  },
  "Bioconductor": {
    "Version": "3.18"
  },
  "Packages": {
    "fullpkg": {
      "Package": "fullpkg",
      "Version": "1.2.3",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "0123456789abcdef0123456789abcdef",
      "biocViews": "Genomics",
      "RemoteType": "github",
      "RemoteHost": "api.github.com",
      "RemoteUsername": "rstudio",
      "RemoteRepo": "fullpkg",
      "RemoteRepos": "https://cloud.r-project.org",
      "RemoteRef": "main",
      "RemotePkgRef": "fullpkg",
      "RemotePkgPlatform": "x86_64-pc-linux-gnu",
      "RemoteSha": "abcdefabcdefabcdefabcdefabcdefab",
      "Title": "A Full Featured Package",
      "Authors@R": "person(\\"Jane\\", \\"Doe\\", role = c(\\"aut\\", \\"cre\\"), email = \\"jane@doe.com\\")",
      "Description": "This is a comprehensive DESCRIPTION-like entry with every field populated.",
      "License": "MIT + file LICENSE",
      "URL": "https://github.com/rstudio/fullpkg",
      "BugReports": "https://github.com/rstudio/fullpkg/issues",
      "Author": "Jane Doe [aut, cre]",
      "Maintainer": "Jane Doe <jane@doe.com>",
      "Depends": ["R (>= 4.0)"],
      "Imports": ["cli", "rlang"],
      "LinkingTo": ["cpp11"],
      "Suggests": ["testthat", "knitr"],
      "Enhances": ["shiny"],
      "SystemRequirements": "libxml2 (>= 2.9.0)",
      "VignetteBuilder": "knitr",
      "Type": "Package",
      "Date": "2025-04-01",
      "ByteCompile": "true",
      "Biarch": "TRUE",
      "Encoding": "UTF-8",
      "Language": "en-US",
      "RoxygenNote": "7.4.0",
      "NeedsCompilation": "yes",
      "Copyright": "© 2025 Jane Doe",
      "Config/Needs/website": "pkgdown",
      "Config/testthat/edition": "3",
      "Config/testthat/parallel": "true",
      "Config/testthat/start-first": "install,restore",
      "Config/autostyle/scope": "line_breaks",
      "Config/autostyle/strict": "true",
      "Config/autostyle/rmd": "false"
    }
  }
}
'
  expect_no_error(lockfile_validate(lockfile = lockfile))
  expect_true(lockfile_validate(lockfile = lockfile))

})


test_that("a known-good lockfile with extra fields passes validation", {

  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  # Lockfile adds a R$Nickname field not present in the schema
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Nickname": "Shortstop Beagle",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      },
      {
        "Name": "BioCsoft",
        "URL": "https://bioconductor.org/packages/3.8/bioc"
      }
    ]
  },
  "Python": {
    "Version": "3.10.12",
    "Type": "virtualenv",
    "Name": "./renv/python/virtualenvs/renv-python-3.10"
  },
  "Bioconductor": {
    "Version": "3.8"
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    },
    "mime": {
      "Package": "mime",
      "Version": "0.12.1",
      "Source": "GitHub",
      "RemoteType": "github",
      "RemoteHost": "api.github.com",
      "RemoteUsername": "yihui",
      "RemoteRepo": "mime",
      "RemoteRef": "main",
      "RemoteSha": "1763e0dcb72fb58d97bab97bb834fc71f1e012bc",
      "Requirements": [
        "tools"
      ],
      "Hash": "c2772b6269924dad6784aaa1d99dbb86"
    }
  }
}
'
  expect_no_error(lockfile_validate(lockfile = lockfile))
  expect_true(lockfile_validate(lockfile = lockfile))

})

test_that("a custom schema file can be used for successful validation", {

  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  # Custom schema adds a required R$Nickname field present in the lockfile
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Nickname": "Shortstop Beagle",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  }
}
'

  schema <- '
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "R": {
      "type": "object",
      "properties": {
        "Version": {
          "type": "string"
        },
        "Nickname": {
          "type": "string"
        }
      },
      "required": ["Version", "Nickname"]
    }
  }
}
'
  expect_no_error(lockfile_validate(lockfile = lockfile, schema = schema))
  expect_true(lockfile_validate(lockfile = lockfile, schema = schema))

})

test_that("a custom schema file can be used for failed validation", {

  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  # Custom schema adds a required R$Nickname field not present in the lockfile
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  }
}
'

  schema <- '
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "R": {
      "type": "object",
      "properties": {
        "Version": {
          "type": "string"
        },
        "Nickname": {
          "type": "string"
        }
      },
      "required": ["Version", "Nickname"]
    }
  }
}
'

  expect_false(lockfile_validate(lockfile = lockfile, schema = schema))

})

test_that("an incorrect Packages$Hash field fails validation", {

  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd"
    }
  }
}
'

  expect_false(lockfile_validate(lockfile = lockfile))

})

test_that("invalid JSON fails validation", {

  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  # Packages uses [] which is not valid JSON
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": [
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  ]
}
'
  expect_error(lockfile_validate(lockfile = lockfile, error = TRUE))

})

test_that("strict mode catches unknown keyword in provided schema", {

  skip_on_cran()
  skip_if_not_installed("jsonvalidate")

  # Custom schema provides "Version" with "type": "UNKNOWN"
  lockfile <- '
{
  "R": {
    "Version": "4.2.3",
    "Repositories": [
      {
        "Name": "CRAN",
        "URL": "https://cloud.r-project.org"
      }
    ]
  },
  "Packages": {
    "markdown": {
      "Package": "markdown",
      "Version": "1.0",
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "4584a57f565dd7987d59dda3a02cfb41"
    }
  }
}
'

  schema <- '
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "R": {
      "type": "object",
      "properties": {
        "Version": {
          "UNKNOWN": "string"
        }
      }
    }
  }
}
'
  expect_true(lockfile_validate(lockfile = lockfile, schema = schema))
  expect_error(
    lockfile_validate(lockfile = lockfile, schema = schema, strict = TRUE)
  )

})
