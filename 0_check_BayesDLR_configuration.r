required_packages <-
  c("config", "dplyr", "openssl", "openxlsx", "optimx", "readr", "readxl",
    "stringr", "tidyr")

installed_packages <-
  as.data.frame(installed.packages()[, c("Package", "Version")])

packages_to_install <-
  setdiff(
    x = required_packages,
    y = installed_packages
  )

install.packages(
  pkgs = packages_to_install
)