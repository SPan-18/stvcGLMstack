# Required package names
packages <- c("pak")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], 
                   repos = "http://cran.us.r-project.org")
}

# install spStack from GitHub respository SPan-18/spStack-dev
pak::pak("SPan-18/spStack-dev")