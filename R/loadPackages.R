#load packages needed

loadPackages <- function(myPackages) {
  # Checks for and installs missing packages
  new.packages <- myPackages[!(myPackages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) {
    install.packages(new.packages)
  }

  # Loads packages
  lapply(myPackages, require, character.only = TRUE)
}
