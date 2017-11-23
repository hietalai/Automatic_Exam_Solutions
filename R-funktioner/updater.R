if(!require(installr)){
  install.packages("installr")
  require(installr)
}

updateR(browse_news = FALSE,
        install_R = TRUE,
        copy_packages = FALSE,
        keep_old_packages = FALSE,
        update_packages = FALSE,
        use_GUI = FALSE)

# Uninstalling the second to last version of R on the device
uninstall.R()

# #Restarts R-session
# .rs.restartR()

packages <- c("GGally",
              "ggplot2",
              "gridExtra",
              "RColorBrewer",
              "digest",
              "xtable",
              "rmarkdown",
              "mvtnorm",
              "pxweb",
              "survey",
              "xlsx",
              "knitr",
              "DMwR",
              "ggmap",
              "RPostgreSQL",
              "reshape2",
              "R.utils",
              "stringr",
              "lubridate",
              "MASS",
              "dbscan",
              "animation",
              "DBI",
              "dplyr",
              "arules",
              "arulesSequences",
              "tm",
              "SnowballC",
              "wordcloud",
              "diagram",
              "neuralnet",
              "formatR",
              "devtools",
              "tidyr",
              "shiny")

install.packages(packages)

# For ggplot2 book
library(devtools)
if (packageVersion("devtools") < "1.9.1") {
  message("Please upgrade devtools")
}
devtools::install_deps()
devtools::install_github("hadley/oldbookdown")

# STIMA Packages
install.packages("devtools", repos = "https://cloud.r-project.org")
cat("devtools installed...\n")
devtools::install_github("STIMALiU/ComputerLabs", subdir = "RPackage")
cat("stimaRpackages...\n")
stimaRpackages::install_stima_packages(course = "X732G31.Isak.Hietala")

stimaRpackages::get_r_course_gsheet()
stimaRpackages::get_stima_r_courses()



