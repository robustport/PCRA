Companion to PCRM

Please update to latest R, R-3.4.4 (as of March 2018) 

# install

Since the update of devtool 2.0.0, there is a bug in devtool that making DESCRIPTION file not able to be read

work around:

library(devtools)

install_version("devtools", version = "1.13.6", repos = "http://cran.us.r-project.org")

install_github('kecoli/PCRM')

runExample("portOpt")

runExample("perfMetric")
 
 
