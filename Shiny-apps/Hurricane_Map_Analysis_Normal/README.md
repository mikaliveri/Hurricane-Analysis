This folder...

![](Map-Analysis-App-Normal.png)

To run the app in RStudio, execute the following code in R:

```r
library(shiny)

# Run an app from a subdirectory in the repo
runGitHub(
repo="Hurricane-Analysis",
username = "mikaliveri",
subdir = "Shiny-apps/Hurricane_Map_Analysis_Normal"
)