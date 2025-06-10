# Human_heart_metabolome_atlas

## Overview

This directory contains the code and demo data for the interactive web tool in this article. The web tool depends on R  package shiny.



### **Installation for R and R-studio on Linux Mint OS:**

```
# R-base
$ sudo apt-get update
$ sudo apt-get install r-base

# R-studio 
$ sudo apt-get install gdebi-core
$ wget -c https://download1.rstudio.org/electron/jammy/amd64/rstudio-2025.05.1-513-amd64.deb
$ sudo gdebi rstudio-2025.05.1-513-amd64.deb
$ echo "export RSTUDIO_WHICH_R=/path_to_R_installed/lib/R/bin/R" >> ~/.bashrc
```



### **Installation for R packages:**

```R
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("readr")
install.packages("sf")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggrepel")
```



### **Run the shiny application:**

Start the r-studio program by using Linux command:

```
$ rstudio app.R   
```

Then, click on the button 'Run App' on the panel to run the shiny application.



### Reference

[Shiny - Getting Started](https://shiny.posit.co/r/getstarted/build-an-app/hello-shiny/getting-started.html)