# R at Sea

This repository contains the materials used in the 24 - 26 January 2020 R at Sea Course offered by [Field School](www.getintothefield.com) in Miami, Florida.

### Set Up Instructions  

1. [Install R](https://www.r-project.org/)  
- If you are using Windows, you can do this simply by downloading and running [this .exe file](https://cran.r-project.org/bin/windows/base/release.htm). If you are using Mac or Linux, you can follow [this link](https://cran.r-project.org/mirrors.html), select the location nearest to your institution to access the files you will need to download. You should have R version 3.6.1.  

2. Install the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/#download) by selecting the 1.2.5001 version appropriate for your operating system (Windows, Mac, or Linux/Ubuntu).  

*Note that if you have separate user and admin accounts, you should run the installers as administrator (right-click on .exe file and select "Run as administrator" instead of double-clicking). Otherwise problems may occur later, for example when installing R packages.*  

3. Download the files contained in [the current repository](https://github.com/DanielleQuinn/R-at-Sea-January2020) by clicking the green button that says "Clone or Download" and selecting "Download ZIP".  

4. Unzip these files into a folder on your Desktop called "RSea"  

5. If you are comfortable doing so, run the following lines of code in RStudio (ensuring that you're connected to the internet) to install the main packages we'll be using during this workshop. If you need assistance with this, we'll be covering this process in more detail on Day One of the workshop.  

`install.packages("ggplot2")`  
`install.packages("lubridate")`  
`install.packages("dplyr")`  
`install.packages("tidyr")`  
`install.packages("MASS")`  
`install.packages("Ecdat")`  

### Module 1: Introduction to R and RStudio     
**introduction_BLANK.R** includes an outline and can be used to code along with the instructor (recommended)  
**introduction.R** includes notes and code  

### Module 2: Data Wrangling and Visualization  
**dplyr_ggplot2_BLANK.R** includes an outline and can be used to code along with the instructor (recommended)  
**dplyr_ggplot2.R** includes notes and code  
**visualization.PNG** contains a plot that will be used during an exercise in this module  

### Module 3: Introduction to Linear Models  
**introduction_linear_models_BLANK.R** includes an outline and can be used to code along with the instructor (recommended)  
**introduction_linear_models.R** includes notes and code  

### Module 4: Generalized Linear Models  
**generalized_linear_models_BLANK.R** includes an outline and can be used to code along with the instructor (recommended)  
**generalized_linear_models.R** includes notes and code  

### Module 5: Mixed Effect Models  
**mixed_effect_models_BLANK.R** includes an outline and can be used to code along with the instructor (recommended)  
**mixed_effect_models.R** includes notes and code  

### Data  
*The data used in this workshop is a modified version of the publicly avilable data cited below. It is used only as a teaching tool and the modifications and/or use of this data for the purpose of this workshop is not representative of the original data or its intentions.*  

**KingsCreekFishSurveys.csv**  
Gido, K. B. 2018. CFP01 Fish population on selected watersheds at Konza Prairie . Environmental Data Initiative. http://dx.doi.org/10.6073/pasta/e1ac8845d032315df70819ece0d37c42.

**new_dragonflies.csv** and **orchids.csv** contain simulated data only used for teaching purposes
