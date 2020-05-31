# analyzeJ2Kmodel
This package is for checking if you have rightly setup the J2000 hydrological model and also helps to create some basic plots out of your model. 
Enjoy :)

## Install the devtools package
To install a R package from github, start by installing the devtools package. The best way to do this is from CRAN, by typing:  
* _install.packages("devtools")_

## Install the package from GitHub
The devtools package provides install_github() that enables installing packages from GitHub. Now the package of interest from GitHub can be installed using the author name and the name of the package.   For example in the following example we are installing analzeJ2Kmodel created kabiraj404. (in GitHub jargon, the package is the repository)

* _install_github("kabiraj404/analzeJ2Kmodel")_ #( kabiraj404 is the author and analzeJ2Kmodel is the package name )

* _library(devtools)_

* _library(analyzeJ2Kmodel)_

If you are using R.4.0 you might get the following error while trying to install the package, it might be possibly due to the version mismatch so you can escape installing the newer versions. 
<img width="312" alt="installPackage1" src="https://user-images.githubusercontent.com/64681103/83350456-fbcba480-a35b-11ea-9199-ff3737035fcd.png">

## Install the dependent packages 
Once your package is installed on your mashine. YOu are ready to go. Before that make sure you have the package required for this package to run.    
Currently, the following packages are applied for the different functions used in the package therefore, its better if you copy and paste the following chunk of code 

_if (!require("data.table")) install.packages("data.table")    
library(data.table)    
if (!require("ggplot2")) install.packages("ggplot2")     
library(ggplot2)    
if (!require("dplyr")) install.packages("dplyr")    
library(dplyr)    
if (!require("tidyr")) install.packages("tidyr")    
library(tidyr)    
if (!require("knitr")) install.packages("knitr")     
library(knitr)    
if (!require("zoo")) install.packages("zoo")    
library(zoo)    
if (!require("scales")) install.packages("scales")    
library(scales)_    

Once the required package are in your machine. You are all set to use all the functions of the alayzeJ2Kmodel package.    
Here are some of the functions. 

## Running the analyzeJ2Kpackage package
Note: you should have workingFolder as the path where your jam file of the model is situated. In my case I do the following.    
_*workingFolder <- "D:\\R\\CreatingPackage\\test20200531\\j2k_dudh_koshi_modified\\"_

Lets starts with visulizing the input and output of the model. For this purpose use:    
* **J2K_inVSoutInfo()**     

NOTE: You might get the following error:     
_Error in fread(paste(workingFolder, "output\\current\\TimeLoop.dat", sep = ""),  : could not find function "fread"_        
This is possible because to read the timeloop.dat file of the model, I have used the _fread_ function from the _time.loop_ package and as requested above you might not have installed the dependent package. **Therefore, I would again suggest you to install all the required dependent package at first.** I am not sure but if it annoying, I will try to make these package to be installed by default while installing the _analyzeJ2K_ package by itself. But for now lets go ahead :) 

In my example I have get the following result. 


