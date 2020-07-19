# analyzeJ2Kmodel
This package is for checking some of the basic information while setting up the J2000 hydrological model in any basin. This package provides the basic glimpse of the  some basic plots/information of your basin. 
Enjoy :)

## Install the devtools package
To install a R package from github, start by installing the devtools package. The best way to do this is from CRAN, by typing:  
* _install.packages("devtools")_

## Install the package from GitHub
The devtools package provides install_github() that enables installing packages from GitHub. Now, the package of interest, from GitHub, can be installed using the author name and the name of the package. For example in the following example we are installing analzeJ2Kmodel created kabiraj404. *_(in GitHub jargon, the package is the repository)_

* _library(devtools)_

* _install_github("kabiraj404/analyzeJ2Kmodel")_ #( here, kabiraj404 is the author and analyzeJ2Kmodel is the package name )

* _library(analyzeJ2Kmodel)_
While using R.4.0, the following message might be seen while trying to install the package, Possibly due to the version mismatch, here the newer versions of packages can be skipped (3 in my case).     
<img width="550" alt="d2WgZaObkv" src="https://user-images.githubusercontent.com/64681103/87873950-838d7100-c9e5-11ea-99e8-76779225f591.png">

## Install the dependent packages 
Once the analyzeJ2Kmodel package is installed on the mashine. It is ready for the further analysis. Before that make sure the required suppliment packages for this package to run are on your mashine.    
Currently, the following packages are applied for the different functions used in the package therefore, its better if you copy and paste the following code 

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

Once the required package are in the machine. You are all set to use all the functions of the alayzeJ2Kmodel package.    
Here is some glimpse of each functions. 

## Running the analyzeJ2Kpackage package
Note: You should have workingFolder as the path where your jam file of the model is situated. In my case, I do the following. 
(I have also kept the dummy model with limited data inside the repository "SampleData/J2K_model" to check the package)
_*workingFolder <- "C:\\J2K_model\\"_

Lets starts with visulizing the input and output of the model. For this purpose use:    
* **_J2K_inVSoutInfo()_**     

NOTE: You might get the following error:     
_Error in fread(paste(workingFolder, "output\\current\\TimeLoop.dat", sep = ""),  : could not find function "fread"_        
This is possible because to read the timeloop.dat file of the model, I have used the _fread_ function from the _time.loop_ package and you might not have installed the dependent packages as requested above . **Therefore, I would again suggest you to install all the required dependent package at first.** I am not sure whether it is annoying, if it is, I will try to make these package to be installed by default while installing the _analyzeJ2Kmodel_ package.  

In my example I have get the following result. 

<img width="550" alt="INVSOUT" src="https://user-images.githubusercontent.com/64681103/83351237-e6597900-a361-11ea-8f66-f722871bf6af.png"> 
So, we can see that the model has run for the four years. And input and output for each year is shown in the figure. But we might get the question on what are the major components of the input and output. What is the precipitation or discharge values and so on. 


* **_J2K_WatBalMajorComps()_**      

The major component of the water balance are visualized as below:

<img width="550" alt="inoutvariables" src="https://user-images.githubusercontent.com/64681103/86511000-a8de8480-be14-11ea-973c-57efe2e3e0aa.png">

From this figure, we can see the major variables of the input and outpout. Additionally, the snow and glacier component amount are also displayed. All the values are in "mm" unit. It is an important plot for the visulization of the overall information of the basin. For further understanding of the basin, the daily water balance of the basin can be viewed using the given function.  

* **_J2K_WatBalplot()_**  

<img width="550" alt="watbal" src="https://user-images.githubusercontent.com/64681103/83352538-db0b4b00-a36b-11ea-8365-3f510334543b.png">     
You might not get the exactly this figure but it has to be near to zero. 

Now, the following function saves the summary data in the working folder in the "csv" format.The quick overview is also displayed in the console. The suppliment information you would get is the percentage of the different variables. For example: yearly percentage of glacier in runoff. percentage of snowrunoff, evapotranspiration etc. 

* **_J2K_WatBalsummarySave()_**  

<img width="550" alt="P5iTlFkVLm" src="https://user-images.githubusercontent.com/64681103/87873784-fe558c80-c9e3-11ea-85e5-01ca3732f8f4.png">

<img width="934" alt="write" src="https://user-images.githubusercontent.com/64681103/86511419-b7c73600-be18-11ea-9a64-7d88f6201b77.png">

Now, lets explore some function to explore the snow cover area of the basin/catchment. 


* **_J2K_snowcoverTS()_**     

It helps in the visulization of the daily snow cover area in each day. It helps to visualize the MODIS and the output from the model at the same instance. The other products can also be used in the model, however it needs some adjustment. I got the following figure. 

<img width="550" alt="snowcoverTS" src="https://user-images.githubusercontent.com/64681103/86511010-d297ab80-be14-11ea-89d5-c8c863cf7e71.png">    

Another way to visualize the snow cover of the area is by viewing the monthly average, for that purpose you can use the following function. 

* **_J2K_MonthMeanSC()_**         

<img width="550" alt="montlyMeanSC" src="https://user-images.githubusercontent.com/64681103/86511296-68343a80-be17-11ea-9cdb-69cb97df7803.png">  

Likewise, to view the snow cover in the annual scale, following function is used.  

* **_J2K_AnnualSumSC()_**         

<img width="550" alt="ANnualSnowSUm" src="https://user-images.githubusercontent.com/64681103/86511181-37073a80-be16-11ea-9d13-d3d113765364.png">    
It is important to visualize of the precipitation and discharge in the same unit to get the holistic water balance situation of the basin. Following function displays the precipitation and discharge in the same unit "mm",based on your intput data. 

* **_J2K_RainVsRunoffmmMonthly()_**         

<img width="550" alt="MOnthlyPvsQmm" src="https://user-images.githubusercontent.com/64681103/86511239-e2b08a80-be16-11ea-8434-33aa5e1520b5.png">   

While preparing this plot, the date format is asked, some of the date format are %d.%m.%Y; %Y-%m-%d, you can check your rain.dat, orun.dat or other input data to see the date format in start or end date. Now, lets visulize the annual plots

* **_J2K_RainVsRunoffmmYearly()_** 

<img width="550" alt="PvsQmmYearly" src="https://user-images.githubusercontent.com/64681103/86511254-fbb93b80-be16-11ea-902b-ce5f0040ccfb.png">   

One important issue while working in the model is the issue where the non-glacier HRUs supply water to the glacier area. for this purpose the non-glacier ID which are doing so are corrected and saved to the working folder. 

* **_J2K_FixmyNG2G()_** 

<img width="550" alt="DnGCgRPuDa" src="https://user-images.githubusercontent.com/64681103/87048308-4671f380-c21b-11ea-9d25-2db8e568444d.png">


These are some of the ideas and plots, if have amy more, share your idea. 




