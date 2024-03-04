# CellCyteLogistic
R package for fitting logistic curves to growth data collected on the CellCyteX machine

## Installation

Work is underway to create an R package. Until then:
1. Clone the repository.
2. Add the following dependencies:
tidyverse,
ggpubr,
readxl,
rstatix,
viridisLite
```
lapply(c("tidyverse", "ggpubr", "readxl", "rstatix", "viridisLite"), require, character.only=T)
```
3. Now load all the functions for CellCyteLogistic (replacing PATH_TO_GIT_CLONE with the path to which you downloaded the functions):
```
invisible(lapply(list.files("PATH_TO_GIT_CLONE", recursive = T, full.names = T, pattern = ".R$"),source))
```
4. You are now ready to analyze your data

## Data Analysis Example

1. Download 3D growth data from the CellCyteX software. *(Note: this can be adapted for 2D confluency data, but that is not yet implemented)*
2. Create a metadata table that describes your experiment. There should be a "Well" column, which includes an entry for every well included in the analysis, and a "Line" column that represents the groups used in the analysis. *(Note: it has to be named "Line" even if you're not grouping lines - this will be updated for clarity moving forward)* For example:
      ```
      metadata<-data.frame(
        Well=paste0(LETTERS[1:8], rep(1:12, each=8)),
        Line=c(rep(paste0("RMIMDM_", rev(seq(0.2, 2, by=0.2)), "x_FGmSFv4100x"), each=8),
               rep("CytivaIMDM_FGmV5.1.2",8),
               rep("CytivaIMDM_FGmSFv4100x",8)
               )
      )
      ```
      *(Note: a function to import metadata from CellCyte Studio analysis json files may be developed in the future)*

3. Load the data using ```CellCyte_Input()```::
    ```
    data <- CellCyte_Input("PATH/TO/DATA.xlsx", metadata)
    ```
    *(Note: the ```excel=F``` option can be used if data was downloaded in csv format)*
4. Run the ```CellCyte_Optim()``` function to generate fits::
    ```
    data <- CellCyte_Optim(data)
    ```
5. Use ```CC_plot()``` to look at an individual Line/Group to check the fits::
    ```
    CC_plot(data, "CytivaIMDM_FGmSFv4100x")
    ```
    <img src="http://github.com/dylanrahe/CellCyteLogistic/blob/main/test_fit_plot.png?raw=true" width="431" height="415">
    
    _Figure 1: Sample CC_plot() plot of fits (red line) amongst all well data (grey dots). Light grey dots were are determined to be outliers and exluded from fit function_
6. Use ```CC_SlopesPlot()``` to plot the slope of the maximum growth rate (inflection point) of each well's fit:
    ```
    CC_SlopesPlot(data)
    ```
    <img src="http://github.com/dylanrahe/CellCyteLogistic/blob/main/test_slopes_plot.png?raw=true" width="740" height="615">
    
    _Figure 2: Sample CC_SlopesPlot() boxplot of slopes from each well (colored dots) and from fit using all wells in a Line/group (grey dot)._
7. If a reference group exists, the ```refGroup`` option can be used to include statistics, and the ```title``` option can be used to change the title:
    ```
    CC_SlopesPlot(data, refGroup="RMIMDM_1x_FGmSFv4100x", title="RMIMDM AA Titration")
    ```
    <img src="http://github.com/dylanrahe/CellCyteLogistic/blob/main/test_slopes_plot2.png?raw=true" width="740" height="615">
    
    _Figure 3: Sample CC_SlopesPlot() boxplot as above, but with statistics and title._
8. Similarly, the carrying capacity, K, can be plotted using ```CC_Kplot()``` and the same syntax/options:
    ```
    CC_Kplot(data, refGroup="RMIMDM_1x_FGmSFv4100x", title="RMIMDM AA Titration, Carrying Capacity")
    ```
    <img src="http://github.com/dylanrahe/CellCyteLogistic/blob/main/test_k_plot.png?raw=true" width="740" height="615">
    
    _Figure 4: Sample CC_Kplot() boxplot of carrying capacity with statistics and title._
    




