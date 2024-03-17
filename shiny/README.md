# CellCyteLogistic Shiny Implementation

## Installation
1. This does not include shiny server setup and installation. <a href="https://www.digitalocean.com/community/tutorials/how-to-set-up-shiny-server-on-ubuntu-20-04">Digital Ocean</a> has a good guide for installing on Ubuntu.
2. Clone CellCyteLogistic (see README for installation instructions in folder above), and edit the directory location in the first lines of app.R to direct the shiny app to the git clone.
```
//put the relevant line here///
```
3. Add a directory to your shiny server directory (typically /srv/shiny-server/).
4. Move ```app.R``` and the ```www/``` directory to that directory.
5. Restart the shiny server. Typically (for Ubuntu):
```
sudo systemctl restart shiny-server
```
6. Test the installation by navigating to the local shiny server implementation. The default port is 3838:
```
http://localhost:3838/Rshiny/YOUR_CCLOG_SHINY_DIR_NAME
```
7. This should now be accessible on your network.

## Usage
1. Use the mouse to highlight "wells" of the 96-well plate.
2. Enter a name for this experimental group in the text input box
3. Click "Enter Group Name"
4. Repeat for each group until all wells with experimental data are specified.
5. Alternatively, load a raw data file and click "Autopopulate Groups" and each column in the data will be named sequentially (Sample1, Sample2, etc.) according to how many rows are present in the data.
6. Once data is loaded and well groups are specified, click "Run CellCyteLogistic" to perform calculations
7. Now the "Slopes" and "Carrying Capacity" etc. tabs are populated with figures from the ```CC_Slopes()``` or ```CC_ParamPlot.R()```.
8. The "Fits" tab contains the calculations of the fits as table data
9. The tabs have download buttons in the top right for downloading
10. The "Show statistics" and "Plot Outliers" radio buttons allow customizing of the plots before downloading






