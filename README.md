# FHMs_daily_COVID-19_reports

This programm would be written in R. And the results(figure ploted by plotly) would save in form of html.

# Project descreption


# About the resource data
The resource of data comes from [Folkhalsomyndigheten](https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/statistik-och-analyser/bekraftade-fall-i-sverige/), by using the Wayback Machine to get the old data. 

The python script for getting old data is in [WayBackMachine_FHM_COVID-19_statistics](https://github.com/ChenjianS47/WayBackMachine_FHM_COVID-19_statistics)

# Current Target

The first part of the project is to download (appropriately naming) Folkhalsomyndigheten’s daily (for those days that it was provided) excel spreadsheet report concerning COVID19 in Sweden from Wayback machine. Then, to design a database (in R) that stores all the the information in those sheets, implement a program (R) that reads in all of the sheets. The program should read these sheets from a given directory on the hard drive. The program should be flexible in the sense to read all the files in the given directory, not have the file names hardcoded, as new files can be added at a later stage. The database and program have to be flexible as sheet names in the excel files evolve, e.g. new ones are added can be renamed. They should be flexible for the future if FHM adds new, renames, removes sheets. The R database should be designed in such a way that is easy to use, with unproblematic extraction of the data from it. As part of the final report the downloaded spreadsheets and all the R code need to be handed-in as publicly available data and R package on GitHub or CRAN.

# Finished 
1. Load the data from the .xlxs file and name the datafram with the name of the sheet.
2. Seperate the total data to daily changes
3. User could define the name of the file and sheet they need

# License

This programm is distributed under the MIT license. See
[LICENSE.md](https://github.com/ChenjianS47/FHMs_daily_COVID-19_reports/blob/main/LICENSE)
for more information.
