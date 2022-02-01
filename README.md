# MSNA-Burst-Analysis
Automated quantification of the integrated sympathetic neurogram

# Introduction
Muscle sympathetic nerve activity (MSNA) is typically quantified from an integrated neurogram in which the raw MSNA signal is bandpass filtered, rectified, and integrated with a leaky integrator (time constant of 0.1s). The integrated MSNA neurogram then undergoes burst detection to determine bursts of sympathetic activity that are greater than a pre-defined signal-to-noise ratio (typically 3:1). Bursts can then be assessed for their latency from the originating R-wave, amplitude, and area. After acquiring a list of bursts subsequent analysis can be conducted to determine burst frequency, and burst amplitude/area can be normalized as appropriate for the experimental conditions. Additional analysis may include the quantification of total sympathetic activity or other similar composite metrics which have been published on previously. Many laboratories that specialize in this area of research have their own in-house custom software for detecting MSNA bursts but source code is not made readily available to other researchers. Additionally, these software can be out-dated and thus not functioning on today's current operating systems. 

The shiny app provided here is loosely based on my interpretation of the methodology published by [Hamner JW & Taylor JA](https://journals.physiology.org/doi/full/10.1152/jappl.2001.91.3.1199) in 2001. However, this shiny app offers unique data visualization features permitting the user to quickly identify optimal burst detection settings and the flexibility to select or deselect bursts which do not meet the scorers criteria.

# Getting set up
To run this application on your system you must first install [R](https://cran.r-project.org/) and [R Studio](https://www.rstudio.com/) both freely available and open source. 

After installing R and R Studio, launch R Studio and use this command in the console to install 'shiny' permitting you to directly launch the app from the github repository.

`install.packages('shiny')`

The application has other R package dependencies which should automatically install when you first run the application.  If you run into problems the following dependent packages can be installed manually:

1. remotes
2. shiny
3. shinyBS
4. tidyverse
5. MESS
6. zip
7. thematic
8. shinythemes
9. signal
10. features
11. plotly (development version)

Packages can be installed manually using the code:

`install.packages('<package name>')`

To install plotly's development version use:

`remotes::install_github("ropensci/plotly")`

# Running the application
After installing R, Rstudio, and shiny, you can run this application without the need to download it directly from github by running this command from the console in R studio.

`shiny::runGitHub('MSNA-Burst-Analysis', 'gefoster11', ref="main")`

The application should run in your default browser. Using this approach will ensure you are always using the latest source code available within this github repository.

# Preparing your input data
Within this GitHub repository you will find example data files including a clean and noisy data set. To download these files you can click on the green "code" button and download a ZIP file of the entire repository. Once unzipped you can access the needed beat file and the raw signal files for either the clean or noisy data sets.  These files can then be drag and dropped to the appropriate file input on the launched application.

To run the software with your own data files, you will need to generate and prepare two data files. A beat file which contains a one row header (variable names) and contains the time in seconds of each R-wave from your ECG signal within the first column. The file should be saved as a comma separated file. A raw signals file contains a standard ADInstruments labchart 9 row header with columns in the following order: Time in seconds, arterial pressure, electrocardiogram, and the integrated MSNA signal. All signals should be downsampled to 25 Hz. This file should be saved as a tab-delimited data file.

*Note: Although the app can manage data of different sampling frequencies downsampling to 25 Hz provide a reasonable balance between processing power and signal integrity. Don't use LabChart? No problem, just provide 9 blank rows in your file.*

# Workflow within the App
After running the application drag and drop your beat and signals file into the correct file input. 'Start' and 'End' times should populate as should the loaded data on the 'Input Data' tab. Confirm data has loaded as expected. Confirm your analysis settings are correct. Choose your expected latency depending upon the nerve location and your participant. For fibular nerve this should be ~1.3 s. The application will look this far ahead from each R-wave in your selection and then select a time window on either side of this location to look for a peak. The time window defaults to +/- 300 ms and should be appropriate for most applications. This is a first pass assessment of bursts. A second pass will follow where the windowing is based on the R-to-R interval of the originating R wave. The signal-to-noise ratio (SNR) is typically set to 3. This is only used for display purposes based upon your selected 'Burst Threshold'. For a given Burst Threshold a noise band will be illustrated on the MSNA Burst Selection so that you can visualize it and a dotted red line will demonstrate the burst threshold. This will only be illustrated after you click the 'Analyze Selection' button. The 'Noise Threshold' will permit you to remove noise spike that commonly are detected as bursts by simple peak detection algorithms. The smaller the number the more large bursts will be removed from your analysis. Set the burst Threshold to set your burst amplitude criteria.

Select 'MSNA Burst Selection' tab to view input data. Adjust start and end time to capture a region for analysis. The graph will adjust to the selected time. It is possible click and drag on the graph to zoom in on specific areas or the bottom data viewer can be adjusted in size and dragged across data to gain a closer look. Also, hovering over the graphs will present a toolbar in the top right of the graph. Selecting 'toggle Spike lines' will give a tool that can be helpful for estimating the magnitude of your noise and can be used to estimate your 'Burst Threshold'. For example, if the noise band is roughly 0.05 volts wide and your SNR is set to 3 then multiply by 3 to get your desired 'Burst Threshold'. If happy with your data selection and settings then click 'Analyze Selection'. A progress bar will display in lower right corner. The analysis time will depend on how much data you are asking the program to analyze.

After running 'Analyze Selection' the graph will re-draw and will display red and grey symbols on each possible burst. The red markers denoting those meeting your criteria and the grey markers denoting bursts not accepted.  It is possible to click on these markers to toggle the burst -on or -off. Hovering over these markers will provide details regarding the burst characteristics. The blue crosses denote the identified start and end point of the selected bursts. The blue ribbon displays the noise band while the red dotted line represents the burst threshold. The neurogram presented below this graph shows the bursts only with noise removed. If you wish to carefully review your data click 'First' at the top and select a 30s or 60s x-axis range. Now you can navigate through the file using the 'Next' and 'Previous' buttons from window to window.

The 'MSNA Burst Properties' tab is a useful window for visualizing your data. The upper left graph displays your identified bursts in red and your excluded bursts in grey. The upper right graph displays the burst latency versus burst amplitude relationship. From here it is very easy to identify the noise band and choose a suitable burst threshold. The vertical dotted line denotes your burst threshold while the horiztonal dashed line denotes the average latency based on the accepted bursts. The lower two plots provide information regarding fall and rise ampitude and slope of each burst. Hovering over data in any of these plots provides useful information about each burst. Clicking on a data point or burst will isolate that burst in all four plots permitting you to identify it. If you wish to include or exclude a selected point click the 'Toggle Selected Data' button. Click the 'Reset' button to reset all toggled bursts.

The 'Analyzed Data' tab provides a tabular view of the outputted data. To download your analysis click 'Download'. This will output a zip file containing your tabulated bursts and also a file containing the raw neurograms after zeroing (1), filtering (2), and noise removal (3) which may be useful for data presentation. The 'Merge Output' checkbox allows you to merge your beat-by-beat file with your burst analysis which can be helpful for facilitating other analyses or streamlining workflow.

# Conclusion
This application provides a simple tool for MSNA burst detection which can subsequently be used for determining standard MSNA metrics. Although the application has some nuances the workflow described above will minimize these issues. If in utilizing this application you identify other critical errors please provide a minimally reproducible example so other users are aware of the issue and so they can be fixed. I urge others to develop similar analysis applications to build a collective environment where signal processing tools can be shared and contribute to the advancement of our field.

# Referencing
If you use this application in your research, please reference our methodology article: <br>
Foster GE, Shafer BM, Shing C. (2021) [An open-source application for the standardized burst identification from the integrated muscle sympathetic neurogram](https://journals.physiology.org/doi/abs/10.1152/jn.00397.2021). *Journal of Neurophysiology*. 126(5): 1831-1841.
