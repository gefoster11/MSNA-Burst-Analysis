# MSNA-Burst-Analysis
Automated quantification of the integrated sympathetic neurogram

# Introduction
Muscle sympathetic nerve activity (MSNA) is typically quantified from an integrated neurogram in which the raw MSNA signal is bandpass filtered, rectified, and integrated with a leaky integrator (time constant of 0.1s). The integrated MSNA neurogram then undergoes burst detection to determine bursts of sympathetic activity that are greater than a pre-defined signal-to-noise ratio (typically 3:1). Bursts can then be assessed for their latency from the originating R-wave, amplitude, and area. After acquiring a list of bursts subsequent analysis can be conducted to determine burst frequency, and burst amplitude/area can be normalized as appropriate for the experimental conditions. Additional analysis may include the quantification of total sympathetic activity or other similar composite metrics which have been published on previously. Many laboratories that specialize in this area of research have their own in-house custom software for detecting MSNA bursts but source code is not made readily available to other researchers. Additionally, these software can be out-dated and thus not functioning on today's current operating systems. 

The shiny app provided here is loosely based on my interpretation of the methodology published by [Hamner JW & Taylor JA](https://journals.physiology.org/doi/full/10.1152/jappl.2001.91.3.1199]) in 2001. However, this shiny app offers unique data visualization features permitting the user to quickly identify optimal burst detection settings and the flexibility to select or deselect bursts which do not meet the scorers criteria.

# Getting set up
To run this application on your system you must first install [R](https://cran.r-project.org/) and [R studio](https://www.rstudio.com/) both freely available and open source. 

In order to run directly from github you must first install 'shiny'

`install.packages(shiny)`

# Running the application
After installing R, Rstudio, and shiny, you can run this application without the need to download it directly from github by running this command from the console in R studio.

`shiny::runGitHub('MSNA-Burst-Analysis', 'gefoster11', ref="main")`

The application should in your default browser. Using this approach will ensure you are always using the latest source code available within this github repository.

# Preparing your input data
Within this repository you will find example data files including a clean and noisy data set. Download this files and try to load them in the application to confirm normal functionality.

To run the software with your own data files, you will need generate and prepare two data files. A beat file which contains a one row header (variable names) and contains the time in seconds of each R-wave from your ECG signal over youyr study duration and saved as a comma separated file. A raw signals file which contains a standard ADInstruments labchart 9 row header with columns in the following order: Time in second, arterial pressure, electrocardiogram, and the integrated MSNA signal. All signals should be downsampled to 25 Hz. Although the app can manage data of different sampling frequencies downsampling to 25 Hz provide a reasonable balance for processing power while mantaining signal integretity. This file should be saved as a tab-delimited data file.

# Workflow within the App
After running the application drag and drop your beat and signals file into the correct file input. Start and End times should populate as should the loaded data on the 'Input Data' tab. Confirm data has loaded as expected. Confirm your analysis settings are correct. Choose your expected latency depending upon the nerve location and your participant. For fibular nerve this should be around 1.3 s. The application will look this far ahead for each R-wave in your selection and then select a time window on either side of this location to look for a peak. The time window defaults to +/- 300 ms and should be appropriate for most applications. This is a first pass assessment of bursts. A second pass will follow where the windowing is based on the R-to-R interval of the originating R wave. The signal-to-noise ratio (SNR) is typically set to 3. This is only used for display purposes based upon your selected 'Burst Threshold'. For a given Burst Threshold a noise band will be illustrated on the MSNA Burst Selection so that you can visualize it and a dotted red line will demonstrate the burst threshold. This will only be illustrated after you click the 'Analyze Selection' button. The 'Noise Threshold' will permit you to remove noise spike that commonly are detected as bursts by simple peak detection algorithms. The smaller the number the more large bursts will be removed from your analysis.




