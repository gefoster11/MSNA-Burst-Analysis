#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This application is meant to load a beat-by-beat data file with 
# time in the first column and a one row header save as a .csv file and a raw data file which contains a labchar header
# and contains time in seconds, finometer bp, ecg, and integrated MSNA as a reasonable sampling frequency (e.g., 25 Hz)
# The selection criteria for bursts can be adjusted to automatically remove noise and select bursts.
# Bursts can be clicked on to toggle them on or off if then haven't been detected or excluded correctly.
# Once finished click the download button to save a .zip file containing the selected burst data and
# a neurogram file containing the original signal, a filtered signal, the original signal filtered to zero DC
# offset, and finally a MSNA_vjust channel which removes all noise only showing the detected bursts.
# The analysis is largely based on the methodology described in the reference provided at the bottom of the app

source("./functions/helper_functions.R")

# Sets max file size to 30 Mb
options(shiny.maxRequestSize = 30*1024^2, scipen = 999)

# Package Dependency
packages = c("remotes",
             "shiny",
             "shinyBS",
             "tidyverse",
             "MESS",
             "zip",
             "thematic",
             "shinythemes",
             "signal",
             "features")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Package Dependency - Install plotly from github
   if (!require("plotly", character.only = TRUE)) {
      remotes::install_github("ropensci/plotly")
      library("plotly", character.only = TRUE)
    }

# ---- Define UI for data upload app ----
ui <- fluidPage(
    theme = shinytheme("united"),
    # ---- App title ----
    titlePanel("MSNA Burst Detector"),
    
    # ---- Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # ---- Sidebar panel for inputs ----
        sidebarPanel(width = 3,
            
            # ---- Input: Select beat file ----
            fileInput("f_beat", "Choose Beat File",
                      accept = ".csv"),

            # ---- Input: Select beat file ----
            fileInput("f_MSNA", "Choose MSNA File",
                      accept = ".txt"),
            
            # # ---- Horizontal line ----
            # tags$hr(),
            # 
            # # ---- Input: Head display? ----
            # radioButtons("disp", "Display",
            #              choices = c(Head = "head",
            #                          All = "all"),
            #              selected = "head"),
            #  bsTooltip("disp", "toggle to display whole file",
            #           "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Input: start and end times ----
            numericInput("start", label = "Start (s)", value = NA, width = "90%"),
             bsTooltip("start", "set start time of your experiment",
                      "right", options = list(container = "body")),
            numericInput("end", label = "End (s)", value = NA, width = "90%"),
             bsTooltip("end", "set end time of your experiment",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Output sample frequency ----
            textOutput("fs"),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- MSNA Select Controls ----            
            numericInput("latency", label = "Expected Latency", value = 1.3, width = "90%"),
             bsTooltip("latency", 
                      "Set expected latency, default = 1.3",
                      "right", options = list(container = "body")),
            numericInput("t_window", label = "Time Window", value = 0.3, width = "90%"),
             bsTooltip("t_window", 
                      "Initial time window in seconds, default = 0.3s",
                      "right", options = list(container = "body")),
            numericInput("SNR", label = "SNR", value = 3, width = "90%"),
            bsTooltip("SNR", 
                      "Desired Signal-to-Noise Ratio, default = 3",
                      "right", options = list(container = "body")),
            numericInput("noise", label = "Noise Threshold", value = 15, width = "90%"),
             bsTooltip("noise", 
                       "For removing noise spikes - a lower number will remove more bursts",
                       "right", options = list(container = "body")),
            numericInput("BurstThreshold", label = "Burst Threshold", value = 0.2, width = "90%"),
             bsTooltip("BurstThreshold", "set to optimize burst detection based on burst amplitude",
                       "right", options = list(container = "body")),
            tags$hr(),
            actionButton("Analyze", "Analyze Selection", width = "90%"),
             bsTooltip("Analyze", "click to analyze bursts",
                      "right", options = list(container = "body")),
            tags$hr(),
            actionButton("reset", "Reset", width = "90%"),
            bsTooltip("reset", "click to reset selected bursts",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),

            # ---- Input: Save Output File ----
            downloadButton("save", "Download"),
            bsTooltip("save", "saves the selected data to a csv file", "right", 
                      options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Reference ----
            tags$div(
              HTML("<p>2021; Created by Glen Foster</p><br>")),
            
            tags$div(
              HTML("<p><b><u>Modification of the analysis first described by:</b></u><br> Hamner JW & Taylor JA (2001). <u>Automated quantification of sympathetic beat-by-beat activity, independent of signal quality.</u> <i>Journal of Applied Physiology. </i> <b>91</b>, 1199–1206.
                   </p>")),

        ),
        
        # ---- Main panel ----
        mainPanel(
            
            # ---- Output: Tabset w/ parsing table, selected data, and plot ----
            tabsetPanel(type = "tabs",
                        tabPanel("Input Data", 
                                 dataTableOutput("beat"),
                                 dataTableOutput("MSNA")),
                        tabPanel("Analyzed Data", 
                                 dataTableOutput("data")),
                        tabPanel("MSNA Burst Selection",
                                 
                                 fluidRow(
                                   # ---- Input: RangeSlider Scale ----
                                   column(width = 2, align = "center",
                                     radioButtons("range", "Choose Range",
                                                  choices = c("30s" = "30s",
                                                              "60s" = "60s",
                                                              "All" = "all"),
                                                  selected = "all", width = "20%"),
                                     bsTooltip("range", "Choose display range",
                                               "right", options = list(container = "body")),
                                   ),  
                                   
                                   column(width = 6, align= "center",
                                          tags$hr(),
                                     actionButton("first", label = "First", width = "30%"),
                                     bsTooltip("first", "set range slide to start of selected data",
                                               "right", options = list(container = "body")),
                                     
                                     actionButton("next", "Next", width = "30%"),
                                     bsTooltip("next", "next bin",
                                               "right", options = list(container = "body")),
                                     
                                     actionButton("previous", "Previous", width = "30%"),
                                     bsTooltip("previous", "previous bin",
                                               "right", options = list(container = "body")),
                                     tags$hr()
                                   ),
                                     
                                     
                                     
                                     # ---- Horizontal line ----
                                 ),
                                 
                                 tags$hr(),
                                 
                                 # tags$div(
                                 #   HTML("<p>-Click to toggle bursts</p><br>")),
                                 
                                 # ---- Main Plot ----
                                 plotlyOutput("plot", height = "900px")
                                 ),
                        tabPanel("MSNA Burst Properties",
                                 # ---- Horizontal line ----
                                 tags$hr(),
                                 
                                 fluidRow(
                                   # ---- Input: RangeSlider Scale ----
                                   column(width = 3, align = "center",
                                          actionButton("toggle", "Toggle Selected Data", width = "75%"),
                                          bsTooltip("toggle", "toggle selected data to remove or add burst",
                                                    "right", options = list(container = "body")),
                                          tags$hr()
                                   ),
                                   # column(width = 4, align = "left",
                                   #    tags$div(
                                   #          HTML("<p>-If analyzing long files use start and end time fields to select smaller chunks of data to improve
                                   #               performance</p><br>")),
                                   #            )
                                   ),
                                   
                                 
                                 # ---- Secondary Plot
                                 plotlyOutput("plot2", height = "1000px")
                                 )
                        )
            )
    )
)

# ---- Define server logic ----
server <- function(input, output) {
    
    # ---- initialize reactive values ----
    values <- reactiveValues(
        beat = NULL,
        beat_select = NULL,
        MSNA = NULL,
        fs = NULL,
        df = NULL,
        neurogram = NULL,
        burst_keep = NULL,
        burst_discard = NULL,
        x_start = NULL)
    
    # ---- Listen: delimiter inputs ----
    loadlisten <- reactive({
        list(input$f_beat$datapath, input$f_MSNA$datapath)
    })
    
    # ---- Observe: data load - df ----
    observeEvent(loadlisten(), {
        
        req(input$f_beat$datapath, input$f_MSNA$datapath)
      
      #browser()  
      
        tryCatch( {
            # load Beat
            beat <- read_csv(file = input$f_beat$datapath) %>% 
                rename(time = Time) %>%
                select(time) %>%
                mutate(RRI = lead(time) - time)
            
            values$beat <- beat[!is.na(beat$RRI),]
            
            # load MSNA
            MSNA <- read_tsv(file = input$f_MSNA$datapath, skip = 9, col_names = c("MSNA_time", "BP", "ECG", "MSNA_v"))
            
            # determine and set fs
            fs <- round(1/(MSNA$MSNA_time[2] - MSNA$MSNA_time[1]), 1)
            values$fs <- fs
            
            #### set bandpass filter (0.05-2Hz) and high pass filter to remove DC offset of original signal
            MSNA <- load_MSNA(MSNA, fs)
            
            values$MSNA <- MSNA
            
            #### set start and end inputs
            time_min <- min(MSNA$MSNA_time)
            time_max <- max(MSNA$MSNA_time)
            updateNumericInput("start", value = round(time_min, 1), session = getDefaultReactiveDomain())
            updateNumericInput("end", value = round(time_max, 1), session = getDefaultReactiveDomain())
            updateNumericInput("x_axis_start", value = time_min, session = getDefaultReactiveDomain())
            updateNumericInput("x_axis_end", value = time_max, session = getDefaultReactiveDomain())
            
        }, error = function(e) {
            stop(safeError(e))
        }
        )
    })

    # ---- Output: sample frequency ----
    output$fs <- renderText({
        fs <- values$fs
        print(paste0("Sample Frequency = ", fs))
    })
    
    # ---- Observe: data loaded set data ----
    observeEvent(input$Analyze, {
        withProgress(message = "Calculation in Progress - be patient!", {
            
          #browser()
         
            req(input$f_MSNA, input$f_beat)
            
            t_start <- input$start
            t_end <- input$end
            
            beat_sel <- values$beat %>% plotly::filter(time >= t_start & time <= t_end)
            MSNA <- values$MSNA
            
            df <- local_maxima(beat_sel, MSNA, input$latency, input$t_window)
            
            incProgress(amount = 0.25)
            #function to get windowed data from local maxima within each windowed beat.
            df <- df[!is.na(df$RRI),] %>% select(beat_no, beat_time, RRI, latency, t_max) %>% group_by(beat_no) %>%
                unique(by = beat_no)
            
            df <- window(df, MSNA, values$fs)
            
            incProgress(amount = 0.25)
            # set burst labels to true
            df$burst_lab <- TRUE
            # function to identify noise spikes from 1st derivative
            df <- noise(df, input$noise)
            
            incProgress(amount = 0.25)

            #### determine 2nd derivative ####
            df <- df %>% group_by(beat_no) %>%
                nest(burst_df = c(rel_time:MSNA_v_zeroed)) %>%
                mutate("2ndDerivative" = map(burst_df, function(.x) {
                  ans <- features(.x$rel_time, .x$MSNA_filt, smoother = "smooth.spline")
                  
                  cbind("deriv_time" = attr(ans, "fits")$x,
                        "MSNA_v_zeroed" = .x$MSNA_v_zeroed,
                        "MSNA_filt" = .x$MSNA_filt,
                        "1stDerivative" = attr(ans, "fits")$d1,
                        "2ndDerivative" = attr(ans, "fits")$d2,
                        "updn_1d" = sign(c(0, diff(sign(attr(ans, "fits")$d1)))),
                        "v_crossing_1d" = abs(sign(c(0, diff(sign(attr(ans, "fits")$d1))))) * .x$MSNA_filt,
                        "updn_2d" = sign(c(0, diff(sign(attr(ans, "fits")$d2)))),
                        "v_crossing_2d" = abs(sign(c(0, diff(sign(attr(ans, "fits")$d2))))) * .x$MSNA_filt
                        ) %>%
                    as.data.frame()
                    
                }))
            
            # get start and end time using helper function.
            
            #browser()
            
            df <- df %>%
              mutate("start_end" = map(`2ndDerivative`, start_end, input$BurstThreshold, input$SNR))
            
            incProgress(amount = 0.25)    
            
            # determine burst properties
            df <- df %>% group_by(beat_no) %>% nest() %>%
              mutate(characteristics = map(data, burst_character))
                                           
            df <- df %>% unnest(c(data, characteristics))
            
            # Correct MSNA_vjust to minimum of start or end voltage
            df <- df %>% unnest(burst_df) %>%
                group_by(beat_no) %>% 
                mutate(MSNA_vjust = MSNA_v_zeroed + (-1)*min(c(start_v, end_v))) 
            
        })
        
        df$selected <- FALSE
        
        values$beat_select <- beat_sel
        values$df <- df %>% group_by(beat_no) %>% nest(burst_df = c(rel_time:MSNA_v_zeroed))
        values$x_start <- input$start
        
        
    })

    # ---- Output: Input Data Table ----    
    # beat
    output$beat <- renderDataTable({
    
        req(input$start, input$end)
        

            return(values$ beat %>% 
                    plotly::filter(time >= input$start & time <= input$end) %>% round(2))
        
    }, options = list(pageLength = 10))
    
    # MSNA
    output$MSNA <- renderDataTable({

        req(input$start, input$end)
        
            return(values$MSNA %>%
                    plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end) %>% round(2))
  
    }, options = list(pageLength = 10))
    
    # ---- Output: Create Selected Data Table ----
    output$data <- renderDataTable({
        
        req(input$start, input$end, values$burst_keep)
        
        df <- values$burst_keep %>% select(beat_no, beat_time, RRI, burst_time, latency, amp, area)

            return(df %>%
                       plotly::filter(beat_time >= input$start & beat_time <= input$end) %>% round(2))
        
        
    }, options = list(pageLength = 25))
    
    # burst selection activity
    observe({
        
        #browser()      
      
        req(values$df, values$MSNA, input$start, input$end, input$Analyze)
        
        df <- isolate(values$df) %>% unnest(burst_df)
        MSNA <- isolate(values$MSNA)
        
        # Determine what's a burst
        df$noise <- df$max_deriv > input$noise
        df$amp_thresh <- df$rise_amp > input$BurstThreshold | df$fall_amp > input$BurstThreshold
        df$burst_lab <- (!df$noise & df$amp_thresh)
        
        
        # toggle selected values
        df$burst_lab <- xor(df$burst_lab, df$selected)
        
        # create a window channel to window bursts from start to end time
        df$window <- NA
        df$window[df$rel_time < df$start_t | df$rel_time > df$end_t] <- 0
        df$window[df$rel_time >= df$start_t & df$rel_time <= df$end_t] <- 1
        
        # Pull out windowed neurogram of only those where burst_lab = TRUE
        temp <- df %>% plotly::filter(burst_lab == TRUE, window == 1)
        temp <- temp[!duplicated(temp$MSNA_time),]
        
        # set neurogram$MSNA_vjust to zero
        neurogram <- MSNA %>% mutate(MSNA_vjust = 0)
        # set neurogram when bursts present
        neurogram$MSNA_vjust[which(neurogram$MSNA_time %in% temp$MSNA_time)] <- temp$MSNA_vjust*temp$window 
        neurogram <- neurogram %>% 
          plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end) # the contents of this file contains the raw and detected neurogram which could be saved to file
        
        #######
        temp <- df %>% nest(burst_df = c(rel_time:MSNA_v_zeroed, window, MSNA_vjust)) %>% mutate(amp = max(rise_amp, fall_amp))
        Burst_keep <- temp %>% plotly::filter(burst_lab == TRUE)
        Burst_discard <- temp %>% plotly::filter(burst_lab == FALSE)
        
        #browser()
        
        #### need to determine noise and threshold levels here using a function.
        #temp <- temp %>% select(!'start_end') %>%
        #  mutate("start_end" = map(`2ndDerivative`, start_end, input$BurstThreshold, input$SNR))
        
        #output reactive values
        values$df <- temp
        values$neurogram <- neurogram
        values$burst_keep <- Burst_keep
        values$burst_discard <- Burst_discard
        
    })
    
    
    
    
    # ---- Output: Create MSNA Plot ----
    output$plot <- renderPlotly({
      
        req(input$start, input$end)
        
        # set range slider start and end times
        if (input$range == "30s") {
            range_start <- values$x_start
                #input$start
            range_end <- range_start + 30
        } else if (input$range == "60s") {
            range_start <- values$x_start
                #input$start
            range_end <- range_start + 60
        } else if (input$range == "all") {
            range_start <- input$start
            range_end <- input$end
        }
        
        if (is.null(values$beat_select) | is.null(values$burst_keep)) {
            
            figA <- plot_ly() %>%
              add_lines(data = values$MSNA %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end),
                        x = ~MSNA_time,
                        y = ~BP,
                        name = "BP",
                        line = list(color = "black")
              ) %>%
              layout(yaxis = list(title = "BP (mm Hg)"))
            
            figB <- plot_ly() %>%
              add_lines(data = values$MSNA %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end),
                        x = ~MSNA_time,
                        y = ~ECG,
                        name = "ECG",
                        line = list(color = "black")
              ) %>%
              layout(yaxis = list(title = "ECG (V)"))
          
            figC <- plot_ly() %>%
                add_lines(data = values$beat %>% plotly::filter(time >= input$start & time <= input$end),
                          x = ~time,
                          y = ~RRI,
                          name = "RRI",
                          line = list(color = "black", width =2, shape = "hv")
                ) %>%
                layout(yaxis = list(title = "RRI (s)"))
            
            figD <- plot_ly() %>%
                add_lines(data = values$MSNA %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end),
                          x = ~MSNA_time,
                          y = ~MSNA_v_zeroed,
                          name = "Neurogram",
                          showlegend = FALSE,
                          line = list(color = "black")
                ) %>%
                layout(yaxis = list(title = "MSNA (V)", fixedrange = FALSE),
                       xaxis = list(title = "Time (s)")) %>%
                rangeslider(range_start, range_end)
            
            subplot(figA, figB, figC, figD, nrows = 4, shareX =TRUE, titleY = TRUE, heights = c(0.5/3,0.5/3,0.5/3,0.5))
            
        } else {
            
            df <- values$df
            neurogram <- values$neurogram
            Burst_keep <- values$burst_keep
            Burst_discard <- values$burst_discard
            MSNA <- values$MSNA
            burst_thresh <- isolate(input$BurstThreshold)
            
            beat_time <- df$beat_time %>% unique() # to get beat times

            lines <- map(beat_time, vline) # uses helper function
            h_lines <- hline(burst_thresh)
            
            #browser()
            
            time <- df %>% select(beat_no, burst_lab, burst_time, start_t, end_t) %>% pivot_longer(cols = c(start_t, end_t), names_to = "Time_fact", values_to = "time")
            
            volts <- df %>% select(beat_no, burst_lab, burst_time, start_v, end_v) %>% pivot_longer(cols = c(start_v, end_v), names_to = "v_fact", values_to = "volts") %>% ungroup() %>% select(v_fact, volts)
            
            noise_band <- cbind(time, volts) %>% mutate(noise = volts + burst_thresh/input$SNR, 
                                                   threshold = volts + burst_thresh,
                                                   time = burst_time + time) %>% ungroup()
            
            thresh_df <- noise_band %>% group_by(beat_no) %>%
              plotly::filter(burst_lab == TRUE) %>% 
              select(beat_no, burst_time, threshold) %>% 
              summarise(threshold = min(threshold), burst_time = mean(burst_time)) %>% ungroup()
            
            start_end_markers <- noise_band %>% 
              plotly::filter(burst_lab == TRUE)
             
            # Plot
            # Burst neurogram
            figA <- plot_ly(source = "B") %>%
              add_lines(data = values$MSNA %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end),
                        x = ~MSNA_time,
                        y = ~BP,
                        name = "BP",
                        line = list(color = "black")
              ) %>%
              layout(shapes = lines, yaxis = list(title = "BP (mm Hg)"))
            
            figB <- plot_ly(source = "B") %>%                
              add_lines(data = values$MSNA %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end),
                        x = ~MSNA_time,
                        y = ~ECG,
                        name = "ECG",
                        line = list(color = "black")
              ) %>%
              layout(shapes = lines, yaxis = list(title = "ECG (V)"))
            
            figC <- plot_ly(source = "B") %>%
                add_lines(data = df %>% ungroup(),
                          x = ~beat_time,
                          y = ~RRI,
                          name = "RRI",
                          line = list(color = "black", width =2, shape = "hv")
                ) %>%
                layout(yaxis = list(title = "RRI (s)"))
            
            
            figD <- plot_ly(source = "B") %>%
                add_markers(data = Burst_keep, 
                            x = ~beat_time + latency,
                            y = ~MSNA_max,
                            name = "Bursts Detected",
                            marker = list(size = 10,
                                          color = "red",
                                          line = list(color = "black", width =2)
                            ),
                            text = ~paste("beat no: ", 
                                          round(beat_no, 2), '<br>burst time:', 
                                          round(burst_time, 2), '<br>latency:', 
                                          round(latency, 2), '<br>Amplitude:', 
                                          round(amp, 2), '<br>Area:',
                                          round(area,2)),
                            customdata = ~beat_no
                ) %>%
                add_markers(data = Burst_discard, 
                            x = ~beat_time + latency,
                            y = ~MSNA_max,
                            name = "Burts Discarded",
                            marker = list(size = 10,
                                          color = "grey",
                                          opacity = 0.5,
                                          line = list(color = "black", width =2)
                            ),
                            text = ~paste("beat no: ", 
                                          round(beat_no, 2), '<br>burst time:', 
                                          round(burst_time, 2), '<br>latency:', 
                                          round(latency, 2), '<br>Amplitude:', 
                                          round(amp, 2), '<br>Area:',
                                          round(area,2)),
                            customdata = ~beat_no
                ) %>%
                add_lines(data = MSNA %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end),
                          x = ~MSNA_time,
                          y = ~MSNA_v_zeroed,
                          name = "Neurogram",
                          showlegend = FALSE,
                          line = list(color = "black")
                ) %>%
                add_lines(data = thresh_df %>% 
                            plotly::filter(burst_time >= input$start & burst_time <= input$end),
                        x = ~burst_time,
                        y = ~threshold,
                        name = "Threshold",
                        showlegend = TRUE,
                        line = list(color = "red", dash = 'dot', width = 1)
              ) %>%
                add_ribbons(data = noise_band %>% plotly::filter(time >= input$start & time <= input$end),
                        x = ~time,
                        ymin = ~volts,
                        ymax = ~noise,
                        name = "Noise",
                        showlegend = TRUE,
                        line = list(color = "grey", dash = 'dot', width = 1),
                        fillcolor = 'rgba(7, 164, 181, 0.2)'
              ) %>%   
                add_markers(data = start_end_markers %>% plotly::filter(time >= input$start & time <= input$end), 
                                                x = ~time,
                                                y = ~volts,
                                                name = "start-end markers",
                                                marker = list(symbol = 'x',
                                                              size = 10,
                                                              color = "blue",
                                                              opacity = 0.5),
              ) %>%
                layout(yaxis = list(title = "MSNA (V)", fixedrange = FALSE),
                       xaxis = list(title = "Time (s)"))
            
            # Burst neurogram
            figE <- plot_ly(source = "B") %>%
                add_lines(data = neurogram,
                          x = ~MSNA_time,
                          y = ~MSNA_vjust,
                          name = "Neurogram",
                          showlegend = FALSE,
                          line = list(color = "black")
                ) %>%
                layout(shapes = h_lines, yaxis = list(title = "MSNA Amplitude (v)"),
                       xaxis = list(title = "Time (s)")) %>%
                rangeslider(range_start, range_end)
            
            subplot(figA, figB, figC, figD, figE, 
                    nrows = 5, shareX = TRUE, titleY = TRUE, heights = c(0.5/4, 0.5/4, 0.5/4, 0.5, 0.5/4))
            
        } 
           
    })

    
    # ---- Output: Create Burst Characteristics Plot ----
    output$plot2 <- renderPlotly({
        
        req(input$start, input$end, values$burst_keep)
        
        #browser()  
      
        df <- values$df %>% 
           plotly::filter(burst_time >= input$start & burst_time <= input$end) %>%
            unnest(burst_df)
          
        burst_thresh <- input$BurstThreshold

        df_key <- highlight_key(df, ~beat_no)
        
        lines <- map(input$BurstThreshold, vline)
        latent_lines <- map(mean(df %>% plotly::filter(burst_lab == TRUE) %>% .$latency), latent_line)
        X_line <- map(input$BurstThreshold, hline)
        Y_line <- map(input$BurstThreshold, vline)
        
        base <- plot_ly(df_key,source = "C") %>% group_by(beat_no)
        
        T1 <- base %>%
          group_by(beat_no) %>%
          add_lines(#data = df_key,
                    color = ~burst_lab,
                    colors = c("grey", "red"),
                    x = ~rel_time,
                    #y = ~MSNA_v_zeroed,
                    y = ~MSNA_vjust,
                    line = list(width =2),
                    legendgroup = ~burst_lab,
                    text = ~paste("beat no: ", 
                                  round(beat_no, 2), '<br>burst time:', 
                                  round(burst_time, 2)
                                  # , '<br>latency:', 
                                  # round(latency, 2), '<br>Amplitude:', 
                                  # round(amp, 2), '<br>Area:',
                                  # round(area,2)
                                  ),
                    customdata = ~beat_no,
          ) %>%
          layout(shapes = X_line, yaxis = list(title = "MSNA (V)"), xaxis = list(title = "Time (s)")) 
        
        # latency plot
        
        T2 <- base %>%
          summarise(beat_no = unique(beat_no),
                    amp = mean(amp),
                    latency = mean(latency),
                    burst_lab = unique(burst_lab),
                    burst_time = mean(burst_time),
                    area = mean(area),
                    rise_amp = mean(rise_amp),
                    fall_amp = mean(fall_amp),
                    rise_slope = mean(rise_slope),
                    fall_slope = mean(fall_slope)
          ) %>%
          add_markers(#data = df_key,
                      color = ~burst_lab,
                      colors = c("grey", "red"),
                      x = ~amp,
                      y = ~latency,
                      customdata = ~beat_no,
                      legendgroup = ~burst_lab,
                      text = ~paste("beat no: ", 
                                    round(beat_no, 2), '<br>burst time:', 
                                    round(burst_time, 2), '<br>latency:', 
                                    round(latency, 2), '<br>Amplitude:', 
                                    round(amp, 2), '<br>Area:',
                                    round(area,2)
                                    ),
                      marker = list(size = 10,
                                    line = list(color = "black", width =2)),
                      showlegend = F
                      ) %>%
          
          layout(shapes = c(lines, latent_lines), 
            yaxis = list(title = "latency (s)"), xaxis = list(title = "Burst Amplitude (V)"))
        
        # rise vs fall amplitude

        T3 <- base %>%
          summarise(amp = mean(amp),
                    latency = mean(latency),
                    burst_lab = unique(burst_lab),
                    burst_time = mean(burst_time),
                    area = mean(area),
                    rise_amp = mean(rise_amp),
                    fall_amp = mean(fall_amp),
                    rise_slope = mean(rise_slope),
                    fall_slope = mean(fall_slope)
          ) %>%
          add_markers(#data = df_key,
                      color = ~burst_lab,
                      colors = c("grey", "red"),
                      x = ~rise_amp,
                      y = ~fall_amp,
                      customdata = ~beat_no,
                      legendgroup = ~burst_lab,
                      text = ~paste("beat no: ", 
                                    round(beat_no, 2), '<br>burst time:', 
                                    round(burst_time, 2), '<br>latency:', 
                                    round(latency, 2), '<br>Amplitude:', 
                                    round(amp, 2), '<br>Area:',
                                    round(area,2)),
                      marker = list(size = 10, line = list(color = "black", width =2)),
                      showlegend = F
          ) %>%
          layout(shapes = c(X_line, Y_line), yaxis = list(title = "Fall Amplitude (V)"), xaxis = list(title = "Rise Amplitude (V)"))
        
        # rise vs fall slope
        T4 <- base %>%
          summarise(amp = mean(amp),
                    latency = mean(latency),
                    burst_lab = unique(burst_lab),
                    burst_time = mean(burst_time),
                    area = mean(area),
                    rise_amp = mean(rise_amp),
                    fall_amp = mean(fall_amp),
                    rise_slope = mean(rise_slope),
                    fall_slope = mean(fall_slope)
          ) %>% 
          add_markers(#data = df_key,
                      color = ~burst_lab,
                      colors = c("grey", "red"),
                      x = ~rise_slope,
                      y = ~fall_slope,
                      customdata = ~beat_no,
                      legendgroup = ~burst_lab,
                      text = ~paste("beat no: ", 
                                    round(beat_no, 2), '<br>burst time:', 
                                    round(burst_time, 2), '<br>latency:', 
                                    round(latency, 2), '<br>Amplitude:', 
                                    round(amp, 2), '<br>Area:',
                                    round(area,2)),
                      marker = list(size = 10, line = list(color = "black", width =2)),
                      showlegend = F
          ) %>%
          layout(yaxis = list(title = "Fall Slope (V/s)"), xaxis = list(title = "Rise Slope (V/s)")) 
        
        
        subplot(T1, T2, T3, T4,
          nrows = 2, titleY = TRUE, titleX = TRUE, margin = c(0.08, 0.08, 0.1, 0.1)) %>%
          highlight(on = "plotly_click", off = "plotly_relayout", opacityDim = 0.1, debounce = 250)
          
    })
    

    # ---- Observe: plot click ----
    observe({
        
        eventData <- event_data("plotly_click", source = c("B"))
        
        if ("customdata" %in% names(eventData)) {
            df <- isolate(values$df)
            beat_no <- eventData$customdata
            
            df$selected[df$beat_no == beat_no] <- !df$selected[df$beat_no == beat_no]
            
            values$df <- df
            
            }
      })
    
    # ---- Observe: plot click ----
    observeEvent(input$toggle, {
      
      eventData <- event_data("plotly_click", source = "C")
      
      if ("customdata" %in% names(eventData)) {
        df <- isolate(values$df)
        beat_no <- eventData$customdata
        
        df$selected[df$beat_no == beat_no] <- !df$selected[df$beat_no == beat_no]
        
        values$df <- df
        
      }
      
    })
    

    # ---- Observe: slider layout ----
    observe({
        eventData <- event_data("plotly_relayout", source = "B")
        values$x_start <- eventData$xaxis.range[1]
    }, priority = -1)

    # ---- Observe: first ----
    observeEvent(input$first, {
        values$x_start <- input$start
    })
    
    # ---- Observe: next ----
    observeEvent(input$`next`, {
        
        if (input$range == "30s") {
            values$x_start <- values$x_start + 30
        }
        
        if (input$range == "60s") {
            values$x_start <- values$x_start + 60
        }
    })
    
    # ---- Observe: previous ----
    observeEvent(input$previous, {

        if (input$range == "30s") {
            values$x_start <- values$x_start - 30
        }
        
        if (input$range == "60s") {
            values$x_start <- values$x_start - 60
        }
        
    })

    # ---- Observe: reset ----
    observeEvent(input$reset, {
        df <- values$df
        df$selected <- FALSE
        values$df <- df
    })
    
    
    # ---- Downloadable csv of selected dataset ----
    output$save <- downloadHandler(

        filename = function() {
            paste(tools::file_path_sans_ext(input$f_MSNA$name), "-burst.zip", sep = "")
        },
        content = function(file){


          #go to a temp dir to avoid permission issues
          owd <- setwd(tempdir())
          on.exit(setwd(owd))

            fileName <- c(paste(tools::file_path_sans_ext(input$f_MSNA$name), "-burst.csv", sep = ""),
                          paste(tools::file_path_sans_ext(input$f_MSNA$name), "-neurogram.csv", sep = ""))
            write_csv(values$burst_keep %>% select(c(beat_no, beat_time, RRI, burst_time, latency, amp, area)) %>% plotly::filter(beat_time >= input$start & beat_time <= input$end)
                      , fileName[[1]])
            write_csv(values$neurogram %>% plotly::filter(MSNA_time >= input$start & MSNA_time <= input$end), fileName[[2]])


          #create the zip file
          zip(file,fileName)

        }
    )
    
  
}

# Create Shiny app ----
thematic::thematic_shiny()
shinyApp(ui, server)
