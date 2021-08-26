
#### Prep MSNA file ####
load_MSNA <- function(MSNA, fs) {
  MSNA <- MSNA
  
    # Create filtered MSNA signal 0.05 - 4 Hz passband.
  hf <- 4 #default for human applications is 4 Hz. 
  lf <- 0.05 #default for human applications is 0.05 Hz.
  
  nyq_l <- fs/2 # nyquist limit
  bf <- signal::butter(1, c(lf/nyq_l, hf/nyq_l), type = "pass") # butterworth filter 1 order passband
  x <- MSNA$MSNA_v
  MSNA$MSNA_filt <- signal::filtfilt(bf, x) # filtered signal
  
  # correct proportions changed by filter
  lm_prop <- lm(MSNA$MSNA_v~MSNA$MSNA_filt)
  int <- lm_prop$coefficients[[1]]
  slope <- lm_prop$coefficients[[2]]
  MSNA$MSNA_filt <- slope*MSNA$MSNA_filt #+ int
  
  # Create filter for MSNA signal to remove DC offset
  bf <- signal::butter(1, lf/nyq_l, type = "high")
  x <- MSNA$MSNA_v
  MSNA$MSNA_v_zeroed <- signal::filtfilt(bf, x)
  lm_prop <- lm(MSNA$MSNA_v~MSNA$MSNA_v_zeroed)
  int <- lm_prop$coefficients[[1]]
  slope <- lm_prop$coefficients[[2]]
  MSNA$MSNA_v_zeroed <- slope*MSNA$MSNA_v_zeroed
  
  return(MSNA)
}

#### Find local maxima ####
local_maxima <- function(beat_df, MSNA_df, latency, t_window) {
  
  #troubleshoot
  #beat_df <- beat_sel
  #MSNA_df <- MSNA
  #latency <- input$latency
  #t_window <- input$t_window
  
  df <- NULL
  
  for (i in seq_along(beat_df$time)) {
   #i<-645
    temp2 <- MSNA_df %>% 
      plotly::filter(MSNA_time >= beat_df$time[[i]] + (latency - t_window) & MSNA_time <= beat_df$time[[i]] + (latency + t_window))
    
    if (nrow(temp2) == 0) {
      temp <- cbind(beat_no = i, 
                    beat_time = beat_df$time[[i]], 
                    RRI = beat_df$RRI[[i]], 
                    MSNA_time = NA, 
                    MSNA_v = NA, 
                    MSNA_filt = NA, 
                    MSNA_v_zeroed = NA, 
                    max_pos = NA, 
                    t_max = NA, 
                    latency = NA)
    } else {
      temp <- cbind(beat_no = i, 
                    beat_time = beat_df$time[[i]], 
                    RRI = beat_df$RRI[[i]], 
                    temp2)
      
      temp$max_pos <- which.max(temp2$MSNA_filt)
      temp$t_max <- temp$MSNA_time[temp$max_pos[[1]]]
      temp$latency <- temp$t_max - temp$beat_time
      
    }
    
    df <- temp %>% rbind(df, .)
    
  }
  return(df)
}

#### window MSNA data around local maxima ####
window <- function(df, MSNA, fs) {
  df2 <- NULL
  
  for (i in seq_along(df$beat_no)) {
    temp2 <- MSNA %>% 
      plotly::filter(MSNA_time >= df$t_max[[i]] - df$RRI[[i]]/2 & MSNA_time <= df$t_max[[i]] + df$RRI[[i]]/2)
    
    if (nrow(temp2) == 0) {
      temp <- cbind(
        beat_no = i, 
        beat_time = df$beat_time[[i]], 
        RRI = df$RRI[[i]],
        burst_time = NA,
        rel_time = NA,
        MSNA_time = NA, 
        MSNA_v = NA,
        MSNA_v_zeroed = NA,
        MSNA_filt = NA, 
        latency = NA,
        MSNA_max = NA)
    } else {
      temp <- cbind(
        beat_no = i, 
        beat_time = df$beat_time[[i]],
        RRI = df$RRI[[i]],
        burst_time = df$t_max[[i]],
        rel_time = seq(from = -(df$RRI[[i]]/2) + 1/fs, by = 1/fs, length.out = nrow(temp2)),
        temp2, 
        latency = df$latency[[i]],
        MSNA_max = MSNA$MSNA_v_zeroed[MSNA$MSNA_time == df$t_max[[i]]])
    }
    
    df2 <- temp %>% rbind(df2, .)
    
  }
  return(df2)
}

#### Noise spike detection ####
noise <- function(df, noise_thresh) {
  
  temp <- df
  
  noise <- temp %>% 
    group_by(beat_no) %>% 
    summarise(deriv = diff(MSNA_v_zeroed)/diff(MSNA_time)) %>%
    summarise(max_deriv = max(deriv)) %>%
    mutate(noise = max_deriv > noise_thresh)
  
  temp <- left_join(temp, noise, by = "beat_no")
  
  return(temp)
}

#### Identify burst Start and End ####
# Uses the 1st and 2nd derivatives to identify the start and end of a burst
# Burst must exceed noise threshold (based on SNR) in order for processing on window to occur.
# if does not exceed noise threshold the start and end is set to the start and end of the window.
# Noise threshold = input$BurstThreshold/input$SNR + min(df$`2ndDerivative`[[i]]$MSNA_filt)
# If permitted to exceed - finds first positive 1st derivative

start_end <- function(x, BurstThreshold, SNR) {
  #troubleshoot
  #----
    #x <- df$`2ndDerivative`[[1]]
  #---
  
  # Determine start details from deriv_times <0
    start_df <- x %>% plotly::filter(deriv_time <= 0)
      
    if (1 %in% start_df$updn_1d) {
    
      start <- start_df %>% plotly::filter(updn_1d > 0) %>% .[nrow(.),] %>% cbind("POI" = "start", .)
        
      start_t <- start
      
    } else {
      start <- start_df[1,] %>% cbind("POI" = "start", .)

      start_t <- start     
    }
    
    # Determine end details from deriv_times >0
  end_df <- x %>% plotly::filter(deriv_time >= 0)
  
    if (1 %in% end_df$updn_1d) {
      
      end <- end_df %>% plotly::filter(updn_1d > 0) %>% .[1,] %>% cbind("POI" = "end", .)
      
      end_t <-  end
    } else {
      end <- end_df[nrow(end_df),] %>% cbind("POI" = "end", .)
      
      end_t <- end
    }
    
  temp <- rbind(start_t, end_t)    

  } 


#### Determine burst characteristics ####

burst_character <- function(x) {
  
  #x <- test$data[[7]] # for troubleshooting
  
  burst_df <- x$burst_df[[1]]
  MSNA_max <- x$MSNA_max 
  start_end <- x$start_end[[1]]
  
  start_t <- start_end %>% plotly::filter(POI == "start") %>% select(deriv_time) %>% .[[1]]
  end_t <- start_end %>% plotly::filter(POI == "end") %>% select(deriv_time) %>% .[[1]]
  
  start_v <- start_end %>% plotly::filter(POI == "start") %>% select(MSNA_v_zeroed) %>% .[[1]]
  rise_amp <- MSNA_max - start_v
  rise_slope <- rise_amp/abs(start_t)
  
  end_v <- start_end %>% plotly::filter(POI == "end") %>% select(MSNA_v_zeroed) %>% .[[1]]
  fall_amp <- MSNA_max - end_v
  fall_slope <- fall_amp/abs(end_t)
  
  # determine burst area
  temp <- burst_df %>% plotly::filter(rel_time >= start_t & rel_time <= end_t) %>% mutate(MSNA_v_zeroed = MSNA_v_zeroed - min(start_v, end_v))
  area <- auc(x = temp$MSNA_time, y = temp$MSNA_v_zeroed - min(temp$MSNA_v_zeroed))
  
  characteristics <- cbind(start_t, start_v, 
                           rise_amp, 
                           rise_slope, 
                           end_t, end_v, 
                           fall_amp, 
                           fall_slope, area) %>% as.data.frame()
  
  characteristics  
}






# Function to plot vertical lines for each heart beat
vline <- function(x) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = "red", dash = 'dot', width = 1)
  )
}

hline <- function(y) {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = "red", dash = 'dot', width = 1)
  )
}


latent_line <- function(y) {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = "red", dash = 'dash', width = 1)
  )
}


# Function to detect zero crossings.
zero_crossings <- function(x) {
  
  # Estimate gradient
  zerocrossings <- uniroot.all(stats::approxfun(1:length(x), x), interval = range(1:length(x)))
  if (length(zerocrossings) == 0) {
    return(NA)
  }
  zerocrossings
}
