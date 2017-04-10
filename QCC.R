#-----------------------------------------------------------------------------------------#
# Simon Shook, Darren Taylor and Kyle Saltmarsh
#
# QCC Rule functions
#
#-----------------------------------------------------------------------------------------#



#-----------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------#
# Main

# Inputs are default values if not specified
Apply_selected_QCC_Rules <- function(Input_Data, 
                                     QCC_LIST = seq(9), 
                                     QCC1_n_sigma  =  3,
                                     QCC2_Window_Size = 9,
                                     QCC3_Window_Size = 6,
                                     QCC4_Window_Size = 15,
                                     QCC5_n_sigma = 2, 
                                     QCC5_Window_Size = 3,
                                     QCC6_n_sigma = 1, 
                                     QCC6_Window_Size = 5,
                                     QCC7_n_sigma = 1, 
                                     QCC7_Window_Size = 15,
                                     QCC8_n_sigma = 1, 
                                     QCC8_Window_Size = 8,
                                     QCC9_window_size = 15,...) {
  # Function to apply selected QCC rules
  # Get the input tag name
  target = Input_Data[,colnames(.SD),.SDcols=-"TAGDATE"]
  
  # Function to apply selected QCC rules
  Output_Data <- data.table(TAGDATE = Input_Data$TAGDATE)
  function_list <- c(QCC1,QCC2,QCC3,QCC4,QCC5,QCC6,QCC7,QCC8,QCC9)
  
  for (i in QCC_LIST) {
    
    Output_Data[,paste0('Output_',as.integer(i)) := as.numeric(function_list[[as.integer(i)]](Input_Data[[2]]))]
    
  }
  
  # Return the timeseries of results but no the input
  return(Output_Data)
  
}

# Create our QCC model
Model_Payload <- list( # Package functions and objects as a named list 
  Model_Object_Function = Apply_selected_QCC_Rules, # Mandatory entry point function 
  Model_Family_Functions = c("QCC_Rule1.Rds", # Stand alone models
                             "QCC_Rule2.Rds",
                             "QCC_Rule3.Rds",
                             "QCC_Rule4.Rds",
                             "QCC_Rule5.Rds",
                             "QCC_Rule6.Rds",
                             "QCC_Rule7.Rds",
                             "QCC_Rule8.Rds",
                             "QCC_Rule9.Rds")#,
  #Model_Object = list() # Enter a training object here if relevant to model
)

saveRDS(Model_Payload,paste0("QCC.Rds"))# Save as .Rds 

#-----------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------#
# Rule functions
#
#
#-----------------------------------------------------------------------------------------#



#-----------------------------------------------------------------------------------------#
# QCC Rule 1
# One point is more than 3 standard deviations from the mean


QCC1 <- function(Input_Data, QCC1_n_sigma = 3) {
  
  upper_threshold <- mean(Input_Data) + QCC1_n_sigma*sd(Input_Data)
  lower_threshold <- mean(Input_Data) - QCC1_n_sigma*sd(Input_Data)
  
  
  (Input_Data < lower_threshold | Input_Data >  upper_threshold)
  
}

Rule1 = list(QCC1 = QCC1)
saveRDS(Rule1,"QCC_Rule1.Rds")

#-----------------------------------------------------------------------------------------#
# Rule 2
# Nine (or more) points in a row are on the same side of the mean


QCC2 <- function(Input_Data, QCC2_Window_Size = 9) {
  
  Input_Data_Mean <- mean(Input_Data)
  
  is_on_same_side_of_mean <- function(x) {
    all(x > Input_Data_Mean) | all(x < Input_Data_Mean)
  }
  
  c(rep.int(F, QCC2_Window_Size - 1), # Left padding
    rollapply(Input_Data, 
              width= QCC2_Window_Size, 
              FUN = is_on_same_side_of_mean, 
              align = "left"))
}

Rule2 = list(QCC2 = QCC2)
saveRDS(Rule2,"QCC_Rule2.Rds")

#-----------------------------------------------------------------------------------------#
# Rule 3
# Six (or more) points in a row are continually increasing (or decreasing)


QCC3 <- function(Input_Data, QCC3_Window_Size = 6) {
  
  is_strictly_monotonic <- function(x) {
    (all(x == cummax(x)) | all(x == cummin(x))) & !any(duplicated(x))
  }
  
  c(rep.int(F, QCC3_Window_Size - 1), # Left padding
    rollapply(Input_Data,
              width = QCC3_Window_Size,
              FUN = is_strictly_monotonic,
              align = "left"))
}

Rule3 = list(QCC3 = QCC3)
saveRDS(Rule3,"QCC_Rule3.Rds")

#-----------------------------------------------------------------------------------------#
# Rule 4
# Fourteen (or more) points in a row alternate in direction, increasing then decreasing


QCC4 <- function(Input_Data, QCC4_Window_Size = 15) {
  
  # alternating up/down over 15 observations
  # The smallest observable sawtooth (3 wide).
  is_single_sawtooth <- function(x) {
    if (length(x) !=3)
      stop('Error: Single sawtooth only handles vectors of length 3')
    
    # Is the middle value is bigger/smaller than both the other values? If yes, then it is a single sawtooth
    (x[2] > x[1] && x[2] > x[3]) || (x[2] < x[1] && x[2] < x[3])
    
  }
  
  # Determine which points correspond to a sawtooth of length three
  single_sawtooth <- c(rep.int(F, 3 - 1), # Left padding
                       rollapply(Input_Data,
                                 width = 3,
                                 FUN = is_single_sawtooth,
                                 align = "left"))
  
  c(rep.int(F, QCC4_Window_Size - 1), # Left padding
    rollapply(single_sawtooth,
              width = QCC4_Window_Size,
              FUN = function(x) {all(x[3:QCC4_Window_Size])}, # Ignore the first two values, as they may be false. The third value is what determines whether the first two were part of a sawtooth pattern
              align = "left"))
}

Rule4 = list(QCC4 = QCC4) # Package function as a named list 
saveRDS(Rule4,"QCC_Rule4.Rds") # Save as .Rds 

#-----------------------------------------------------------------------------------------#
# Rule 5
# Two (or three) out of three points in a row are more than 2 standard deviations from 
# the mean in the same direction


QCC5 <- function(Input_Data, QCC5_n_sigma = 2, QCC5_Window_Size = 3) {
  
  Input_Data_Mean <- mean(Input_Data)
  Input_Data_SD <- sd(Input_Data)
  
  count_outside_two_sigma_exceeds_threshold <- function(x) {
    count_outside_two_sigma <- sum((x > Input_Data_Mean + QCC5_n_sigma*Input_Data_SD) | (x < Input_Data_Mean - QCC5_n_sigma*Input_Data_SD))
    count_outside_two_sigma >= QCC5_Window_Size - 1
  }
  
  c(rep.int(F,QCC5_Window_Size - 1), # Left padding
    rollapply(Input_Data,
              width= QCC5_Window_Size,
              FUN = count_outside_two_sigma_exceeds_threshold,
              align = "left"))
}

Rule5 = list(QCC5 = QCC5)
saveRDS(Rule5,"QCC_Rule5.Rds")

#-----------------------------------------------------------------------------------------#
# Rule 6
# Four (or five) out of five points in a row are more than 1 standard deviation from the 
# mean in the same direction


QCC6 <- function(Input_Data, QCC6_n_sigma = 1, QCC6_Window_Size = 5) {
  
  Input_Data_Mean <- mean(Input_Data)
  Input_Data_SD <- sd(Input_Data)
  
  count_outside_1_sigma_exceeds_threshold <- function(x) {
    count_outside_1_sigma <- sum((x > Input_Data_Mean + QCC6_n_sigma*Input_Data_SD) | (x < Input_Data_Mean - QCC6_n_sigma*Input_Data_SD))
    count_outside_1_sigma >= QCC6_Window_Size - 1
  }
  
  c(rep.int(F, QCC6_Window_Size - 1), # Left padding
    rollapply(Input_Data,
              width= QCC6_Window_Size,
              FUN = count_outside_1_sigma_exceeds_threshold,
              align = "left"))
}

Rule6 = list(QCC6 = QCC6)
saveRDS(Rule6,"QCC_Rule6.Rds")

#-----------------------------------------------------------------------------------------#
# Rule 7
# Fifteen points in a row are all within 1 standard deviation of the mean on either side 
# of the mean


QCC7 <- function(Input_Data, QCC7_n_sigma = 1, QCC7_Window_Size = 15) {
  
  Input_Data_Mean <- mean(Input_Data)  
  Input_Data_SD <- sd(Input_Data)
  
  # If the previous window_size observations lie within 1 sigma, then true. Else false.
  is_inside_1_sigma <- function(x) {
    all((x < Input_Data_Mean + QCC7_n_sigma*Input_Data_SD) & (x > Input_Data_Mean - QCC7_n_sigma*Input_Data_SD))
  }
  
  c(rep.int(F, QCC7_Window_Size - 1), # Left padding
    rollapply(Input_Data,
              width = QCC7_Window_Size,
              FUN = is_inside_1_sigma,
              align = "left"))
}

Rule7 = list(QCC7 = QCC7)
saveRDS(Rule7,"QCC_Rule7.Rds")

#-----------------------------------------------------------------------------------------#
# Rule 8
# Eight points in a row exist, but none within 1 standard deviation of the mean, and the 
# points are in both directions from the mean


QCC8 <- function(Input_Data, QCC8_n_sigma = 1, QCC8_Window_Size = 8) {
  
  Input_Data_Mean <- mean(Input_Data)  
  Input_Data_SD <- sd(Input_Data)
  
  # If the previous window_size observations lie within 1 sigma, then true. Else false.
  is_inside_1_sigma <- function(x) {
    !all((x < Input_Data_Mean + QCC8_n_sigma*Input_Data_SD) & (x > Input_Data_Mean - QCC8_n_sigma*Input_Data_SD))
  }
  
  c(rep.int(F, QCC8_Window_Size - 1), # Left padding
    rollapply(Input_Data,
              width = QCC8_Window_Size,
              FUN = is_inside_1_sigma,
              align = "left"))
}

Rule8 = list(QCC8 = QCC8)
saveRDS(Rule8,"QCC_Rule8.Rds")

#-----------------------------------------------------------------------------------------#
# Flatline Rule
# Check if sequence is repeating itself


QCC9 <- function(Input_Data, QCC9_window_size = 15) {
  
  isFlatline <- function(x){
    all(diff(x, differences = 2)==0)
  }
  
  c(rep.int(F, QCC9_window_size-1), # Left padding
    rollapply(Input_Data,
              width = QCC9_window_size,
              FUN = isFlatline,
              align = "left"))
  
}

Rule9 = list(QCC9 = QCC9)
saveRDS(Rule9,"QCC_Rule9.Rds")

#-----------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------#
# End of code
#
#
#-----------------------------------------------------------------------------------------#