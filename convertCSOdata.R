# Convert census stats from summations to percentages
#
# Defines a function that takes raw CSO figures and returns stats (for some headings.)
#
# Shane Lynn 2014-01-12

convertCSOdata <- function(data, idcol="GEOGDESC"){
  results <- data.frame(id = data[[idcol]])
  
  # extract weighted average age.
  age_data <- data[,74:108]
  mean_ages <- c(0:19, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87)
  results$avr_age <- apply(age_data, MARGIN=1, function(x){
    #want this function to calculate the average weighted age
    # weighted mean age = E(num_people in age bin * average age of bin) / total_people
    age <- sum(x[1:length(mean_ages)] * mean_ages) / x[length(x)]
  })
  
  # average household size
  household_data <- data[, 331:339]
  mean_household_size <- c(1:8)
  results$avr_household_size <- apply(household_data, 1, function(x){
    size <- sum(x[1:length(mean_household_size)] * mean_household_size) / x[length(x)]
  })
  
  # Average education level
  education_data <- data[,622:634]
  mean_education_level <- c(0:10) # the 11th column of data is "not stated" answers - to be removed
  results$avr_education_level <- apply(education_data, 1, function(x){
    # need to remove the not-stated answers from the total - thus measuring only average on only people who answered questions
    education <- sum(x[1:length(mean_education_level)] * mean_education_level) / (x[length(x)] - x[length(x)-1]) 
  })
  
    
  # Average number of cars per household
  car_data <- data[754:758]
  num_cars <- c(0:4) # the number of cars per household for each column of temp data
  results$avr_num_cars <- apply(car_data, 1, function(x){
    # need to remove the not-stated answers from the total - thus measuring only average on only people who answered questions  
    cars <- sum(x * num_cars) / (sum(x))
  })
  
  # Average health reported
  health_data <- data[,696:702]
  mean_health <- c(5:1) # the 6th column of data is "not stated" answers - to be removed
  results$avr_health <- apply(health_data, 1, function(x){
    # need to remove the not-stated answers from the total - thus measuring only average on only people who answered questions
    health <- sum(x[1:length(mean_health)] * mean_health) / (x[length(x)] - x[length(x)-1]) 
  })
  
  
  # Percentage rented accomodation = Rented from private landlord / (total - not_stated)
  rent_data <- data[, c("T6_3_RPLH", "T6_3_NSH", "T6_3_TH")]
  results$rented_percent <- (rent_data[, 1] / (rent_data[, 3] - rent_data[,2])) * 100
  
  # Average unemployment = (looking_for_first_job + lost_job) / (total - disability)
  employment_data <- data[, c("T8_1_LFFJT", "T8_1_ULGUPJT", "T8_1_UTWSDT", "T8_1_TT")]
  results$unemployment_percent <- ((rowSums(employment_data[, 1:2])) / (employment_data[, 4] - employment_data[,3])) * 100
  
  # Average internet penetration = (broadband + other) / (total - not_stated)
  internet_data <- data[, c("T15_3_B", "T15_3_OTH", "T15_3_NS", "T15_3_T")]
  results$internet_percent <- ((rowSums(internet_data[, 1:2])) / (internet_data[, 4] - internet_data[,3])) * 100
  
  # Single Percent
  # Married Percent
  # Divorced Percent
  # Widowed Percent
  marital_data <- data[, c("T1_2SGLT", "T1_2MART", "T1_2SEPT", "T1_2DIVT", "T1_2WIDT", "T1_2T")]
  marital_percents <- data.frame(t(apply(marital_data, 1, function(x) {x[1:5]/x[6]})) * 100)
  names(marital_percents) <- c("single_percent", "married_percent", "separated_percent", "divorced_percent", "widow_percent")  
  results <- cbind(results, marital_percents)
  
  return(results)
}
