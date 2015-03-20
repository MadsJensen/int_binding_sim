library(dplyr)


gen_data <- function(n_subs, n_trials, mean, sd,
                     condition = "cond",
                     modality = "modal"){
    for (i in 1:n_subs){
    # generate data
    sub_data <- rnorm(n = n_trials, mean = mean, sd = sd)
    sub_data <- tbl_df(as.data.frame(sub_data))
    
    # setup dataframe
    sub_data <- tbl_df(as.data.frame(sub_data))
    sub_data$id <- i 
    
    if (i == 1){sim_data <- sub_data}
    else sim_data <- bind_rows(sim_data, sub_data)
        
    }
    sim_data$condition <- condition
    sim_data$modality <- modality
    sim_data <- rename(sim_data, value = sub_data)
    return(sim_data)
}



baseline_action <- gen_data(20, 40, 6, 66, "baseline", "action")
baselie_tone <- gen_data(20, 40, 15, 72, "baselise", "tone")
opr_action <- gen_data(20, 40, 21, 57, "oprant", "action")
opr_tone <- gen_data(20, 40, -31, 71, "oprant", "tone")


data_single <- bind_rows(baseline_action, baselie_tone, opr_action, opr_tone)

data_grp <- data_single %>%
    group_by(condition, modality, id ) %>%
    summarise(value = mean(value), sd = sd(value))
