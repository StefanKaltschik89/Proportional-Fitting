#Fill in the name of your dataset instead of "survey" and adapt the variable names (e.g. "gender") to your needs!
#---check weights from raw-data
wpct(survey$gender)
wpct(survey$migration)


#---setting target weights from population data
#Make sure that the factor levels and names correspond to your dataframe
target <- list(
  gender = setNames(c(0.50, 0.50), c("female", "male")),
  migration = setNames(c(0.3, 0.7), c("yes", "no"))
)

#--- change data type to dataframe to prevent error message
survey_cleaned<-as.data.frame(survey_cleaned) 

-------------------------------------------------------------------------------
#---raking process
raking<-anesrake(target,
                 survey_cleaned,
                 survey_cleaned$participant_id,                    
                 cap = 5,                      # Maximum allowed weight per iteration
                choosemethod = "total",       # How are parameters compared for selection?
                type = "pctlim",              # What selection criterion is used?
                 pctlim = 0.05                 )

#---raking results
raking_summary <- summary(raking)
raking_summary

#---add column with weights to dataset
survey_cleaned$weight <- raking$weight 

#---show summary of weights 
result<-survey_cleaned %>% select(gender, migration, weight) %>% unique() %>% arrange(weight)
sum(result$weight)

#---standardize weights
survey_cleaned$weight<-survey_cleaned$weight/sum(survey_cleaned$weight)
--------------------------------------------------------------------------------------------------------------
#---draw random sample from dataset using weights
sample_idx <- sample(1:nrow(survey_cleaned), size = 400, prob = survey_cleaned$weight)
sample_df <- survey_cleaned[sample_idx, ]
