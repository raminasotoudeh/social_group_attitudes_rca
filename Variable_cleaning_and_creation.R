### Cleaning and Creating Variables

# Read in data
anes2016 = read.dta13("anes_timeseries_2016_Stata13.dta")

# Load in RCA class memberships
ids_to_classes = readRDS("RCA_ids.RDS")

# Set name of ID column to "ID"
colnames(anes2016)[2] = "ID"

# Merge by ID
anes_w_classes_all = merge(ids_to_classes, anes2016, by = "ID")

# Choose covariates
covars = c("V162171", "V161158x",
           "V161270", "V161310x", "V161511",
           "V161342", "V161267", 
           "V161241", "V161361x",
           "V162309x", "V161244",
           "V161245", "V161265x",
           "V161264x", "ID", "RCA",
           "V162095", "V162096",
           "V162097",
           "V162099", 
           "V162101",  
           "V162103", 
           "V162105", "V162106",
           "V162107", "V162108",
           "V162111", "V162112",
            "V162310",
           "V162311", "V162312",
           "V162313", "V162314",
           "V161219", "V162194",
           "V163003","V162256",
           "V162367", "V161009", ## laurison::
           "V161126", "V161178","V161217","V161205",
           "V161206","V161207","V161208","V161209",
           "V161210" ,"V161211","V161212","V162102","V162104",
           "V161219", "V162207", "V162209", "V162210", ## race::
           "V162238x", "V161204x", "V162317",
           "V162322", "V162323", "V162324", "V162325",
           "V162345", "V162346","V162347","V162348","V162349","V162350","V162351","V162352",
           "V162266", "V161322a",
           "V162357", "V162358", "V162359", "V162360")

# Gather covariate names
covars_labels = c("political_identity", "party_id",
                  "education", "race",  "sexual_orientation",
                  "gender", "age", 
                  "relig_important", "income_pre",
                  "income_post", "ever_attend",
                  "church_freq", "religion",
                  "detailed_denom", "ID", "RCA",
                  "attitude_ChristianFundamentalists", "attitude_Feminists",
                  "attitude_Liberals", 
                  "attitude_PoorPeople", 
                  "attitude_Conservatives", 
                  "attitude_GayLesbians", 
                  "attitude_RichPeople", "attitude_Muslims",
                  "attitude_Christians", "attitude_Jews",
                  "attitude_Transgender", "attitude_Scientists",
                  "attitude_AsianAmericans",
                  "attitude_Hispanics", "attitude_Blacks",
                  "attitude_IllegalImmigrants", "attitude_Whites",
                  "social_trust","num_org",
                  "region", "political_interest",
                  "personal_discrim",  "attention_to_news",
                  "pol_lib_cons_scale", "pol_gov_spending", "pol_gov_waste_tax", "pol_fed_spend_soc_sec", "pol_fed_spend_pub_schl",
                  "pol_fed_spend_sci_tech", "pol_fed_spend_crime", "pol_fed_spend_welfare", "pol_fed_spend_childcare",  
                  "pol_fed_spend_poor", "pol_fed_spend_env",  "pol_therm_supreme_court","pol_therm_congress", 
                  "non_pol_ppl_trusted", "non_pol_world_adjust", "non_pol_tolerant_moral", "non_pol_traditional_fam",
                  "preferential_hiring_blacks", "affirmative_action", "whites_jobs_to_minorities",
                  "whites_influence_pol", "blacks_influence_pol", "hispanics_influence_pol", "asians_influence_pol",
                  "whites_hardworking", "blacks_hardworking", "hispanics_hardworking", "asians_hardworking", "whites_violent", "blacks_violent", "hispanics_violent", "asians_violent",
                  "minorities_should_adapt", "non_us_native",
                  "discrimination_US_blacks", "discrimination_US_hispanics", "discrimination_US_asians", "discrimination_US_whites")


# Check if all covariates chosen are in data
covars[which(!covars %in% colnames(anes_w_classes_all))]

# Split sample to include chosen covariates
anes_w_classes = anes_w_classes_all[,covars]

# Replace names
colnames(anes_w_classes) = covars_labels

# Recode and clean covariates
anes_w_classes$education = ifelse(anes_w_classes$education == -9 | anes_w_classes$education == 95 | anes_w_classes$education == 90, NA, anes_w_classes$education)
anes_w_classes$gender = ifelse(anes_w_classes$gender == -9 | anes_w_classes$gender > 2, NA, anes_w_classes$gender)
anes_w_classes$female = anes_w_classes$gender - 1
anes_w_classes$age = ifelse(anes_w_classes$age == -9 | anes_w_classes$age == -8, NA, anes_w_classes$age)
anes_w_classes$party_id = ifelse(anes_w_classes$party_id < 0 , NA, anes_w_classes$party_id)
anes_w_classes$sexual_orientation =  ifelse(anes_w_classes$sexual_orientation < 0 , NA, anes_w_classes$sexual_orientation)
anes_w_classes$race = ifelse(anes_w_classes$race < 0, NA, anes_w_classes$race)
anes_w_classes$asian = ifelse(anes_w_classes$race == 3, 1, 0)
anes_w_classes$black = ifelse(anes_w_classes$race == 2, 1, 0)
anes_w_classes$hispanic = ifelse(anes_w_classes$race == 5, 1, 0)
anes_w_classes$white = ifelse(anes_w_classes$race == 1, 1, 0)
anes_w_classes$other_racial = ifelse(anes_w_classes$race == 4 | anes_w_classes$race == 6 , 1, 0)
anes_w_classes$church_freq = ifelse(anes_w_classes$church_freq == -9, NA, anes_w_classes$church_freq)
anes_w_classes$church_freq = ifelse(anes_w_classes$church_freq == -1, 6, anes_w_classes$church_freq)
anes_w_classes$church_freq = abs(anes_w_classes$church_freq - 6)
anes_w_classes$religion = ifelse(anes_w_classes$religion < 0, NA, anes_w_classes$religion)
anes_w_classes$income <- ifelse(is.na(anes_w_classes$income_pre), anes_w_classes$income_post, anes_w_classes$income_pre)
anes_w_classes$non_us_native <- ifelse(anes_w_classes$non_us_native > 0, 1, 0)

# Standard Deviation of responses
# select relevant attitudes
attitudes = c("attitude_AsianAmericans", "attitude_Blacks", "attitude_Hispanics", "attitude_Whites", "attitude_Jews", "attitude_Muslims", "attitude_Christians", "attitude_ChristianFundamentalists",
              "attitude_IllegalImmigrants", "attitude_RichPeople", "attitude_PoorPeople", "attitude_Liberals", "attitude_Conservatives", "attitude_GayLesbians", "attitude_Transgender",
              "attitude_Scientists", "attitude_Feminists")

# Use rowSds function from matrixStats package
anes_w_classes$within_ind_sd = matrixStats::rowSds(as.matrix(anes_w_classes[,attitudes]), na.rm = T)

# Another way to do it is to normalize attitudes first then take SD
attitude_vals <- anes_w_classes[,attitudes]
attitude_vals <- sapply(attitude_vals, scale)
anes_w_classes$within_ind_sd_scaled <- matrixStats::rowSds(attitude_vals, na.rm = T)

# The two sd measures are 92% correlated

# Construct categorical/dummy variables
anes_w_classes$racial_cat = dplyr::recode(anes_w_classes$race, `1` = "White", `2` = "Black", `3` = "Asian", `c(4,6)` = "Other", `5` = "Hispanic")
anes_w_classes$religion_cat = dplyr::recode(anes_w_classes$religion, `1` = "Protestant", `2` = "Evangelical", `3` = "Black Protestant", `4` = "Catholic", `5` = "Other Christian", `6` = "Jewish", `7` = "Other", `8` = "Not Religious")
anes_w_classes$sexual_orientation_cat = dplyr::recode(anes_w_classes$sexual_orientation, `1` = "Heterosexual", `2` = "Homosexual", `3` = "Bisexual")
anes_w_classes$homosexual = ifelse(anes_w_classes$sexual_orientation == 2, 1, 0)
anes_w_classes$heterosexual = ifelse(anes_w_classes$sexual_orientation == 1, 1, 0)
anes_w_classes$bisexual = ifelse(anes_w_classes$sexual_orientation == 3, 1, 0)
anes_w_classes$protestant = ifelse(anes_w_classes$religion == 1, 1, 0)
anes_w_classes$catholic = ifelse(anes_w_classes$religion == 4, 1, 0)
anes_w_classes$evangelical = ifelse(anes_w_classes$religion == 2, 1, 0)
anes_w_classes$jewish = ifelse(anes_w_classes$religion == 6, 1, 0)
anes_w_classes$not_religious = ifelse(anes_w_classes$religion == 8, 1, 0)
anes_w_classes$black_protestant = ifelse(anes_w_classes$religion == 3, 1, 0)
anes_w_classes$other_religion = ifelse(anes_w_classes$religion == 7, 1, 0)
anes_w_classes$other_christian = ifelse(anes_w_classes$religion == 5, 1, 0)
anes_w_classes$southern = ifelse(anes_w_classes$region == "3. South", 1, 0)
anes_w_classes$northeast = ifelse(anes_w_classes$region == "1. Northeast", 1, 0)
anes_w_classes$midwest = ifelse(anes_w_classes$region == "2. Midwest", 1, 0)
anes_w_classes$west = ifelse(anes_w_classes$region == "4. West", 1, 0)
anes_w_classes$college_grad = ifelse(anes_w_classes$education > 12, 1, 0)
anes_w_classes$less_high_school = ifelse(anes_w_classes$education < 9, 1, 0)
anes_w_classes$high_school_some_college = ifelse(anes_w_classes$education == 9 | anes_w_classes$education == 10 | anes_w_classes$education == 11 | anes_w_classes$education == 12, 1, 0) # includes vocational and associate degrees
anes_w_classes$bachelors_degree = ifelse(anes_w_classes$education == 13, 1, 0)
anes_w_classes$advanced_degree = ifelse(anes_w_classes$education > 13, 1, 0)
anes_w_classes$non_hetero = ifelse(anes_w_classes$sexual_orientation == 2 | anes_w_classes$sexual_orientation == 3, 1, 0)
anes_w_classes$strength_party_id  = abs(anes_w_classes$party_id - 4)
anes_w_classes$strength_ideology  = abs(anes_w_classes$political_identity - 4)
anes_w_classes$democrat = ifelse(anes_w_classes$party_id == 1 | anes_w_classes$party_id == 2 | anes_w_classes$party_id == 3,1,0)
anes_w_classes$independent = ifelse(anes_w_classes$party_id == 4, 1, 0)
anes_w_classes$republican = ifelse(anes_w_classes$party_id == 5 | anes_w_classes$party_id == 6 | anes_w_classes$party_id == 7,1,0)
anes_w_classes$attention_to_news = ifelse(anes_w_classes$attention_to_news < 0, NA, anes_w_classes$attention_to_news)
anes_w_classes$attention_to_news = abs(anes_w_classes$attention_to_news - 6)
anes_w_classes$political_interest = ifelse(anes_w_classes$political_interest < 0, NA, anes_w_classes$political_interest)
anes_w_classes$political_interest = abs(anes_w_classes$political_interest - 5)
anes_w_classes$personal_discrim = ifelse(anes_w_classes$personal_discrim == -9 | anes_w_classes$personal_discrim == -4, NA, anes_w_classes$personal_discrim)
anes_w_classes$personal_discrim = abs(anes_w_classes$personal_discrim - 6)

# Income quintiles
anes_w_classes$income[anes_w_classes$income %in% c(-5, -9)] <- NA
inc_cats_2016 <- quantile(anes_w_classes$income, c(0, .17, .33, .67, .83, 1), na.rm = TRUE) # Following Baldassarri & Goldberg (2014), I divide income into 5 categories that correspond to 0-17, 17-33, 33-67, 67-83 and 83-100 percentiles (this also allows for standardization across years)
anes_w_classes$income_quantiles <- cut(anes_w_classes$income, breaks = inc_cats_2016, labels = c(1:5),
                                       include.lowest = TRUE, right = TRUE, ordered_result = TRUE)
# mMke discrete variables for income quintiles
anes_w_classes$income_q_1 = ifelse(anes_w_classes$income_quantiles == 1, 1, 0)
anes_w_classes$income_q_2 = ifelse(anes_w_classes$income_quantiles == 2, 1, 0)
anes_w_classes$income_q_3 = ifelse(anes_w_classes$income_quantiles == 3, 1, 0)
anes_w_classes$income_q_4 = ifelse(anes_w_classes$income_quantiles == 4, 1, 0)
anes_w_classes$income_q_5 = ifelse(anes_w_classes$income_quantiles == 5, 1, 0)

# Make class numbers line up with our interpretation (Class 1 = Partisan; Class 2 = Racial; Class 3 = Neutral)
anes_w_classes$RCA_new = dplyr::recode(anes_w_classes$RCA, `1` = "1", `2` = "3", `3` = "2")
anes_w_classes$RCA_new = as.numeric(anes_w_classes$RCA_new)
anes_w_classes$RCA_new_cat = dplyr::recode(anes_w_classes$RCA_new, `1` = "Partisan", `2` = "Racialist", `3` = "Neutral")

# Make discrete class dummies
anes_w_classes$RCA_1_new = ifelse(anes_w_classes$RCA_new == 1, 1, 0)
anes_w_classes$RCA_2_new = ifelse(anes_w_classes$RCA_new == 2, 1, 0)
anes_w_classes$RCA_3_new = ifelse(anes_w_classes$RCA_new == 3, 1, 0)


## race variables
anes_w_classes$preferential_hiring_blacks = ifelse(anes_w_classes$preferential_hiring_blacks < 0, NA, anes_w_classes$preferential_hiring_blacks)
anes_w_classes$preferential_hiring_blacks = abs(anes_w_classes$preferential_hiring_blacks-6)

anes_w_classes$affirmative_action = ifelse(anes_w_classes$affirmative_action < 0, NA, anes_w_classes$affirmative_action)
anes_w_classes$affirmative_action = abs(anes_w_classes$affirmative_action-8)

## only asked from white respondentes~!!!
anes_w_classes$whites_jobs_to_minorities = ifelse(anes_w_classes$whites_jobs_to_minorities < 0, NA, anes_w_classes$whites_jobs_to_minorities)
anes_w_classes$whites_jobs_to_minorities = abs(anes_w_classes$whites_jobs_to_minorities-6)

## Laurison variables
pol_qs_w_99 <- c("pol_lib_cons_scale", "pol_gov_spending")

# DKs for these two is tricky. -9 is refused, 999 is dont know this group (very few 999s)
# interviewers probed DKs: "PROBE for don’t know response: when you say don’t know, do you mean that you don’t know who the person is, or do you have something else in mind?"
pol_qs_w_999 <- c("pol_therm_supreme_court", "pol_therm_congress")

# all other political varibales
regular_pol_qs <- c("pol_gov_waste_tax", "pol_fed_spend_soc_sec", 
  "pol_fed_spend_pub_schl", "pol_fed_spend_sci_tech", 
  "pol_fed_spend_crime", "pol_fed_spend_welfare", 
  "pol_fed_spend_childcare", "pol_fed_spend_poor", "pol_fed_spend_env")

# non-political variables
non_pol_qs <- c("non_pol_ppl_trusted", 
                "non_pol_world_adjust", 
                "non_pol_tolerant_moral", 
                "non_pol_traditional_fam")

# total list of all political questions
all_pol_qs <- c(pol_qs_w_99, pol_qs_w_999, regular_pol_qs)

# total list of all questions
all_qs <- c(all_pol_qs, non_pol_qs)

# just to be safe, create new data
df <- anes_w_classes

# list of don't know values
dk_vals = c(-8, -9)

# list of values which should be NA. if you want to remove refusal as DK, 
# put -9 here and remove it from above
to_na = c(-6, -7)

# loop through questions
for(var in all_qs){
  
  # set NAs
  to_na_temp <- to_na
  
  # if the variable is in the qs where 99 is DK, add it to list of dk_vals
  dk_vals_temp <- dk_vals
  if(var %in% pol_qs_w_99){
    dk_vals_temp <- c(dk_vals_temp, 99)
  }
  
  # or add 999 if it is in the questions where 999 is DK
  # here, 998 is added as an NA (temporarily)
  if(var %in% pol_qs_w_999){
    dk_vals_temp <- c(dk_vals_temp, 999)
    to_na_temp <- c(to_na_temp, 998)
  }
  
  # finally, if to_na_temp is non-null (i.e. some responses should be set to NA)
  # before identifying don't knows, then they will be set to NA
  if(!is.null(to_na_temp)){
    df$temp <- ifelse(df[,var] %in% to_na_temp, NA, df[,var])
  }
  # identifying NAs as those in the dk_vals_temp vector
  df$temp_df <- ifelse(df[,var] %in% dk_vals_temp, 1, 0)
  
  # set column name to variable + _dk
  colnames(df)[length(df)] = paste0(var, "_dk")
}

# count the number of political, non-political, and total (pol + non_pol) variables that they were asked)
df$total_qs <- apply(df[,paste0(all_qs, "_dk")], 1, FUN = function(x) sum(!is.na(x)))
df$total_pol_qs <- apply(df[,paste0(all_pol_qs, "_dk")], 1, FUN = function(x) sum(!is.na(x)))
df$total_non_pol_qs <- apply(df[,paste0(non_pol_qs, "_dk")], 1, FUN = function(x) sum(!is.na(x)))

# count the number of DKs across 1) total qs, 2) pol qs, and 3) non pol qs
df$dk_qs <- rowSums(df[,paste0(all_qs, "_dk")], na.rm = T)
df$pol_dk_qs <- rowSums(df[,paste0(all_pol_qs, "_dk")], na.rm = T)
df$non_pol_dk_qs <- rowSums(df[,paste0(non_pol_qs, "_dk")], na.rm = T)

# divide dk counts by total var counts for each type
df$prop_dk <- df$dk_qs/df$total_qs
df$prop_pol_dk <- df$pol_dk_qs/df$total_pol_qs
df$prop_non_pol_dk <- df$non_pol_dk_qs/df$total_non_pol_qs

# drop the temp variable created as part of loop above
df <- df[,colnames(df) != "temp"]

# then, if all looks good (check to make sure!), assign resulting df back to anes_w_classes
anes_w_classes <- df

### now the kitchen soup method
df <- anes2016

# list of don't know values
dk_vals = c(-8, -9)

# list of values which should be NA. if you want to remove refusal as DK, 
# put -9 here and remove it from above
to_na = c(-1:-7)

# identify which columns have -8, -9, 99 or 999 in them (i.e. potential candidates for evaluating dk)
cols_w_89 <- c()
cols_w_99 <- c()
cols_w_999 <- c()

for(i in 2:ncol(anes_w_classes_all)){
  if((-8 %in% anes_w_classes_all[,i]) | (-9 %in% anes_w_classes_all[,i])){
    cols_w_89 <- c(cols_w_89, i)
  }
  if((99 %in% anes_w_classes_all[,i])){
    cols_w_99 <- c(cols_w_99, i)
  }
  if((999 %in% anes_w_classes_all[,i])){
    cols_w_999 <- c(cols_w_999, i)
  }
}

cols_w_999 <- subset(cols_w_999, !cols_w_999 %in% cols_w_89)
cols_w_99 <- subset(cols_w_99, (!cols_w_99 %in% cols_w_89) & (!cols_w_99 %in% cols_w_999))

# loop through questions
for(var in c(cols_w_89, cols_w_99, cols_w_999)){
  
  # set NAs
  to_na_temp <- to_na
  
  # if the variable is in the qs where 99 is DK, add it to list of dk_vals
  dk_vals_temp <- dk_vals
  if(var %in% cols_w_99){
    dk_vals_temp <- c(dk_vals_temp, 99)
  }
  
  # or add 999 if it is in the questions where 999 is DK
  # here, 998 is added as an NA (temporarily)
  if(var %in% cols_w_999){
    dk_vals_temp <- c(dk_vals_temp, 999)
    to_na_temp <- c(to_na_temp, 998)
  }
  
  # finally, if to_na_temp is non-null (i.e. some responses should be set to NA)
  # before identifying don't knows, then they will be set to NA
  if(!is.null(to_na_temp)){
    df$temp <- ifelse(df[,var] %in% to_na_temp, NA, df[,var])
  } else {
    df$temp <- df[,var]
  }
  # identifying NAs as those in the dk_vals_temp vector
  df$temp_dk <- ifelse(df$temp %in% dk_vals_temp, 1, 0)
  df$temp_dk <- ifelse(is.na(df$temp), NA, df$temp_dk)
  
  # set column name to variable + _dk
  colnames(df)[length(df)] = paste0(var, "_dk")
}

# count number of DKs for across all variables
dk_rows <- df[,grepl("_dk", colnames(df))]
df$total_dks_all_qs <- rowSums(dk_rows, na.rm = T)

# now count number of variables observed for each person
df_mat <- as.matrix(dk_rows)
df_mat[] <- ifelse(!is.na(df_mat[]), 1, NA)
df$total_qs <- rowSums(df_mat, na.rm = T)

# divide the two sums to get overall proportion dk
df$total_prop_dk <- df$total_dks_all_qs/df$total_qs

## these are the kitchen soup variables
df <- df[,c("ID", "total_dks_all_qs", "total_qs", "total_prop_dk")]

# merge into main data
anes_w_classes <- merge(anes_w_classes, df, by = "ID")

# 
all_groups <- c("attitude_ChristianFundamentalists", "attitude_Feminists",
                "attitude_Liberals", 
                "attitude_PoorPeople", 
                "attitude_Conservatives", 
                "attitude_GayLesbians", 
                "attitude_RichPeople", "attitude_Muslims",
                "attitude_Christians", "attitude_Jews",
                "attitude_Transgender", "attitude_Scientists",
                "attitude_AsianAmericans",
                "attitude_Hispanics", "attitude_Blacks",
                "attitude_IllegalImmigrants", "attitude_Whites")

racial_ethnic_attitudes <- c("attitude_AsianAmericans",
                      "attitude_Hispanics", "attitude_Blacks",
                      "attitude_Whites")

# exclude racial/ethnic groups according to above classifications
non_racial_ethnic_attitudes <- subset(all_groups, !all_groups %in% racial_ethnic_attitudes)

# identify exactly 50 responses
exactly_fifty <- sapply(anes_w_classes[,all_groups], FUN = function(x) as.numeric(x == 50))

# proportion exactly fifty
anes_w_classes$prop_exactly_fifty <- rowMeans(exactly_fifty)

# evaluate proportions for each of the categorizations above
anes_w_classes$prop_racial_ethnic_exactly_fifty <- rowMeans(exactly_fifty[,racial_ethnic_attitudes])
anes_w_classes$prop_non_racial_ethnic_exactly_fifty <- rowMeans(exactly_fifty[,non_racial_ethnic_attitudes])

anes_w_classes <- merge(anes2016, anes_w_classes, by = "ID")

# write final data to .RDS file
saveRDS(anes_w_classes, "anes_w_classes_w_covars_and_dks.RDS")
