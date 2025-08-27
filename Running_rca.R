### Running RCA

# Read data
anes2016 = read.dta13("anes_timeseries_2016_Stata13.dta")

# List of relevant variables (ANES code)
vars = c("V160001", c("V162095", "V162096", "V162097", "V162099", "V162101", "V162103", "V162105", "V162106", "V162107", "V162108", "V162111", "V162112", "V162310", "V162311", "V162312", "V162313", "V162314"))

# Names of relevant variables
labels = c("ID", c("ChristianFundamentalists", "Feminists", "Liberals", "PoorPeople", "Conservatives", "GayLesbians", "RichPeople", "Muslims", "Christians", "Jews", "Transgender", "Scientists", "AsianAmericans", "Hispanics", "Blacks", "IllegalImmigrants", "Whites"))

# Subset to relevant variables
anes2016 = anes2016[,vars]

# Replace names
colnames(anes2016) = labels

# Clean data (drop year and ID, make matrix, set all values > 110 or < 0 to NA, add 1 to every value (so that no variable has 0))
anes_run = anes2016[,2:length(anes2016)]
anes_run = as.matrix(anes_run)
anes_run[] = ifelse(anes_run[] < 0 | anes_run > 110, NA, anes_run[])
anes_run[] = anes_run[] + 1

# Add cleaned data back to regular data (basically preserving year and id which were dropped as part of cleaning stage)
anes2016[,2:length(anes2016)] = anes_run

anes2016 <- na.omit(anes2016)

# Apply rescale and categorize function dataset (saving as new name so RCA can be run on each)
anes2016_cat = anes2016
anes2016_cat[,2:length(anes2016_cat)] = rescale_cat(anes2016_cat[,2:length(anes2016_cat)])
anes2016_cat[,2:length(anes2016_cat)] = sapply(anes2016_cat[,2:length(anes2016_cat)], FUN = function(x) as.numeric(as.character(x)))

# Run RCA on each (starting with categories)
set.seed(1016)
rca_out_2016 = rca.full.3(as.matrix(anes2016_cat[,2:length(anes2016_cat)]), amirsd = F)

# Make a data.frame with the class and pole assignments
ids_to_classes = data.frame(ID = anes2016_cat$ID,
                            RCA = rca_out_2016$membership,
                            stringsAsFactors = F)


saveRDS(ids_to_classes, "RCA_ids.RDS")