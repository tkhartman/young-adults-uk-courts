##' ---
##' title: "MoJ Young Adults Data Analysis Project, Updated: 2007 - 2019"
##' author: "Nathan Hughes and Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' ---

## NOTE - Some analyses below have been commented out of this
## version of the R script because the MoJ datasets are different
## between 2007-2017 and 2009-2019. Use the original v7 
## script for this analysis (e.g., Police Force Area)

#### Housekeeping
## Install packages using 'pacman'
pacman::p_load(broom, data.table, ggplot2,
               googleVis, magicfor, nomisr,
               questionr, sjPlot, tidyverse)

#### Load 2007 - 2017 MoJ Data
## Entire Dataset  [18 variables measured over 11 years, 2007 – 2017]
## N = 3,908,325 (unweighted)
## N = 18,209,510 (weighted by ‘Count’)
## Full data and script files available: https://github.com/tkhartman/young-adults-uk-courts
## Criminal Justice System statistics quarterly: December 2016
## https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/614433/csv-behind-interactive-data-tools.zip
## Only loaded .csv file once -- converted to RData file for future use
# df <- fread("data-behind-interactive-data-tools/Court outcomes by PFA 2017.csv")

load("_Original_Data_Analysis_2007_2017/Court_outcomes_by_PFA_2017.RData")
df1 <- df
rm(df)
glimpse(df1)

#### Load 2009 - 2019 MoJ Data
## Entire Dataset  [20 variables measured over 11 years, 2009 – 2019]
## N = 986,497 (unweighted)
## N = 13,799,928 (weighted by ‘Count’)
## Criminal justice system statistics quarterly (December 2019):
## https://www.gov.uk/government/statistics/criminal-justice-system-statistics-quarterly-december-2019
## Full URL for data download:
## https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/888292/court-outcomes-by-PFA-2019.xlsx
## Only load .xlsx/.csv file once -- converted to RData file for future use (and smaller file size!)

load("_Updated_Data_Analysis_2009_2019/Court_outcomes_by_PFA_2009-2019.RData")
glimpse(df2)

## If desired, expand the data based on the 'Count' variable
## This makes a massive file with 18m+ or 13m+ observations
# df.full <- df[rep(1:nrow(df), df$Count), ]
# df2.full <- df2[rep(1:nrow(df2), df2$Count), ]

## Subset by years 2018 - 2019 for later appending
df2.sub <- df2 %>%
    filter(`Year.of.Appearance` %in% c(2018, 2019))

## Harmonize column names in df2.sub with df1
df2.sub <- df2.sub %>%
    rename(`Year of Appearance` = `Year.of.Appearance`, 
           `Court Type` = Courts,
           `Offence Type` = `Offence.Type`,
           `Offence Group` = `Offence.Group`,
           `Age Group` = `Age.Group`,
           `Age Range` = `Age.Range`,
           `Type of Defendant` = `Type.of.Offender`,
           `Custodial Sentence Length` = `Custodial.Sentence.Length`,
           `Outcome` = `Sentence.Outcome`
           )

## Select only variables in df2 that match df1
df2.sub <- df2.sub %>% 
    select(`Year of Appearance`, `Type of Defendant`, 
           Sex, `Age Group`, `Age Range`, Ethnicity, 
           `Court Type`, `Offence Type`, `Offence Group`, 
           `Outcome`, `Custodial Sentence Length`, Count)

## Recode values in df2 for consistency
df2.sub <- df2.sub %>%
    mutate(`Type of Defendant` = as.character(`Type of Defendant`), 
           Sex = as.character(recode_factor(Sex, 
                                            '03: Companies, public bodies etc.' = '03: Other (companies, public bodies, etc)' ) ),
           `Age Group` = as.character(recode_factor(`Age Group`, '01: Children' = '01: Juveniles', 
                                                    '05: Not known' = NA_character_ ) ),
            `Age Range` = as.character(recode_factor(`Age Range`, 
                                        '06: 25+ (prior to 2017)' = '06: 25+',
                                        '07: 25-29 (2017 onwards)' = '06: 25+',
                                        '08: 30-39 (2017 onwards)' = '06: 25+',
                                        '09: 40-49 (2017 onwards)' = '06: 25+',
                                        '10: 50-59 (2017 onwards)' = '06: 25+',
                                        '11: 60-69 (2017 onwards)' = '06: 25+',
                                        '12: 70+ (2017 onwards)' = '06: 25+' ) ),
           Ethnicity = as.character(Ethnicity),
           `Court Type` = as.character(recode_factor(`Court Type`, 
                                              'Crown Court' = '01: Crown Court', 
                                              'Magistrates Court' = '02: Magistrates Court') ),
           `Offence Type` = as.character(recode_factor(`Offence Type`, 
                                                       '05: Not known' = NA_character_) ),
           `Offence Group` = as.character(recode_factor(`Offence Group`, 
                                                        '09: Miscellaneous crimes agains' = '09: Miscellaneous crimes against society',
                                                        '13: Not known' = NA_character_) ),
           Outcome = as.character(recode_factor(Outcome, '01: Immediate Custody' = '15: Immediate custody',
                                                '02: Suspended Sentence' = '14: Suspended sentence',
                                                '03: Community Sentence' = '13: Community sentence',
                                                '04: Fine' = '12: Fine', 
                                                '05: Absolute Discharge' = '10: Absolute discharge', 
                                                '06: Conditional Dischar' = '11: Conditional discharge',
                                                 '07: Compensation' = '17: Compensation',
                                                '08: Otherwise Dealt Wit' = '16: Otherwise dealt with' ) ), 
           `Custodial Sentence Length` = as.character(recode_factor(`Custodial Sentence Length`,
                                                                    '15: Over 5 years and up to and including 6 years' = '15: Over 5 years and up to and including 7 years',
                                                                    '16: Over 6 years and up to and including 7 years' = '15: Over 5 years and up to and including 7 years',
                                                                    '17: Over 7 years and up to and including 8 years' = '16: Over 7 years and up to and including 10 years',
                                                                    '18: Over 8 years and up to and including 9 years' = '16: Over 7 years and up to and including 10 years',
                                                                    '19: Over 9 years and up to and including 10 years' = '16: Over 7 years and up to and including 10 years',
                                                                    '20: Over 10 years and up to and including 15 years' = '17: Over 10 years and less than life',
                                                                    '21: Over 15 years and less than life' = '17: Over 10 years and less than life',
                                                                    '22: Imprisonment for public protection' = '18: Imprisonment for public protection',
                                                                    '23: Life sentence' = '19: Life sentence' ) )
           )


## Append df2 onto df1 (i.e., years 2018 - 2019), keeping consistent variables
df <- add_row(df1, df2.sub)

## Number of observations by year unweighted/weighted
table(df$`Year of Appearance`)  # Unweighted
wtd.table(df$`Year of Appearance`, weights = df$Count)  # Weighted

#### Data Munging
## Create age group identifier (to include 21-24 in 'young adult')
age.group <- c("01: 10-11", "02: 12-14", "03: 15-17", 
               "08: Not known (Juvenile)",              # Juveniles
               "04: 18-20", "05: 21-24",                # Young adult
               "06: 25+", "09: Not known (Adult)")      # Adult
df$age <- ifelse(df$`Age Range` %in% age.group[1:4], "1 - Youth", 
                 ifelse(df$`Age Range` %in% age.group[5:6], "2 - Young Adult",
                        ifelse(df$`Age Range` %in% age.group[7:8], "3 - Adult", NA)
                 )
)

table(df$age)  # Unweighted
prop.table(table(df$age))  # Unweighted

wtd.table(df$age, weights = df$Count)  # Weighted
prop.table(wtd.table(df$age, weights = df$Count))  # Weighted

## Create age group dummies for modelling
df$youth <- ifelse(df$age == "1 - Youth", 1, 0)
df$young.adult <- ifelse(df$age == "2 - Young Adult", 1, 0)
df$adult <- ifelse(df$age == "3 - Adult", 1, 0)

table(df$youth)
table(df$young.adult)
table(df$adult)

## Potential DV - Court Type
table(df$`Court Type`)
df$crown.court <- ifelse(df$`Court Type` == "01: Crown Court", 1, 0)
df$mag.court <- ifelse(df$`Court Type` == "02: Magistrates Court", 1, 0)

table(df$crown)
table(df$mag.court)

## Potential DV - Conviction Status
table(df$`Convicted/Not Convicted`)
df$convicted <- ifelse(df$`Convicted/Not Convicted` == "01: Convicted", 1, 0)
table(df$convicted)

## Potential DV - Sentencing Status
table(df$`Sentenced/Not Sentenced`)
df$sentenced <- ifelse(df$`Sentenced/Not Sentenced` == "01: Sentenced", 1, 0)
table(df$sentenced)

## Potential DV - Outcome
## Create combined outcome measure to focus only on 10, 11, 13, and 15
df$outcome.new <- 
    ifelse(df$Outcome == "10: Absolute discharge" | 
               df$Outcome == "11: Conditional discharge", "1 - Discharged", 
           ifelse(df$Outcome == "13: Community sentence", "2 - Community Sentence", 
                  ifelse(df$Outcome == "15: Immediate custody", 
                         "3 - Immediate Custody", "4 - Other")
           )
    )

table(df$outcome.new)
prop.table(table(df$outcome.new))

## Gender (comparison group is males and unknown)
table(df$Sex)
df$male <- ifelse(df$Sex == "01: Male", 1, 0)
df$female <- ifelse(df$Sex == "02: Female", 1, 0)
df$sex.unknown <- ifelse(df$Sex == "04: Not known", 1, 0)
table(df$male)
table(df$female)
table(df$sex.unknown)

## Ethnicity
table(df$Ethnicity)
df$white <- ifelse(df$Ethnicity == "01: White", 1, 0)
df$black <- ifelse(df$Ethnicity == "02: Black", 1, 0)
df$asian <- ifelse(df$Ethnicity == "03: Asian", 1, 0)
df$mixed <- ifelse(df$Ethnicity == "04: Mixed", 1, 0)
df$chinese <- ifelse(df$Ethnicity == "05: Chinese and other", 1, 0)
df$ethn.unknown <- ifelse(df$Ethnicity == "06: Not stated", 1, 0)
table(df$white)
table(df$black)
table(df$asian)
table(df$mixed)
table(df$chinese)
table(df$ethn.unknown)

## Subset data by individuals and non-summary offenses
table(df$`Type of Defendant`)  # Only individuals
table(df$`Offence Type`)  # Only non-summary offenses

df.sub <- subset(df, `Type of Defendant` == "01: Person" &
                     (`Offence Type` == "01: Indictable only" | 
                          `Offence Type` == "02: Triable Either Way") )

## Confirm subseting worked properly
table(df.sub$`Type of Defendant`)  # Only individuals
table(df.sub$`Offence Type`)  # Only non-summary offenses

#### Number of obs by year for subsetted data
## Unweighted N
nrow(df.sub)

## Weighted N
sum(df.sub$Count)

## Number of observations by year unweighted/weighted
table(df.sub$`Year of Appearance`)
wtd.table(df.sub$`Year of Appearance`, weights = df.sub$Count)

#############################################
#######  ALL APPEARANCES BY YEAR PLOT  ######
#############################################
#### Descriptives by YEAR
## Loop to subset data by year 
dframe <- vector(mode = "list", 13)  # Store subsetted dataframes in list 
year <- seq(from = 2007, to = 2019, by = 1)
for (j in 1:length(year)) {
    dframe[[j]] <- df.sub[df.sub$`Year of Appearance` == year[j], ]
}

## Labels for printing results
year.lab.1 <- rep(year, times = 1, each = 3)
age.lab.1 <- rep(c("Youth", "Young Adult", "Adult"), times = 13)

## Loop to print results by year using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["age"]],
                        weights = dframe[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe[[i]][["age"]], 
                                           weights = dframe[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))

## Print AGE by YEAR
out1 <- magic_result_as_dataframe()  # Store results as vector
out1 <- out1[, -1]
out1 <- cbind(year.lab.1, age.lab.1, out1) 
names(out1) <- c("Year", "Age", "Raw", "Percent")
out1
write.csv(out1, "moj_results_all_appearances_by_year.csv")
magic_free()

## Plot the AGE trend lines
p1 <- ggplot(data = out1, 
             aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                 y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ylim(0, 80) +
    ggtitle("Court Appearances by Age Group")
p1
ggsave("moj_results_all_appearances_by_year.png", dpi = 320, 
       scale = 1.5)

###############################################
#######  COURT TYPE TRENDS BY YEAR PLOT  ######
###############################################
#### BETWEEN AGE GROUPS
## Weighted court type by age group
year.lab.2 <- rep(year, times = 1, each = 6)
age.lab.2 <- rep(c("Youth", "Young Adult", "Adult"), times = 26)
court.lab <- rep(c("Crown Court", "Magistratres Court"), times = 13, each = 3) 

magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["age"]], 
                        dframe[[i]][["Court Type"]], 
                        weights = dframe[[i]][["Count"]]),
        Percent = 100*prop.table(wtd.table(dframe[[i]][["age"]], 
                                           dframe[[i]][["Court Type"]], 
                                           weights = dframe[[i]][["Count"]]), 2)
    )
}

## 2017 COURT TYPE by AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     df.sub$`Court Type`[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     df.sub$`Court Type`[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)

## Print COURT TYPE by AGE and YEAR
out2 <- magic_result_as_dataframe()  # Store results as dataframe
out2 <- out2[, -1]
out2 <- cbind(year.lab.2, age.lab.2, court.lab, out2)  # Add labels
names(out2) <- c("Year", "Age", "Court", "Raw", "Percent")
out2  # Print output
write.csv(out2, "moj_results_court_by_age_year.csv")
magic_free()

## Plot the COURT TYPE by AGE trend lines
p2 <- ggplot(data = out2, 
             aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                 y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 80) +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ggtitle("by Court Type") 
p2 + facet_grid(cols = vars(Court))
ggsave("moj_results_court_by_age_by_year.png", dpi = 320, 
       scale = 1.5)

#### WITHIN AGE GROUPS
## Weighted age group by court type
court.lab.b <- rep(c("Crown Court", "Magistratres Court"), times = 39) 
age.lab.2.b <- rep(c("Youth", "Young Adult", "Adult"), times = 13, each = 2)


magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["Court Type"]], 
                        dframe[[i]][["age"]], 
                        weights = dframe[[i]][["Count"]]),
        Percent = 100*prop.table(wtd.table(dframe[[i]][["Court Type"]], 
                                           dframe[[i]][["age"]], 
                                           weights = dframe[[i]][["Count"]]), 2)
    )
}

## 2017 AGE by COURT TYPE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$`Court Type`[df.sub$`Year of Appearance` == 2017], 
                     df.sub$age[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$`Court Type`[df.sub$`Year of Appearance` == 2017], 
                     df.sub$age[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)

## Print COURT TYPE by AGE and YEAR
out2b <- magic_result_as_dataframe()  # Store results as dataframe
out2b <- out2b[, -1]
out2b <- cbind(year.lab.2, court.lab.b, age.lab.2.b, out2b)  # Add labels
names(out2b) <- c("Year", "Court", "Age", "Raw", "Percent")
out2b  # Print output
write.csv(out2b, "moj_results_age_by_court_by_year.csv")
magic_free()

## Plot the AGE by COURT TYPE trend lines
p2b <- ggplot(data = out2b, 
              aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                  y = Percent, group = Court, color = Court)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 100) +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    ggtitle("by Age Group") 
p2b + facet_grid(cols = vars(Age))
ggsave("moj_results_age_by_court_by_year.png", dpi = 320, 
       scale = 1.5)


######################################################
#######  CONVICTION STATUS TRENDS BY YEAR PLOT  ######
######################################################
#### BETWEEN AGE GROUPS
## Weighted conviction status by age group
#convicted.lab <- rep(c("Convicted", "Not Convicted"), times = 13, each = 3)
#
#magic_for(silent = TRUE)
#for (i in 1:length(year)) {
#    put(
#        Raw = wtd.table(dframe[[i]][["age"]], 
#                        dframe[[i]][["Convicted/Not Convicted"]], 
#                        weights = dframe[[i]][["Count"]]),
#        Percent = 100*prop.table(wtd.table(dframe[[i]][["age"]], 
#                                           dframe[[i]][["Convicted/Not Convicted"]], 
#                                           weights = dframe[[i]][["Count"]]), 2)
#    )
#}
#
## 2017 CONVICTION STATUS by AGE by YEAR (to check loop outputs)
#addmargins(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$`Convicted/Not Convicted`[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
#prop.table(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$`Convicted/Not Convicted`[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)
#
## Print CONVICTION STATUS by AGE and YEAR
#out3 <- magic_result_as_dataframe()  # Store results as dataframe
#out3 <- out3[, -1]
#out3 <- cbind(year.lab.2, age.lab.2, convicted.lab, out3)  # Add labels
#names(out3) <- c("Year", "Age", "Conviction", "Raw", "Percent")
#out3  # Print output
#write.csv(out3, "moj_results_conviction_by_age_year.csv")
#magic_free()
#
## Plot the CONVICTION STATUS by AGE trend lines
#p3 <- ggplot(data = subset(out3, Conviction == "Convicted"), 
#             aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
#                 y = Percent, group = Age, color = Age)) + 
#    geom_point() +
#    geom_line() + 
#    xlab("Year") +
#    ylim(0, 80) +
#    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
#                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
#                     labels = c("07","08","09", "10", "11", "12",
#                                "13", "14", "15", "16", "17", "18", "19")) +
#    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
#    ggtitle("by Conviction Status") 
#p3 + facet_grid(cols = vars(Conviction))
#ggsave("moj_results_conviction_by_age_by_year.png")
#
#### WITHIN AGE GROUPS
#convicted.lab.b <- rep(c("Convicted", "Not Convicted"), times = 39)
#
#magic_for(silent = TRUE)
#for (i in 1:length(year)) {
#    put(
#        Raw = wtd.table(dframe[[i]][["Convicted/Not Convicted"]], 
#                        dframe[[i]][["age"]], 
#                        weights = dframe[[i]][["Count"]]),
#        Percent = 100*prop.table(wtd.table(dframe[[i]][["Convicted/Not Convicted"]], 
#                                           dframe[[i]][["age"]], 
#                                           weights = dframe[[i]][["Count"]]), 2)
#    )
#}
#
## 2017 AGE by CONVICTION STATUS by YEAR (to check loop outputs)
#addmargins(wtd.table(df.sub$`Convicted/Not Convicted`[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$age[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
#prop.table(wtd.table(df.sub$`Convicted/Not Convicted`[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$age[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)
#
## Print AGE by CONVICTION STATUS and YEAR
#out3b <- magic_result_as_dataframe()  # Store results as dataframe
#out3b <- out3b[, -1]
#out3b <- cbind(year.lab.2, convicted.lab.b, age.lab.2.b, out3b)  # Add labels
#names(out3b) <- c("Year", "Conviction", "Age", "Raw", "Percent")
#out3b  # Print output
#write.csv(out3b, "moj_results_age_by_conviction_by_year.csv")
#magic_free()
#
## Plot the CONVICTION STATUS by AGE trend lines
#p3b <- ggplot(data = out3b, 
#              aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
#                  y = Percent, group = Conviction, color = Conviction)) + 
#    geom_point() +
#    geom_line() + 
#    xlab("Year") +
#    ylim(0, 80) +
#    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
#                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
#                     labels = c("07","08","09", "10", "11", "12",
#                                "13", "14", "15", "16", "17", "18", "19")) +
#    ggtitle("by Age") 
#p3b + facet_grid(cols = vars(Age))
#ggsave("moj_results_age_by_conviction_by_year.png", dpi = 320, 
#       scale = 1.5)
#
#
######################################################
#######  SENTENCING STATUS TRENDS BY YEAR PLOT  ######
######################################################
#### BETWEEN AGE GROUPS
## Weighted sentencing status by age group
#sentenced.lab <- rep(c("Sentenced", "Not Sentenced"), times = 13, each = 3)
#
#magic_for(silent = TRUE)
#for (i in 1:length(year)) {
#    put(
#        Raw = wtd.table(dframe[[i]][["age"]], 
#                        dframe[[i]][["Sentenced/Not Sentenced"]], 
#                        weights = dframe[[i]][["Count"]]),
#        Percent = 100*prop.table(wtd.table(dframe[[i]][["age"]], 
#                                           dframe[[i]][["Sentenced/Not Sentenced"]], 
#                                           weights = dframe[[i]][["Count"]]), 2)
#    )
#}
#
## 2017 SENTENCING STATUS by AGE by YEAR (to check loop outputs)
#addmargins(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$`Sentenced/Not Sentenced`[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
#prop.table(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$`Sentenced/Not Sentenced`[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)
#
## Print SENTENCING STATUS by AGE and YEAR
#out4 <- magic_result_as_dataframe()  # Store results as dataframe
#out4 <- out4[, -1]
#out4 <- cbind(year.lab.2, age.lab.2, sentenced.lab, out4)  # Add labels
#names(out4) <- c("Year", "Age", "Sentencing", "Raw", "Percent")
#out4$Sentencing <- factor(out4$Sentencing, levels = c("Sentenced", "Not Sentenced"), 
#                          ordered = FALSE)
#out4  # Print output
#write.csv(out4, "moj_results_sentencing_by_age_year.csv")
#magic_free()
#
## Plot the SENTENCING STATUS by AGE trend lines
#p4 <- ggplot(data = subset(out4, Sentencing == "Sentenced"), 
#             aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
#                 y = Percent, group = Age, color = Age)) + 
#    geom_point() +
#    geom_line() + 
#    xlab("Year") +
#    ylim(0, 80) +
#    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
#                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
#                     labels = c("07","08","09", "10", "11", "12",
#                                "13", "14", "15", "16", "17", "18", "19")) +
#    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
#    ggtitle("by Sentencing Status") 
#p4 + facet_grid(cols = vars(Sentencing))
#ggsave("moj_results_sentencing_by_age_year.png", dpi = 320, 
#       scale = 1.5)
#
#
#### WITHIN AGE GROUPS
#sentenced.lab.b <- rep(c("Sentenced", "Not Sentenced"), times = 39)
#
#magic_for(silent = TRUE)
#for (i in 1:length(year)) {
#    put(
#        Raw = wtd.table(dframe[[i]][["Sentenced/Not Sentenced"]], 
#                        dframe[[i]][["age"]], 
#                        weights = dframe[[i]][["Count"]]),
#        Percent = 100*prop.table(wtd.table(dframe[[i]][["Sentenced/Not Sentenced"]], 
#                                           dframe[[i]][["age"]], 
#                                           weights = dframe[[i]][["Count"]]), 2)
#    )
#}
#
## 2017 SENTENCING STATUS by AGE by YEAR (to check loop outputs)
#addmargins(wtd.table(df.sub$`Sentenced/Not Sentenced`[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$age[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
#prop.table(wtd.table(df.sub$`Sentenced/Not Sentenced`[df.sub$`Year of Appearance` == 2017], 
#                     df.sub$age[df.sub$`Year of Appearance` == 2017],
#                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)
#
## Print SENTENCING STATUS by AGE and YEAR
#out4b <- magic_result_as_dataframe()  # Store results as dataframe
#out4b <- out4b[, -1]
#out4b <- cbind(year.lab.2, sentenced.lab.b, age.lab.2.b, out4b)  # Add labels
#names(out4b) <- c("Year", "Sentencing", "Age", "Raw", "Percent")
#out4b$Sentencing <- factor(out4b$Sentencing, levels = c("Sentenced", "Not Sentenced"), 
#                           ordered = FALSE)
#out4b  # Print output
#write.csv(out4b, "moj_results_age_by_sentencing_by_year.csv")
#magic_free()
#
## Plot the SENTENCING STATUS by AGE trend lines
#p4b <- ggplot(data = out4b, 
#              aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
#                  y = Percent, group = Sentencing, color = Sentencing)) + 
#    geom_point() +
#    geom_line() + 
#    xlab("Year") +
#    ylim(0, 80) +
#    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
#                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
#                     labels = c("07","08","09", "10", "11", "12",
#                                "13", "14", "15", "16", "17", "18", "19")) +
#    ggtitle("by Age Group") 
#p4b + facet_grid(cols = vars(Age))
#ggsave("moj_results_age_by_sentencing_by_year.png", dpi = 320, 
#       scale = 1.5)
#
#
######################################################
#######  OUTCOME TRENDS BY YEAR PLOT  ################
######################################################
#### BETWEEN AGE GROUPS
## Weighted outcome by age group
year.lab.3 <- rep(year, times = 1, each = 12)
age.lab.3 <- rep(c("Youth", "Young Adult", "Adult"), times = 52)
outcome.lab <- rep(c("Discharged", "Community Sentence", "Immediate Custody", "Other"), 
                   times = 13, each = 3)

magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["age"]], 
                        dframe[[i]][["outcome.new"]], 
                        weights = dframe[[i]][["Count"]]),
        Percent = 100*prop.table(wtd.table(dframe[[i]][["age"]], 
                                           dframe[[i]][["outcome.new"]], 
                                           weights = dframe[[i]][["Count"]]), 2)
    )
}

## 2017 OUTCOME by AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     df.sub$outcome.new[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     df.sub$outcome.new[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)

## Print OUTCOME by AGE and YEAR
out5 <- magic_result_as_dataframe()  # Store results as dataframe
out5 <- out5[, -1]
out5 <- cbind(year.lab.3, age.lab.3, outcome.lab, out5)  # Add labels
names(out5) <- c("Year", "Age", "Outcome", "Raw", "Percent")
out5  # Print output
write.csv(out5, "moj_results_outcome_by_age_year.csv")
magic_free()


## Plot the OUTCOME by AGE trend lines
p5 <- ggplot(data = subset(out5, Outcome != "Other"), 
             aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                 y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 80) +
    ggtitle("by Outcome") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))
p5 + facet_wrap(vars(Outcome), nrow = 1)
ggsave("moj_results_outcome_by_age_year.png", dpi = 320, 
       scale = 1.5)

#### WITHIN GROUPS
## Weighted outcome by age group
outcome.lab.b <- rep(c("Discharged", "Community Sentence", "Immediate Custody", "Other"), 
                     times = 39)
age.lab.3.b <- rep(c("Youth", "Young Adult", "Adult"), times = 13, each = 4)

magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["outcome.new"]], 
                        dframe[[i]][["age"]], 
                        weights = dframe[[i]][["Count"]]),
        Percent = 100*prop.table(wtd.table(dframe[[i]][["outcome.new"]], 
                                           dframe[[i]][["age"]], 
                                           weights = dframe[[i]][["Count"]]), 2)
    )
}

## 2017 OUTCOME by AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$outcome.new[df.sub$`Year of Appearance` == 2017], 
                     df.sub$age[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$outcome.new[df.sub$`Year of Appearance` == 2017], 
                     df.sub$age[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)

## Print OUTCOME by AGE and YEAR
out5b <- magic_result_as_dataframe()  # Store results as dataframe
out5b <- out5b[, -1]
out5b <- cbind(year.lab.3, outcome.lab.b, age.lab.3.b, out5b)  # Add labels
names(out5b) <- c("Year","Outcome", "Age", "Raw", "Percent")
out5b  # Print output
write.csv(out5b, "moj_results_age_by_outcome_by_year.csv")
magic_free()


## Plot the OUTCOME by AGE trend lines
p5b <- ggplot(data = subset(out5b, Outcome != "Other"), 
              aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                  y = Percent, group = Outcome, color = Outcome)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 80) +
    ggtitle("by Age Group") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19"))
p5b + facet_wrap(vars(Age), nrow = 1)
ggsave("moj_results_age_by_outcome_by_year.png", dpi = 320, 
       scale = 1.5)


############################################################
#######  OFFENSE GROUP TRENDS BY YEAR PLOT  ################
############################################################
#### BETWEEN AGE GROUPS
## Weighted outcome by age group
year.lab.4 <- rep(year, times = 1, each = 30)
age.lab.4 <- rep(c("Youth", "Young Adult", "Adult"), times = 130)
offence.lab <- rep(c("Violence", "Sexual", "Robbery", "Theft", "Damage", 
                     "Drug", "Weapons", "Public Order", "Miscellaneous", 
                     "Fraud"), 
                   times = 13, each = 3)

magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["age"]], 
                        dframe[[i]][["Offence Group"]], 
                        weights = dframe[[i]][["Count"]]),
        Percent = 100*prop.table(wtd.table(dframe[[i]][["age"]], 
                                           dframe[[i]][["Offence Group"]], 
                                           weights = dframe[[i]][["Count"]]), 2)
    )
}

## 2017 OFFENCE by AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     df.sub$`Offence Group`[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$age[df.sub$`Year of Appearance` == 2017], 
                     df.sub$`Offence Group`[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)

## Print OFFENCE by AGE and YEAR
out6 <- magic_result_as_dataframe()  # Store results as dataframe
out6 <- out6[, -1]
out6 <- cbind(year.lab.4, age.lab.4, offence.lab, out6)  # Add labels
names(out6) <- c("Year", "Age", "Offence", "Raw", "Percent")
tail(out6, n = 30)  # Print end of output -- i.e., 2017
write.csv(out6, "moj_results_offence_by_age_year.csv")
magic_free()


## Plot the OFFENCE by AGE trend lines
p6 <- ggplot(data = subset(out6, Offence == "Theft" | Offence == "Drug" | 
                               Offence == "Violence" | Offence == "Weapons"), 
             aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                 y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 100) +
    ggtitle("by Offence") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))
p6 + facet_wrap(vars(Offence), nrow = 2)
ggsave("moj_results_offence_by_age_year.png", dpi = 320, 
       scale = 1.5)

#### WITHIN GROUPS
offence.lab.b <- rep(c("Violence", "Sexual", "Robbery", "Theft", "Damage", 
                       "Drug", "Weapons", "Public Order", "Miscellaneous", 
                       "Fraud"), times = 39)
age.lab.4.b <- rep(c("Youth", "Young Adult", "Adult"), times = 13, each = 10)


magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe[[i]][["Offence Group"]], 
                        dframe[[i]][["age"]], 
                        weights = dframe[[i]][["Count"]]),
        Percent = 100*prop.table(wtd.table(dframe[[i]][["Offence Group"]], 
                                           dframe[[i]][["age"]], 
                                           weights = dframe[[i]][["Count"]]), 2)
    )
}

## 2017 OFFENCE by AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub$`Offence Group`[df.sub$`Year of Appearance` == 2017], 
                     df.sub$`age`[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub$`Offence Group`[df.sub$`Year of Appearance` == 2017], 
                     df.sub$age[df.sub$`Year of Appearance` == 2017],
                     weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]), 2)

## Print OFFENCE by AGE and YEAR
out6b <- magic_result_as_dataframe()  # Store results as dataframe
out6b <- out6b[, -1]
out6b <- cbind(year.lab.4, offence.lab.b, age.lab.4.b, out6b)  # Add labels
names(out6b) <- c("Year", "Offence", "Age", "Raw", "Percent")
tail(out6b, n = 30)  # Print end of output -- i.e., 2017
write.csv(out6b, "moj_results_age_by_offence_by_year.csv")
magic_free()


## Plot the OFFENCE by AGE trend lines
p6b <- ggplot(data = subset(out6b, Offence != "Miscellaneous"), 
              aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                  y = Percent, group = Offence, color = Offence)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 40) +
    ggtitle("by Age Group") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19"))
p6b + facet_wrap(vars(Age), nrow = 1)
ggsave("moj_results_age_by_offence_year.png", dpi = 320, 
       scale = 1.5)


## Plot the OFFENCE by Young Adults ONLY
p6b.ya <- ggplot(data = subset(out6b, Offence != "Miscellaneous" & Age == "Young Adult"), 
                 aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                     y = Percent, group = Offence, color = Offence)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylim(0, 35) +
    ggtitle("Young Adult") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19"))
p6b.ya + facet_wrap(vars(Age), nrow = 1)
ggsave("moj_results_YOUNG_ADULT_by_offence_year.png", dpi = 320, 
       scale = 1.5)

######################################################################
#######  ALL APPEARANCES AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT 
######################################################################
pop <- read.csv("_Updated_Data_Analysis_2009_2019/nomis_2018_11_27_154545_v3_WITH_TOTALS_UPDATED.csv")
out.pop <- out1
out.pop <- out.pop[, -4]
out.pop <- cbind(out.pop, "Total" = pop[, 3]) 
out.pop$Rate <- (out.pop$Raw/out.pop$Total) * 1000  # Number per 1,0000
out.pop
write.csv(out.pop, "moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_year.csv")

## Plot the AGE trend lines
p1.pop <- ggplot(data = out.pop, 
                 aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                     y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylab("Rate (per thousand individuals)") +
    ylim(0, 35) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ggtitle("Court Appearance Rate by Age Group")
p1.pop
ggsave("moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_year.png", dpi = 320, 
       scale = 1.5)

######################################################################
#######  COURT AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  #########
######################################################################
#### Toy example
d <- data.frame(year = c(1,1,1,2,2,2,3,3,3), 
                age = c(1,2,3, 1, 2, 3, 1, 2, 3), 
                total = c(5,10,15, 6,11,16,7,12,17))
d1 <- rbind(d, d)
d1[with(d1, order(year)), ]

#### Apply example to real data
tot2 <- rbind(pop, pop)
tot2 <- tot2[with(tot2, order(Year)), ]
tot2
out2.pop <- out2
out2.pop <- out2.pop[, -5]
out2.pop <- cbind(out2.pop, "Total" = tot2$Total) 
out2.pop$Rate <- (out2.pop$Raw/out2.pop$Total) * 1000  # Number per 1,000
out2.pop
write.csv(out2.pop, "moj_results_COURT_RATE_PER_THOUSAND_by_year.csv")

## Plot the COURT by AGE trend lines
p2.pop <- ggplot(data = out2.pop, 
                 aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                     y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    ylim(c(0, 25)) +
    xlab("Year") +
    ggtitle("Court Rate (per thousand)") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))

p2.pop + facet_wrap(vars(Court), nrow = 1)
ggsave("moj_results_COURT_RATE_PER_THOUSAND_age_year.png", dpi = 320, 
       scale = 1.5)


######################################################################
#######  CONVICTION AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  ####
######################################################################
#out3.pop <- out3
#out3.pop <- out3.pop[, -5]
#out3.pop <- cbind(out3.pop, "Total" = tot2$Total) 
#out3.pop$Rate <- (out3.pop$Raw/out3.pop$Total) * 1000  # Number per 1,000
#out3.pop
#write.csv(out3.pop, "moj_results_CONVICTION_RATE_PER_THOUSAND_by_year.csv")
#
## Plot the CONVICTION by AGE trend lines
#p3.pop <- ggplot(data = subset(out3.pop, Conviction == "Convicted"), 
#                 aes(x = factor(Year, levels = seq(from = 2007, to = 2017, by = 1)), 
#                     y = Rate, group = Age, color = Age)) + 
#    geom_point() +
#    geom_line() + 
#    ylim(c(0, 20)) +
#    xlab("Year") +
#    ggtitle("Conviction Rate (per thousand)") +
#    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
#                                "2013", "2014", "2015", "2016", "2017"),
#                     labels = c("07","08","09", "10", "11", "12",
#                                "13", "14", "15", "16", "17")) +
#    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))
#
#p3.pop + facet_wrap(vars(Conviction), nrow = 1)
#ggsave("moj_results_CONVICTION_RATE_PER_THOUSAND_age_year.png", dpi = 320, 
#       scale = 1.5)
#
#
######################################################################
#######  SENTENCING AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  ####
######################################################################
#out4.pop <- out4
#out4.pop <- out4.pop[, -5]
#out4.pop <- cbind(out4.pop, "Total" = tot2$Total) 
#out4.pop$Rate <- (out4.pop$Raw/out4.pop$Total) * 1000  # Number per 1,000
#out4.pop
#write.csv(out4.pop, "moj_results_SENTENCING_RATE_PER_THOUSAND_by_year.csv")
#
## Plot the SENTENCING by AGE trend lines
#p4.pop <- ggplot(data = subset(out4.pop, Sentencing == "Sentenced"), 
#                 aes(x = factor(Year, levels = seq(from = 2007, to = 2017, by = 1)), 
#                     y = Rate, group = Age, color = Age)) + 
#    geom_point() +
#    geom_line() + 
#    ylim(c(0, 20)) +
#    xlab("Year") +
#    ggtitle("Sentencing Rate (per thousand)") +
#    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
#                                "2013", "2014", "2015", "2016", "2017"),
#                     labels = c("07","08","09", "10", "11", "12",
#                                "13", "14", "15", "16", "17")) +
#    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))
#
#p4.pop + facet_wrap(vars(Sentencing), nrow = 1)
#ggsave("moj_results_SENTENCING_RATE_PER_THOUSAND_age_year.png", dpi = 320, 
#       scale = 1.5)
#
#
######################################################################
#######  OUTCOME AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  ####
######################################################################
tot5 <- rbind(pop, pop, pop, pop)
tot5 <- tot5[with(tot5, order(Year)), ]
tot5

out5.pop <- out5
out5.pop <- out5.pop[, -5]
out5.pop <- cbind(out5.pop, "Total" = tot5$Total) 
out5.pop$Rate <- (out5.pop$Raw/out5.pop$Total) * 1000  # Number per 1,000
out5.pop
write.csv(out5.pop, "moj_results_OUTCOME_RATE_PER_THOUSAND_by_year.csv")

## Plot the OUTCOME by AGE trend lines
p5.pop <- ggplot(data = subset(out5.pop, Outcome != "Other"), 
                 aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                     y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    ylim(c(0, 8)) +
    xlab("Year") +
    ggtitle("Outcome Rate (per thousand)") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))

p5.pop + facet_wrap(vars(Outcome), nrow = 1)
ggsave("moj_results_OUTCOME_RATE_PER_THOUSAND_age_year.png", dpi = 320, 
       scale = 1.5)


######################################################################
#######  OFFENCE AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  #######
######################################################################
tot6 <- rbind(pop, pop, pop, pop, pop, pop, pop, pop, pop, pop)
tot6 <- tot6[with(tot6, order(Year)), ]
tot6

out6.pop <- out6
out6.pop <- out6.pop[, -5]
out6.pop <- cbind(out6.pop, "Total" = tot6$Total) 
out6.pop$Rate <- (out6.pop$Raw/out6.pop$Total) * 10000  # Number per 10,000
out6.pop
write.csv(out6.pop, "moj_results_OFFENSE_RATE_PER_THOUSAND_by_year.csv")

## Plot the OFFENCE by AGE trend lines
p6.pop <- ggplot(data = out6.pop, 
                 aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                     y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ggtitle("Charged Offense Rate (per thousand)") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    expand_limits(y = 0) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))

p6.pop + facet_wrap(vars(Offence), scales = "free_y", nrow = 4)
ggsave("moj_results_offence_RATE_PER_THOUSAND_age_year.png", 
       width = 12, height = 9)

p6.pop.2 <- ggplot(data = subset(out6.pop, Offence == "Theft" | Offence == "Drug" |
                                     Offence == "Violence" | Offence == "Weapons"), 
                   aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                       y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ggtitle("Charged Offense Rate (per thousand)") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    expand_limits(y = 0) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))

p6.pop.2 + facet_wrap(vars(Offence), scales = "free_y", nrow = 2)
ggsave("moj_results_MAJOR_offences_RATE_PER_THOUSAND_age_year.png", 
       width = 12, height = 9)

######################################################################
#######  COMBINED RATES AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT #
######################################################################
out.sub1 <- subset(out.pop, select = c("Year", "Age", "Rate"))
out.sub1$Type <- "System"
out.sub1 <- subset(out.sub1, select = c("Year", "Age", "Type", "Rate"))

out.sub2 <- subset(out2.pop, select = c("Year", "Age", "Court", "Rate"))
names(out.sub2) <- c("Year", "Age", "Type", "Rate")

#out.sub3 <- subset(out3.pop, Conviction == "Convicted", 
#                   select = c("Year", "Age", "Conviction", "Rate"))
#names(out.sub3) <- c("Year", "Age", "Type", "Rate")
#
#out.sub4 <- subset(out4.pop, Sentencing == "Sentenced", 
#                   select = c("Year", "Age", "Sentencing", "Rate"))
#names(out.sub4) <- c("Year", "Age", "Type", "Rate")

out.sub5 <- subset(out5.pop, select = c("Year", "Age", "Outcome", "Rate"))
names(out.sub5) <- c("Year", "Age", "Type", "Rate")
out.sub5$Type <- as.character(out.sub5$Type)
out.sub5$Type[out.sub5$Type == "Other"] <- "Other Outcome"


out.sub6 <- subset(out6.pop, select = c("Year", "Age", "Offence", "Rate"))
names(out.sub6) <- c("Year", "Age", "Type", "Rate")
out.sub6$Type <- as.character(out.sub6$Type)
out.sub6$Type[out.sub6$Type == "Miscellaneous"] <- "Misc Crime"


combined <- rbind(out.sub1, out.sub2, #out.sub3, out.sub4,
                  out.sub5, out.sub6)
combined$Type <- factor(combined$Type, 
                        levels = c("System", "Crown Court", "Magistratres Court",
                                   #"Convicted", "Sentenced", 
                                   "Immediate Custody",
                                   "Community Sentence", "Discharged", "Other Outcome",
                                   "Violence", "Robbery", "Public Order", "Weapons",
                                   "Sexual", "Drug", "Theft", "Damage", "Fraud", 
                                   "Misc Offence"), 
                        ordered = FALSE)
combined <- combined[with(combined, order(Year)), ]
head(combined)
write.csv(combined, "moj_results_ALL_RATES_PER_THOUSAND_by_year.csv")

## Plot the ALL RATES by AGE trend lines
p7.pop <- ggplot(data = subset(combined, Type != "Misc Offence"), 
                 aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                     y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ggtitle("All Rates (per thousand)") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    expand_limits(y = 0) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))


p7.pop + facet_wrap(vars(Type), scales = "free_y", nrow = 5)
ggsave("moj_results_ALL_RATES_PER_THOUSAND_age_year.png", 
       width = 12, height = 9)


######################################################################
#######  APPEARANCES BY REGION - CHANGE FROM 2007 WITHIN AGE GROUPS
######################################################################
#### WITHIN AGE GROUPS
## 2017 AGE by POLICE FORCE AREA (to check loop outputs)
#out7b.2007 <- addmargins(wtd.table(df.sub$`Police Force Area`[df.sub$`Year of Appearance` == 2007], 
#                                   df.sub$age[df.sub$`Year of Appearance` == 2007],
#                                   weights = df.sub$Count[df.sub$`Year of Appearance` == 2007]))
#out7b.2007
#
## Young adult propritions in 2007
#out7b.young.2007 <- out7b.2007[, 2] /  out7b.2007[, 4] 
#out7b.young.2007 <- out7b.young.2007[-44]
#
#rank.7b.2007 <- order(out7b.young.2007)
#rank.7b2.2007 <- rev(rank.7b.2007)
#out7b.young.rank.2007 <- out7b.young.2007[rank.7b.2007]
#out7b.young.rank.2007
#
#out7b.2007.df <- as.data.frame(out7b.young.rank.2007)
#out7b.2007.df$Area <- rownames(out7b.2007.df) 
#rownames(out7b.2007.df) <- NULL
#names(out7b.2007.df) <- c("Proportion", "Area")
#
#### PLOT 2007 Proportions
#p7b <- ggplot(data = out7b.2007.df, aes(x = Area, y = Proportion)) +
#    geom_point() +
#    ylim(c(0, .35)) +
#    scale_x_discrete(limits = out7b.2007.df$Area) +
#    ylab("Proportion (in 2007)") +
#    xlab("") +
#    ggtitle("Proportion of Young Adult Court Appearances by Police Force Area") +
#    theme_minimal()
#p7b + coord_flip()
#ggsave("moj_results_2007_court_appearances_by_area.png",
#       width = 9, height = 6)
#
## 2017 AGE by POLICE FORCE AREA (to check loop outputs)
#out7b.2017 <- addmargins(wtd.table(df.sub$`Police Force Area`[df.sub$`Year of Appearance` == 2017], 
#                                   df.sub$age[df.sub$`Year of Appearance` == 2017],
#                                   weights = df.sub$Count[df.sub$`Year of Appearance` == 2017]))
#out7b.2017
#
## Young adult proportions in 2017
#out7b.young.2017 <- out7b.2017[, 2] /  out7b.2017[, 4] 
#out7b.young.2017
#
## Young adult change over time
#out7b.young.2017 <- out7b.young.2017[-34]  # Remove misc PFA
#out7b.young.2017 <- out7b.young.2017[-44]  # Remove misc PFA
#
#out7b.young.change <- out7b.young.2017 - out7b.young.2007
#out7b.young.change
#rank.7b <- order(out7b.young.change)
#rank.7b2 <- rev(rank.7b)
#out7b.young.rank <- out7b.young.change[rank.7b2]
#out7b.young.rank
#
#out7b.df <- as.data.frame(out7b.young.rank)
#out7b.df$Area <- rownames(out7b.df) 
#names(out7b.df) <- c("Change", "Area")
#rownames(out7b.df) <- NULL
#out7b.df
#
#barplot(out7b.df$Change, names = out7b.df$Area, las = 2)
#
#### PLOT 11-year Change
#p7c <- ggplot(data = out7b.df, aes(x = Area, y = Change)) +
#    geom_bar(stat = "identity", fill = "steelblue") +
#    scale_x_discrete(limits = out7b.df$Area) +
#    ylab("Change in Proportion (2017 minus 2007)") +
#    xlab("") +
#    ggtitle("Change in Young Adult Court Appearances by Polce Force Area") +
#    theme_minimal()
#p7c + coord_flip()
#ggsave("moj_results_10_year_change_in_court_appearances_by_area.png",
#       width = 9, height = 6)
#
#
#### PLOT 2017 Proportions
#out7b.young.rank.2017 <- out7b.young.2017[rank.7b.2007] # Use 2007 rank
#out7b.young.rank.2017
#
#out7b.2017.df <- as.data.frame(out7b.young.rank.2017)
#out7b.2017.df$Area <- rownames(out7b.2017.df) 
#rownames(out7b.2017.df) <- NULL
#names(out7b.2017.df) <- c("Proportion", "Area")
#
#p7d <- ggplot() +
#    geom_point(data = out7b.2007.df, aes(x = Area, y = Proportion, 
#                                         color = "steelblue")) +
#    geom_point(data = out7b.2017.df, aes(x = Area, y = Proportion, 
#                                         color = "darkred")) +
#    geom_segment(data = merge(out7b.2007.df, out7b.2017.df, by = "Area"),
#                 aes(x = Area, 
#                     xend = Area,
#                     y = Proportion.x, 
#                     yend = Proportion.y)) +
#    scale_x_discrete(limits = out7b.2007.df$Area) +
#    scale_color_manual(labels = c("2017", "2007"), 
#                       values = c("darkred", "steelblue")) +
#    ylim(c(0, .35)) +
#    ylab("Proportion of All Court Appearances (in 2007 and 2017)") +
#    xlab("") +
#    labs(color = "Year") +
#    ggtitle("Comparison of Young Adult Court Appearances by Police Force Area") +
#    theme_minimal()
#p7d + coord_flip()
#
#ggsave("moj_results_2007_2017_court_appearances_by_area.png",
#       width = 9, height = 6)
#
######################################################################
#######  ALL APPEARANCES AGAINST POPULATION TOTALS (ONS) BY POLICE FORCE AREAS PLOT 
######################################################################
## Load NOMIS population data by LA (England, Scotland, and Wales)
#pop2 <- read.csv("nomis_2019_01_24_080753_LA_Population_Estimates_18-24_year_olds.csv", 
#                 stringsAsFactors = FALSE)
## Load LA to PFA mappings
#la.pfa <- read.csv("LA_to_PFA_codes.csv", stringsAsFactors = FALSE)
#
## Make sure merging by column names match
#names(la.pfa) <- c("LAD16CD", "LA", "PFA16CD", "PFA")
#
## Merge the data
#pop.la <- merge(pop2, la.pfa, by = "LA")
#pop.la$LA[pop.la$LA == "London, City of"] <- "City of London"
#
## Aggregate LA population totals by PFAs
#pop.pfa.2007 <- aggregate(X2007 ~ PFA, sum, data = pop.la)
#pop.pfa.2017 <- aggregate(X2017 ~ PFA, sum, data = pop.la)
#
## Check that PFAs match in each dataset
#rownames(out7b.2007[1:43, ]) == pop.pfa.2007[, 1]
#rownames(out7b.2017[1:43, ]) == pop.pfa.2017[, 1]  
#out7b.2017 <- out7b.2017[-34, ] # Don't match, remove 'misc'
#rownames(out7b.2017[1:43, ]) == pop.pfa.2017[, 1]
#
## Merge PFA population data with court appearances
#out7e.2007 <- cbind("Raw" = out7b.2007[1:43, 2], 
#                    "Population" = pop.pfa.2007[, 2])
#out7e.2007
#out7e.2007 <- out7e.2007[-5, ]   # Remove extreme outlier (City of London)
#out7e.2007 <- as.data.frame(out7e.2007)
#
#out7e.2017 <- cbind("Raw" = out7b.2017[1:43, 2], 
#                    "Population" = pop.pfa.2017[, 2])
#out7e.2017
#out7e.2017 <- out7e.2017[-5, ]   # Remove extreme outlier (City of London)
#out7e.2017 <- as.data.frame(out7e.2017)
#
## Calculate rate per thousan by PFA
#out7e.2007$Rate <- out7e.2007$Raw/out7e.2007$Population * 1000  # Number per 1,0000
#out7e.2007$Area <- rownames(out7e.2007)
#summary(out7e.2007$Rate)
#write.csv(out7e.2007, "moj_results_ALL_APPEARANCES_BY_AREA_RATE_PER_THOUSAND_2007.csv")
#
#out7e.2017$Rate <- out7e.2017$Raw/out7e.2017$Population * 1000  # Number per 1,0000
#summary(out7e.2017$Rate)
#out7e.2017$Area <- rownames(out7e.2017)
#write.csv(out7e.2017, "moj_results_ALL_APPEARANCES_BY_AREA_RATE_PER_THOUSAND_2017.csv")
#
## PLOT 2007 and 2017 RATES
#rank.7e <- order(out7e.2007$Rate)
#out7e.2007 <- out7e.2007[rank.7e, ]
#out7e.2007
#
#p7e <- ggplot() +
#    geom_point(data = out7e.2007, aes(x = Area, y = Rate, 
#                                      color = "steelblue")) +
#    geom_point(data = out7e.2017, aes(x = Area, y = Rate, 
#                                      color = "darkred")) +
#    geom_segment(data = merge(out7e.2007, out7e.2017, by = "Area"),
#                 aes(x = Area, 
#                     xend = Area,
#                     y = Rate.x, 
#                     yend = Rate.y)) +
#    scale_x_discrete(limits = out7e.2007$Area) +
#    scale_color_manual(labels = c("2017", "2007"), 
#                       values = c("darkred", "steelblue")) +
#    ylim(c(0, 45)) +
#    ylab("Rate (Per Thousand) of All Court Appearances (in 2007 and 2017)") +
#    xlab("") +
#    labs(color = "Year") +
#    ggtitle("Comparison of Young Adult Court Appearances by Police Force Area") +
#    theme_minimal()
#p7e + coord_flip()
#
#ggsave("moj_results_2007_2017_RATE_court_appearances_by_area.png",
#       width = 9, height = 6)
#
#
######################################################################
#######  REGRESSION MODELS ###########################################
######################################################################
model <- vector(mode = "list", 13)  # Store subsetted dataframes in list 
output <- data.frame("term" = NA, "estimate" = NA, "std.error" = NA, 
                     "statistic" = NA, "p.value" = NA, "OR" = NA, "Year" = NA)
year <- seq(from = 2007, to = 2019, by = 1)
for (j in 1:length(year)) {
    df <- dframe[[j]]
    indiv.data <- df[ rep(1:nrow(df), df[["Count"]]), 
                      c("Police Force Area", "Year of Appearance", "Quarter",
                        "Type of Defendant", "Sex", "Age Group",
                        "Age Range", "Ethnicity", "Court Type",
                        "Offence Type", "Offence Group", "offence",
                        "Convicted/Not Convicted", "Sentenced/Not Sentenced", 
                        "Outcome", "Detailed Sentence", 
                        "Custodial Sentence Length", 
                        "Count", "age", "youth", "young.adult",
                        "adult", "crown.court", "mag.court", 
                        "convicted", "sentenced", "outcome.new",
                        "male", "female", "sex.unknown", 
                        "white", "black", "asian", 
                        "mixed", "chinese", "ethn.unknown"  ) ]
    model[[j]] <- glm(convicted ~ female + sex.unknown + youth + young.adult + 
                          black + asian + mixed + chinese + ethn.unknown + 
                          crown.court, 
                      family = binomial(link = "logit"),
                      data = indiv.data )
    
    cat("\n", "Year", year[[j]], "\n")
    print(model[[j]])
    model[[j]] <- tidy(model[[j]])
    model[[j]][["OR"]] <- exp(model[[j]][["estimate"]])
    model[[j]][["Year"]] <- year[[j]]
    output <- rbind(output, model[[j]])
}

output <- output[-1, ]
write.csv(output, "moj_regression_rsults.csv")

#############################################################################
#######  MALES: ALL APPEARANCES AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT 
#############################################################################
## Subset the data by MALE
df.sub.m <- subset(df.sub, male == 1)
## Loop to subset data by year 
dframe.m <- vector(mode = "list", 13)  # Store subsetted dataframes in list 
for (j in 1:length(year)) {
    dframe.m[[j]] <- df.sub.m[df.sub.m$`Year of Appearance` == year[j], ]
}

## Loop to print results by year using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe.m[[i]][["age"]],
                        weights = dframe.m[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe.m[[i]][["age"]], 
                                           weights = dframe.m[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub.m$age[df.sub.m$`Year of Appearance` == 2017], 
                     weights = df.sub.m$Count[df.sub.m$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub.m$age[df.sub.m$`Year of Appearance` == 2017], 
                     weights = df.sub.m$Count[df.sub.m$`Year of Appearance` == 2017]))

## Print AGE by YEAR
out1.m <- magic_result_as_dataframe()  # Store results as vector
out1.m <- out1.m[, -1]
out1.m <- cbind(year.lab.1, age.lab.1, out1.m) 
names(out1.m) <- c("Year", "Age", "Raw", "Percent")
out1.m
write.csv(out1.m, "moj_results_all_appearances_by_MALE_by_year.csv")
magic_free()

## Plot the AGE trend lines
p1.m <- ggplot(data = out1.m, 
               aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                   y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ylim(0, 80) +
    ggtitle("Male Court Appearances by Age Group")
p1.m
ggsave("moj_results_all_appearances_by_MALE_by_year.png", dpi = 320, 
       scale = 1.5)

## Now ADD IN RATES for MALES
pop.m <- read.csv("_Updated_Data_Analysis_2009_2019/nomis_2019_04_05_181504_Age_by_MALE_Populations_UPDATED.csv")
out.pop.m <- out1.m
out.pop.m <- out.pop.m[, -4]
out.pop.m <- cbind(out.pop.m, "Total" = pop[, 3]) 
out.pop.m$Rate <- (out.pop.m$Raw/out.pop.m$Total) * 1000  # Number per 1,0000
out.pop.m
write.csv(out.pop.m, "moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_MALE_by_year.csv")

## Plot the AGE trend lines
p1.pop.m <- ggplot(data = out.pop.m, 
                   aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                       y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylab("Rate (per thousand individuals)") +
    ylim(0, 35) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ggtitle("Male Court Appearance Rate by Age Group")
p1.pop.m
ggsave("moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_MALE_by_year.png", dpi = 320, 
       scale = 1.5)


###############################################################################
#######  FEMALES: ALL APPEARANCES AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT 
###############################################################################
## Subset the data by FEMALE
df.sub.f <- subset(df.sub, female == 1)
## Loop to subset data by year 
dframe.f <- vector(mode = "list", 11)  # Store subsetted dataframes in list 
for (j in 1:length(year)) {
    dframe.f[[j]] <- df.sub.f[df.sub.f$`Year of Appearance` == year[j], ]
}

## Loop to print results by year using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year)) {
    put(
        Raw = wtd.table(dframe.f[[i]][["age"]],
                        weights = dframe.f[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe.f[[i]][["age"]], 
                                           weights = dframe.f[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub.f$age[df.sub.f$`Year of Appearance` == 2017], 
                     weights = df.sub.f$Count[df.sub.f$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub.f$age[df.sub.f$`Year of Appearance` == 2017], 
                     weights = df.sub.f$Count[df.sub.f$`Year of Appearance` == 2017]))

## Print AGE by YEAR
out1.f <- magic_result_as_dataframe()  # Store results as vector
out1.f <- out1.f[, -1]
out1.f <- cbind(year.lab.1, age.lab.1, out1.f) 
names(out1.f) <- c("Year", "Age", "Raw", "Percent")
out1.f
write.csv(out1.f, "moj_results_all_appearances_by_FEMALE_by_year.csv")
magic_free()

## Plot the AGE trend lines
p1.f <- ggplot(data = out1.f, 
               aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                   y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ylim(0, 85) +
    ggtitle("Female Court Appearances by Age Group")
p1.f
ggsave("moj_results_all_appearances_by_FEMALE_by_year.png", dpi = 320, 
       scale = 1.5)

## Now ADD IN RATES for FEMALES
pop.f <- read.csv("_Updated_Data_Analysis_2009_2019/nomis_2019_04_05_181504_Age_by_FEMALE_Populations_UPDATED.csv")
out.pop.f <- out1.f
out.pop.f <- out.pop.f[, -4]
out.pop.f <- cbind(out.pop.f, "Total" = pop[, 3]) 
out.pop.f$Rate <- (out.pop.f$Raw/out.pop.f$Total) * 1000  # Number per 1,0000
out.pop.f
write.csv(out.pop.f, "moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_FEMALE_by_year.csv")

## Plot the AGE trend lines
p1.pop.f <- ggplot(data = out.pop.f, 
                   aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                       y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    ylab("Rate (per thousand individuals)") +
    ylim(0, 4) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ggtitle("Female Court Appearance Rate by Age Group")
p1.pop.f
ggsave("moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_FEMALE_by_year.png", dpi = 320, 
       scale = 1.5)


################################################################################
#######  WHITES: ALL APPEARANCES AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT 
################################################################################
## No ethnicity data for 2007 and 2008
year.w <- seq(from = 2009, to = 2019, by = 1)
## No population estimates for Youth (under 16 not available in the APS)
year.lab.w <- rep(year.w, times = 1, each = 2)
age.lab.w <- rep(c("Young Adult", "Adult"), times = 11)
## Subset the data by WHITE AND REMOVE YOUTH
df.sub.w <- subset(df.sub, Ethnicity == "01: White" & `Age Group` != "01: Juveniles")
## Loop to subset data by year 
dframe.w <- vector(mode = "list", 11)  # Store subsetted dataframes in list 
for (j in 1:length(year.w)) {
    dframe.w[[j]] <- df.sub.w[df.sub.w$`Year of Appearance` == year.w[j], ]
}

## Loop to print results by year.w using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year.w)) {
    put(
        Raw = wtd.table(dframe.w[[i]][["age"]],
                        weights = dframe.w[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe.w[[i]][["age"]], 
                                           weights = dframe.w[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub.w$age[df.sub.w$`Year of Appearance` == 2017], 
                     weights = df.sub.w$Count[df.sub.w$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub.w$age[df.sub.w$`Year of Appearance` == 2017], 
                     weights = df.sub.w$Count[df.sub.w$`Year of Appearance` == 2017]))

## Print AGE by YEAR
out1.w <- magic_result_as_dataframe()  # Store results as vector
out1.w <- out1.w[, -1]
out1.w <- cbind(year.lab.w, age.lab.w, out1.w) 
names(out1.w) <- c("Year", "Age", "Raw", "Percent")
out1.w
write.csv(out1.w, "moj_results_all_appearances_by_WHITE_by_year.csv")
magic_free()

## Plot the AGE trend lines
p1.w <- ggplot(data = out1.w, 
               aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                   y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ylim(0, 85) +
    ggtitle("Court Appearances (Whites) by Age Group")
p1.w
ggsave("moj_results_all_appearances_by_WHITE_by_year.png", dpi = 320, 
       scale = 1.5)

## Now ADD IN RATES for WHITES
pop.w <- read.csv("_Updated_Data_Analysis_2009_2019/nomis_2019_05_01_194620_WHIITE_UPDATED.csv")
## Remove 2007 and 2008
pop.w <- pop.w[5:26, ]
out.pop.w <- out1.w
out.pop.w <- out.pop.w[, -4]
out.pop.w <- cbind(out.pop.w, "Total" = pop.w[, 3]) 
out.pop.w$Rate <- (out.pop.w$Raw/out.pop.w$Total) * 1000  # Number per 1,0000
out.pop.w
write.csv(out.pop.w, "moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_WHITE_by_year.csv")

## Plot the AGE trend lines
p1.pop.w <- ggplot(data = out.pop.w, 
                   aes(x = factor(Year, levels = seq(from = 2009, to = 2019, by = 1)), 
                       y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    ylab("Rate (per thousand individuals)") +
    ylim(0, 30) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ggtitle("Court Appearance Rate (Whites) by Age Group")
p1.pop.w
ggsave("moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_WHITE_by_year.png", dpi = 320, 
       scale = 1.5)


################################################################################
#######  NONWHITES: ALL APPEARANCES AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT 
################################################################################
## No ethnicity data for 2007 and 2008
year.nw <- seq(from = 2009, to = 2019, by = 1)
## No population estimates for Youth (under 16 not available in the APS)
year.lab.nw <- rep(year.nw, times = 1, each = 2)
age.lab.nw <- rep(c("Young Adult", "Adult"), times = 11)
## Subset the data by NONWHITE AND REMOVE YOUTH
df.sub.nw <- subset(df.sub, (Ethnicity == "02: Black" | 
                                 Ethnicity == "03: Asian" | 
                                 Ethnicity == "04: Mixed" |
                                 Ethnicity == "05: Chinese and other") & 
                        `Age Group` != "01: Juveniles")
## Loop to subset data by year 
dframe.nw <- vector(mode = "list", 11)  # Store subsetted dataframes in list 
for (j in 1:length(year.nw)) {
    dframe.nw[[j]] <- df.sub.nw[df.sub.nw$`Year of Appearance` == year.nw[j], ]
}

## Loop to print results by year.nw using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year.nw)) {
    put(
        Raw = wtd.table(dframe.nw[[i]][["age"]],
                        weights = dframe.nw[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe.nw[[i]][["age"]], 
                                           weights = dframe.nw[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub.nw$age[df.sub.nw$`Year of Appearance` == 2017], 
                     weights = df.sub.nw$Count[df.sub.nw$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub.nw$age[df.sub.nw$`Year of Appearance` == 2017], 
                     weights = df.sub.nw$Count[df.sub.nw$`Year of Appearance` == 2017]))

## Print AGE by YEAR
out1.nw <- magic_result_as_dataframe()  # Store results as vector
out1.nw <- out1.nw[, -1]
out1.nw <- cbind(year.lab.nw, age.lab.nw, out1.nw) 
names(out1.nw) <- c("Year", "Age", "Raw", "Percent")
out1.nw
write.csv(out1.nw, "moj_results_all_appearances_by_NONWHITE_by_year.csv")
magic_free()

## Plot the AGE trend lines
p1.nw <- ggplot(data = out1.nw, 
                aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                    y = Percent, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ylim(0, 85) +
    ggtitle("Court Appearances (Nonwhites) by Age Group")
p1.nw
ggsave("moj_results_all_appearances_by_NONWHITE_by_year.png", dpi = 320, 
       scale = 1.5)

## Now ADD IN RATES for NONWHITES
pop.nw <- read.csv("_Updated_Data_Analysis_2009_2019/nomis_2019_05_01_194620_NONWHIITE_UPDATED.csv")
## Remove 2007 and 2008
pop.nw <- pop.nw[5:26, ]
out.pop.nw <- out1.nw
out.pop.nw <- out.pop.nw[, -4]
out.pop.nw <- cbind(out.pop.nw, "Total" = pop.nw[, 3]) 
out.pop.nw$Rate <- (out.pop.nw$Raw/out.pop.nw$Total) * 1000  # Number per 1,0000
out.pop.nw
write.csv(out.pop.nw, "moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_NONWHITE_by_year.csv")

## Plot the AGE trend lines
p1.pop.nw <- ggplot(data = out.pop.nw, 
                    aes(x = factor(Year, levels = seq(from = 2009, to = 2019, by = 1)), 
                        y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    xlab("Year") +
    scale_x_discrete(breaks = c("2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    ylab("Rate (per thousand individuals)") +
    ylim(0, 45) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
    ggtitle("Court Appearance Rate (Nonwhites) by Age Group")
p1.pop.nw
ggsave("moj_results_ALL_APPEARANCES_RATE_PER_THOUSAND_by_NONWHITE_by_year.png", dpi = 320, 
       scale = 1.5)


###########################################################################################
#######  WHITES: IMMEDIATE CUSTODY AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  ##########
###########################################################################################
## Using data from previous white/nonwhite subsets (but only for immediate custody)
dframe.w2 <- vector(mode = "list", 11)  # Store subsetted dataframes in list 
for (j in 1:length(year.w)) {
    dframe.w2[[j]] <- subset(dframe.w[[j]], Outcome == "15: Immediate custody")
}

## Loop to print results by year.w using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year.w)) {
    put(
        Raw = wtd.table(dframe.w2[[i]][["age"]],
                        dframe.w2[[i]][["Outcome"]], 
                        weights = dframe.w2[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe.w2[[i]][["age"]], 
                                           dframe.w2[[i]][["Outcome"]], 
                                           weights = dframe.w2[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub.w$age[df.sub.w$`Year of Appearance` == 2017], 
                     df.sub.w$`Outcome`[df.sub.w$`Year of Appearance` == 2017],
                     weights = df.sub.w$Count[df.sub.w$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub.w$age[df.sub.w$`Year of Appearance` == 2017], 
                     df.sub.w$`Outcome`[df.sub.w$`Year of Appearance` == 2017],
                     weights = df.sub.w$Count[df.sub.w$`Year of Appearance` == 2017]))

## Print Immediate Custody by AGE and YEAR
out4.w <- magic_result_as_dataframe()  # Store results as dataframe
out4.w <- out4.w[, -1]
sentenced.lab.w <- rep(c("Immediate Custody"), times = 22, each = 1)
year.lab.2.w <- rep(year.w, times = 1, each = 2)
age.lab.2.w <- rep(c("Young Adult", "Adult"), times = 11)
out4.w <- cbind(year.lab.2.w, age.lab.2.w, sentenced.lab.w, out4.w)  # Add labels
names(out4.w) <- c("Year", "Age", "Outcome", "Raw", "Percent")
out4.w <- out4.w[, -5]
out4.w$Outcome <- factor(out4.w$Outcome, levels = c("Immediate Custody"), 
                         ordered = FALSE)
out4.w  # Print output
write.csv(out4.w, "moj_results_IMMEDIATE_CUSTODY_by_age_year_WHITES.csv")
magic_free()

## Now ADD IN RATES for WHITES
out4.pop.w <- out4.w
out4.pop.w <- cbind(out4.pop.w, "Total" = pop.w[, 3]) 
out4.pop.w$Rate <- (out4.pop.w$Raw/out4.pop.w$Total) * 1000  # Number per 1,0000
out4.pop.w
write.csv(out4.pop.w, "moj_results_IMMEDIATE_CUSTODY_RATE_PER_THOUSAND_by_WHITE_by_year.csv")

## Plot the OUTCOME by AGE trend lines
p4.pop.w <- ggplot(data = out4.pop.w, 
                   aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                       y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    ylim(c(0, 7)) +
    ylab("Rate (per thousand)") +
    xlab("Year") +
    ggtitle("Immediate Custody Rate (Whites) by Age Group") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))

p4.pop.w + facet_wrap(vars(Outcome), nrow = 1)
ggsave("moj_results_IMMEDIATE_CUSTODY_RATE_PER_THOUSAND_WHITES_age_year.png", dpi = 320, 
       scale = 1.5)


##############################################################################################
#######  NONWHITES: IMMEDIATE CUSTODY AGAINST POPULATION TOTALS (ONS) BY YEAR PLOT  ##########
##############################################################################################
## Using data from previous white/nonwhite subsets (but only for immediate custody)
dframe.nw2 <- vector(mode = "list", 11)  # Store subsetted dataframes in list 
for (j in 1:length(year.nw)) {
    dframe.nw2[[j]] <- subset(dframe.nw[[j]], Outcome == "15: Immediate custody")
}

## Loop to print results by year.nw using 'magicfor' package
magic_for(silent = TRUE)
for (i in 1:length(year.nw)) {
    put(
        Raw = wtd.table(dframe.nw2[[i]][["age"]],
                        dframe.nw2[[i]][["Outcome"]], 
                        weights = dframe.nw2[[i]][["Count"]]),  # Raw count of age
        Percent = 100*prop.table(wtd.table(dframe.nw2[[i]][["age"]], 
                                           dframe.nw2[[i]][["Outcome"]], 
                                           weights = dframe.nw2[[i]][["Count"]]))  # Percentage age
    )
}

## 2017 AGE by YEAR (to check loop outputs)
addmargins(wtd.table(df.sub.nw$age[df.sub.nw$`Year of Appearance` == 2017], 
                     df.sub.nw$`Outcome`[df.sub.nw$`Year of Appearance` == 2017],
                     weights = df.sub.nw$Count[df.sub.nw$`Year of Appearance` == 2017]))
prop.table(wtd.table(df.sub.nw$age[df.sub.nw$`Year of Appearance` == 2017], 
                     df.sub.nw$`Outcome`[df.sub.nw$`Year of Appearance` == 2017],
                     weights = df.sub.nw$Count[df.sub.nw$`Year of Appearance` == 2017]))

## Print Immediate Custody by AGE and YEAR
out4.nw <- magic_result_as_dataframe()  # Store results as dataframe
out4.nw <- out4.nw[, -1]
sentenced.lab.nw <- rep(c("Immediate Custody"), times = 22, each = 1)
year.lab.2.nw <- rep(year.nw, times = 1, each = 2)
age.lab.2.nw <- rep(c("Young Adult", "Adult"), times = 11)
out4.nw <- cbind(year.lab.2.nw, age.lab.2.nw, sentenced.lab.nw, out4.nw)  # Add labels
names(out4.nw) <- c("Year", "Age", "Outcome", "Raw", "Percent")
out4.nw <- out4.nw[, -5]
out4.nw$Outcome <- factor(out4.nw$Outcome, levels = c("Immediate Custody"), 
                          ordered = FALSE)
out4.nw  # Print output
write.csv(out4.nw, "moj_results_IMMEDIATE_CUSTODY_by_age_year_NONWHITES.csv")
magic_free()

## Now ADD IN RATES for NONWHITES
out4.pop.nw <- out4.nw
out4.pop.nw <- cbind(out4.pop.nw, "Total" = pop.nw[, 3]) 
out4.pop.nw$Rate <- (out4.pop.nw$Raw/out4.pop.nw$Total) * 1000  # Number per 1,0000
out4.pop.nw
write.csv(out4.pop.nw, "moj_results_IMMEDIATE_CUSTODY_RATE_PER_THOUSAND_by_NONWHITE_by_year.csv")

## Plot the OUTCOME by AGE trend lines
p4.pop.nw <- ggplot(data = out4.pop.nw, 
                    aes(x = factor(Year, levels = seq(from = 2007, to = 2019, by = 1)), 
                        y = Rate, group = Age, color = Age)) + 
    geom_point() +
    geom_line() + 
    ylim(c(0, 7)) +
    ylab("Rate (per thousand)") +
    xlab("Year") +
    ggtitle("Immediate Custody Rate (Nonwhites) by Age Group") +
    scale_x_discrete(breaks = c("2007","2008","2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                     labels = c("07","08","09", "10", "11", "12",
                                "13", "14", "15", "16", "17", "18", "19")) +
    scale_color_manual(values = c("steelblue", "darkred", "darkgreen"))

p4.pop.nw + facet_wrap(vars(Outcome), nrow = 1)
ggsave("moj_results_IMMEDIATE_CUSTODY_RATE_PER_THOUSAND_NONWHITES_age_year.png", dpi = 320, 
       scale = 1.5)
