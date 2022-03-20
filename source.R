library("tidyverse")
library("patchwork")
library("car")

setwd("/Users/abrarnasir/Desktop/School/STA302/Video Project/STA302_VideoProject")
df <- read.csv("VPdataset.csv")

# df %>% drop_na()
df <- transform(df, STATEID = as.numeric(factor(STABBR)))
df <- df[, !(names(df) %in% c("UNITID", "INSTNM", "STABBR"))]
df <- df %>% relocate(STATEID)
df <- df %>% relocate(ADM_RATE)

full0 <- lm(ADM_RATE ~., data = df)

plt1 <- ggplot(df, aes(STATEID, ADM_RATE)) + geom_point() + ggtitle("States") + geom_smooth(method = 'lm', se = FALSE)
plt2 <- ggplot(df, aes(NUMBRANCH, ADM_RATE)) + geom_point() + ggtitle("Number of Branches") + geom_smooth(method = 'lm', se = FALSE)
plt3 <- ggplot(df, aes(CONTROL, ADM_RATE)) + geom_point() + ggtitle("Control") + geom_smooth(method = 'lm', se = FALSE)
plt4 <- ggplot(df, aes(HBCU, ADM_RATE)) + geom_point() + ggtitle("Historically Black") + geom_smooth(method = 'lm', se = FALSE)
plt5 <- ggplot(df, aes(PBI, ADM_RATE)) + geom_point() + ggtitle("Predominantly Black") + geom_smooth(method = 'lm', se = FALSE)
plt6 <- ggplot(df, aes(TRIBAL, ADM_RATE)) + geom_point() + ggtitle("Tribal") + geom_smooth(method = 'lm', se = FALSE)
plt7 <- ggplot(df, aes(HSI, ADM_RATE)) + geom_point() + ggtitle("Hispanic Serving") + geom_smooth(method = 'lm', se = FALSE)
plt8 <- ggplot(df, aes(WOMENONLY, ADM_RATE)) + geom_point() + ggtitle("Women Only") + geom_smooth(method = 'lm', se = FALSE)
plt9 <- ggplot(df, aes(COSTT4_A, ADM_RATE)) + geom_point() + ggtitle("Average Cost of Attendance") + geom_smooth(method = 'lm', se = FALSE)
plt10 <- ggplot(df, aes(AVGFACSAL, ADM_RATE)) + geom_point() + ggtitle("Average Faculty Salary") + geom_smooth(method = 'lm', se = FALSE)
plt11 <- ggplot(df, aes(PFTFAC, ADM_RATE)) + geom_point() + ggtitle("Proportion of Full-Time Faculty Members") + geom_smooth(method = 'lm', se = FALSE)
plt12 <- ggplot(df, aes(REGION, ADM_RATE)) + geom_point() + ggtitle("Regions") + geom_smooth(method = 'lm', se = FALSE)
plt1 + plt2 + plt3 + plt4 + plt5 + plt6 + plt7 + plt8 + plt9 + plt10 + plt11 + plt12 + plot_layout(nrow = 4, byrow = FALSE) + plot_annotation(title = 'Scatterplots of Predictor Variables (Institution Characteristics) against Admission Rate')

plt1 <- ggplot(df, aes(PCTPELL, ADM_RATE)) + geom_point() + ggtitle("Percentage of Undergraduates \n Receiving Pell Grant") + geom_smooth(method = 'lm', se = FALSE)
plt2 <- ggplot(df, aes(UG25ABV, ADM_RATE)) + geom_point() + ggtitle("Percentage of Undergraduates \n Aged 25+") + geom_smooth(method = 'lm', se = FALSE)
plt3 <- ggplot(df, aes(INC_PCT_LO, ADM_RATE)) + geom_point() + ggtitle("Percentage of Aided Students whose \n Family Income is between $0-$30,000") + geom_smooth(method = 'lm', se = FALSE)
plt4 <- ggplot(df, aes(PAR_ED_PCT_1STGEN, ADM_RATE)) + geom_point() + ggtitle("Percentage of \n First-Generation Students") + geom_smooth(method = 'lm', se = FALSE)
plt5 <- ggplot(df, aes(FEMALE, ADM_RATE)) + geom_point() + ggtitle("Proportion of Student Body \n that is Female") + geom_smooth(method = 'lm', se = FALSE)
plt6 <- ggplot(df, aes(MD_FAMINC, ADM_RATE)) + geom_point() + ggtitle("Median Family Income of Students") + geom_smooth(method = 'lm', se = FALSE)
plt7 <- ggplot(df, aes(PCT_WHITE, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is White") + geom_smooth(method = 'lm', se = FALSE)
plt8 <- ggplot(df, aes(PCT_BLACK, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Black") + geom_smooth(method = 'lm', se = FALSE)
plt9 <- ggplot(df, aes(PCT_ASIAN, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Asian") + geom_smooth(method = 'lm', se = FALSE)
plt10 <- ggplot(df, aes(PCT_HISPANIC, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Hispanic") + geom_smooth(method = 'lm', se = FALSE)
plt11 <- ggplot(df, aes(PCT_BA, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Bachelor's Degree over the Age 25") + geom_smooth(method = 'lm', se = FALSE)
plt12 <- ggplot(df, aes(PCT_GRAD_PROF, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Professional Degree over the Age 25") + geom_smooth(method = 'lm', se = FALSE)
plt13 <- ggplot(df, aes(PCT_BORN_US, ADM_RATE)) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes that was Born in the US") + geom_smooth(method = 'lm', se = FALSE)
plt14 <- ggplot(df, aes(POVERTY_RATE, ADM_RATE)) + geom_point() + ggtitle("Poverty Rate") + geom_smooth(method = 'lm', se = FALSE)
plt15 <- ggplot(df, aes(UNEMP_RATE, ADM_RATE)) + geom_point() + ggtitle("Unemployment Rate") + geom_smooth(method = 'lm', se = FALSE)
plt1 + plt2 + plt3 + plt4 + plt5 + plt6 + plt7 + plt8 + plt9 + plt10 + plt11 + plt12 + plt13 + plt14 + plt15  + plot_layout(nrow = 5, byrow = FALSE) + plot_annotation(title = 'Scatterplots of Predictor Variables (Applicant Characteristics) against Admission Rate')

hist1 <- ggplot(df, aes(x = ADM_RATE)) + geom_histogram() + ggtitle("Admission Rate")
hist2 <- ggplot(df, aes(x = STATEID)) + geom_histogram() + ggtitle("States")
hist3 <- ggplot(df, aes(x = REGION)) + geom_histogram() + ggtitle("Regions")
hist4 <- ggplot(df, aes(x = NUMBRANCH)) + geom_histogram() + ggtitle("Number of Branches") 
hist5 <- ggplot(df, aes(x = CONTROL)) + geom_histogram() + ggtitle("Control") 
hist6 <- ggplot(df, aes(x = HBCU)) + geom_histogram() + ggtitle("Historically Black")
hist7 <- ggplot(df, aes(x = PBI)) + geom_histogram() + ggtitle("Predominantly Black")
hist8 <- ggplot(df, aes(x = TRIBAL)) + geom_histogram() + ggtitle("Tribal")
hist9 <- ggplot(df, aes(x = HSI)) + geom_histogram() + ggtitle("Hispanic Serving")
hist10 <- ggplot(df, aes(x = WOMENONLY)) + geom_histogram() + ggtitle("Women Only")
hist11 <- ggplot(df, aes(x = COSTT4_A)) + geom_histogram() + ggtitle("Average Cost of Attendance")
hist12 <- ggplot(df, aes(x = AVGFACSAL)) + geom_histogram() + ggtitle("Average Faculty Salary")
hist13 <- ggplot(df, aes(x = PFTFAC)) + geom_histogram() + ggtitle("Proportion of Full-Time Faculty Members")
hist14 <- ggplot(df, aes(x = PCTPELL)) + geom_histogram() + ggtitle("Percentage of Undergraduates \n Receiving Pell Grant")
hist15 <- ggplot(df, aes(x = UG25ABV)) + geom_histogram() + ggtitle("Percentage of Undergraduates \n Aged 25+")
hist16 <- ggplot(df, aes(x = INC_PCT_LO)) + geom_histogram() + ggtitle("Percentage of Aided Students whose \n Family Income is between $0-$30,000")
hist17 <- ggplot(df, aes(x = PAR_ED_PCT_1STGEN)) + geom_histogram() + ggtitle("Percentage of \n First-Generation Students")
hist18 <- ggplot(df, aes(x = FEMALE)) + geom_histogram() + ggtitle("Proportion of Student Body \n that is Female")
hist19 <- ggplot(df, aes(x = PCT_WHITE)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is White")
hist20 <- ggplot(df, aes(x = PCT_BLACK)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Black")
hist21 <- ggplot(df, aes(x = PCT_ASIAN)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Asian")
hist22 <- ggplot(df, aes(x = PCT_HISPANIC)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Hispanic")
hist23 <- ggplot(df, aes(x = PCT_BA)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Bachelor's Degree over the Age 25")
hist24 <- ggplot(df, aes(x = PCT_GRAD_PROF)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Professional Degree over the Age 25")
hist25 <- ggplot(df, aes(x = PCT_BORN_US)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that was Born in the US")
hist26 <- ggplot(df, aes(x = POVERTY_RATE)) + geom_histogram() + ggtitle("Poverty Rate")
hist27 <- ggplot(df, aes(x = UNEMP_RATE)) + geom_histogram() + ggtitle("Unemployment Rate")
hist28 <- ggplot(df, aes(x = MD_FAMINC)) + geom_histogram() + ggtitle("Median Family Income of Students")
hist1 + hist2 + hist3 + hist4 + hist5 + hist6 + hist7 + hist8 + hist9 + hist10 + hist11 + hist12 + hist13 + hist14 + hist15 + hist16 + hist17 + hist18 + hist19 + hist20 + hist21 + hist22 + hist23 + hist24 + hist25 + hist26 + hist27 + hist28 + plot_layout(nrow = 7, byrow = FALSE) + plot_annotation(title = 'Histograms of Variables')

full <- lm(ADM_RATE ~ PCTPELL + PAR_ED_PCT_1STGEN + FEMALE + MD_FAMINC + PCT_BA + INC_PCT_LO + STATEID + COSTT4_A + AVGFACSAL + PFTFAC, data = df)
summary(full)
pairs(~ PCTPELL + PAR_ED_PCT_1STGEN + FEMALE + MD_FAMINC + PCT_BA + INC_PCT_LO + STATEID + COSTT4_A + AVGFACSAL + PFTFAC, data = df)

mdl1 <- lm(ADM_RATE ~ PCTPELL + FEMALE + PCT_BA + STATEID + COSTT4_A + PFTFAC, data = df)

ggplot(df, aes(fitted(mdl1), ADM_RATE)) + geom_point() + ggtitle("Response versus Fitted") + geom_smooth(method = 'gam', se = FALSE)

pairs(~ PCTPELL + FEMALE + PCT_BA + STATEID + COSTT4_A + PFTFAC, data = df)

resy <- ggplot(df, aes(fitted(mdl1), residuals(mdl1))) + geom_point() + ggtitle("Residuals versus Fitted") + geom_smooth(method = 'lm', se = FALSE)
res1 <- ggplot(df, aes(PCTPELL, residuals(mdl1))) + geom_point() + ggtitle("Residuals versus Percentage of Undergraduates \n Receiving Pell Grant") + geom_smooth(method = 'lm', se = FALSE)
res2 <- ggplot(df, aes(FEMALE, residuals(mdl1))) + geom_point() + ggtitle("Residuals versus Proportion of Student Body \n that is Female") + geom_smooth(method = 'lm', se = FALSE)
res3 <- ggplot(df, aes(PCT_BA, residuals(mdl1))) + geom_point() + ggtitle("Residuals versus Percentage of the Population from \n Students' Zipcodes with a \n Bachelor's Degree over the Age 25") + geom_smooth(method = 'lm', se = FALSE)
res4 <- ggplot(df, aes(STATEID, residuals(mdl1))) + geom_point() + ggtitle("Residuals versus States") + geom_smooth(method = 'lm', se = FALSE)
res5 <- ggplot(df, aes(COSTT4_A, residuals(mdl1))) + geom_point() + ggtitle("Residuals versus Average Cost of Attendance") + geom_smooth(method = 'lm', se = FALSE)
res6 <- ggplot(df, aes(PFTFAC, residuals(mdl1))) + geom_point() + ggtitle("Proportion of Full-Time Faculty Members") + geom_smooth(method = 'lm', se = FALSE)

(resy) /
  (res1 + res2 + res3) /
  (res4 + res5 + res6)

qqnorm(residuals(mdl1))
qqline(residuals(mdl1))

bc <- boxCox(mdl1)
lambda <- bc$x[which.max(bc$y)]

df$transformed_ADM_RATE <- df$ADM_RATE ** 1.5

mdl2 <- lm(transformed_ADM_RATE ~ PCTPELL + FEMALE + PCT_BA + STATEID + COSTT4_A + PFTFAC, data = df)

ggplot(df, aes(fitted(mdl2), transformed_ADM_RATE)) + geom_point() + ggtitle("Response versus Fitted") + geom_smooth(method = 'lm', se = FALSE)

pairs(~ PCTPELL + FEMALE + PCT_BA + STATEID + COSTT4_A + PFTFAC, data = df)

resty <- ggplot(df, aes(fitted(mdl2), residuals(mdl2))) + geom_point() + ggtitle("Residuals versus Fitted") + geom_smooth(method = 'lm', se = FALSE)
rest1 <- ggplot(df, aes(PCTPELL, residuals(mdl2))) + geom_point() + ggtitle("Residuals versus Percentage of Undergraduates \n Receiving Pell Grant") + geom_smooth(method = 'lm', se = FALSE)
rest2 <- ggplot(df, aes(FEMALE, residuals(mdl2))) + geom_point() + ggtitle("Residuals versus Proportion of Student Body \n that is Female") + geom_smooth(method = 'lm', se = FALSE)
rest3 <- ggplot(df, aes(PCT_BA, residuals(mdl2))) + geom_point() + ggtitle("Residuals versus Percentage of the Population from \n Students' Zipcodes with a \n Bachelor's Degree over the Age 25") + geom_smooth(method = 'lm', se = FALSE)
rest4 <- ggplot(df, aes(STATEID, residuals(mdl2))) + geom_point() + ggtitle("Residuals versus States") + geom_smooth(method = 'lm', se = FALSE)
rest5 <- ggplot(df, aes(COSTT4_A, residuals(mdl2))) + geom_point() + ggtitle("Residuals versus Average Cost of Attendance") + geom_smooth(method = 'lm', se = FALSE)
rest6 <- ggplot(df, aes(PFTFAC, residuals(mdl2))) + geom_point() + ggtitle("Proportion of Full-Time Faculty Members") + geom_smooth(method = 'lm', se = FALSE)

(resty) /
  (rest1 + rest2 + rest3) /
  (rest4 + rest5 + rest6)

qqnorm(residuals(mdl2))
qqline(residuals(mdl2))

mdl3 <- lm(ADM_RATE ~ FEMALE + PCT_BA + STATEID + COSTT4_A + PFTFAC, data = df)

summary(full0)
summary(full)
summary(mdl1)
summary(mdl2)
summary(mdl3)



