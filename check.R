library("tidyverse")
library("patchwork")
library("car")

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)

setwd("/Users/abrarnasir/Desktop/School/STA302/STA302_VideoProject")
df <- read.csv("VPdataset.csv")

# df %>% drop_na()
df <- transform(df, STATEID = as.numeric(factor(STABBR)))
df <- df[, !(names(df) %in% c("STABBR"))]
df <- df %>% relocate(STATEID)
df <- df %>% relocate(ADM_RATE)
df <- df %>% relocate(INSTNM)
df <- df %>% relocate(UNITID)

full <- lm(ADM_RATE ~., data = df[, -c(1, 2)])

p <- powerTransform(cbind(df[, -c(1, 2)]) + 0.0001)
summary(p)

ADM_RATE <- df$ADM_RATE ** 1.5
STATEID <- df$STATEID ** 1
NUMBRANCH <- df$NUMBRANCH ** -6
CONTROL <- log(df$CONTROL)
REGION <- df$REGION ** 0.5
# tHBCU <- df$HBCU ** -3
# tPBI <- df$PBI ** -7
# tTRIBAL <- df$TRIBAL ** -70
# tHSI <- df$HSI ** -1
# tWOMENONLY <- df$WOMENONLY ** -18
COSTT4_A <- df$COSTT4_A ** 0.5
AVGFACSAL <- df$AVGFACSAL ** 0.33
PFTFAC <- df$PFTFAC ** 1
PCTPELL <- df$PCTPELL ** 0.5
UG25ABV <- df$UG25ABV ** 0.33
INC_PCT_LO <- df$INC_PCT_LO ** 0.33
PAR_ED_PCT_1STGEN <- df$PAR_ED_PCT_1STGEN ** 1
FEMALE <- df$FEMALE ** 2
MD_FAMINC <- df$MD_FAMINC ** 0.5
PCT_WHITE <- df$PCT_WHITE ** 2.75
PCT_BLACK <- df$PCT_BLACK ** 0.6
PCT_ASIAN <- df$PCT_ASIAN ** 0.1
PCT_HISPANIC <- df$PCT_HISPANIC ** 0.1
PCT_BA <- df$PCT_BA ** 1
PCT_GRAD_PROF <- df$PCT_GRAD_PROF ** 0.33
PCT_BORN_US <- df$PCT_BORN_US ** 10
POVERTY_RATE <- df$POVERTY_RATE ** -0.33
UNEMP_RATE <- df$UNEMP_RATE ** -1

t_df <- data.frame(ADM_RATE, STATEID, NUMBRANCH, CONTROL, REGION, COSTT4_A, AVGFACSAL, PFTFAC, PCTPELL, UG25ABV, INC_PCT_LO, PAR_ED_PCT_1STGEN, FEMALE, MD_FAMINC, PCT_WHITE, PCT_BLACK, PCT_ASIAN, PCT_HISPANIC, PCT_BA, PCT_GRAD_PROF, PCT_BORN_US, POVERTY_RATE, UNEMP_RATE)

model1 <- lm(ADM_RATE ~., data = t_df)

hist1 <- ggplot(t_df, aes(x = ADM_RATE)) + geom_histogram() + ggtitle("Admission Rate")
hist2 <- ggplot(t_df, aes(x = STATEID)) + geom_histogram() + ggtitle("States")
hist3 <- ggplot(t_df, aes(x = REGION)) + geom_histogram() + ggtitle("Regions")
hist4 <- ggplot(t_df, aes(x = NUMBRANCH)) + geom_histogram() + ggtitle("Number of Branches") 
hist5 <- ggplot(t_df, aes(x = CONTROL)) + geom_histogram() + ggtitle("Control") 
hist11 <- ggplot(t_df, aes(x = COSTT4_A)) + geom_histogram() + ggtitle("Average Cost of Attendance")
hist12 <- ggplot(t_df, aes(x = AVGFACSAL)) + geom_histogram() + ggtitle("Average Faculty Salary")
hist13 <- ggplot(t_df, aes(x = PFTFAC)) + geom_histogram() + ggtitle("Proportion of Full-Time Faculty Members")
hist14 <- ggplot(t_df, aes(x = PCTPELL)) + geom_histogram() + ggtitle("Percentage of Undergraduates \n Receiving Pell Grant")
hist15 <- ggplot(t_df, aes(x = UG25ABV)) + geom_histogram() + ggtitle("Percentage of Undergraduates \n Aged 25+")
hist16 <- ggplot(t_df, aes(x = INC_PCT_LO)) + geom_histogram() + ggtitle("Percentage of Aided Students whose \n Family Income is between $0-$30,000")
hist17 <- ggplot(t_df, aes(x = PAR_ED_PCT_1STGEN)) + geom_histogram() + ggtitle("Percentage of \n First-Generation Students")
hist18 <- ggplot(t_df, aes(x = FEMALE)) + geom_histogram() + ggtitle("Proportion of Student Body \n that is Female")
hist19 <- ggplot(t_df, aes(x = PCT_WHITE)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is White")
hist20 <- ggplot(t_df, aes(x = PCT_BLACK)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Black")
hist21 <- ggplot(t_df, aes(x = PCT_ASIAN)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Asian")
hist22 <- ggplot(t_df, aes(x = PCT_HISPANIC)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Hispanic")
hist23 <- ggplot(t_df, aes(x = PCT_BA)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Bachelor's Degree over the Age 25")
hist24 <- ggplot(t_df, aes(x = PCT_GRAD_PROF)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Professional Degree over the Age 25")
hist25 <- ggplot(t_df, aes(x = PCT_BORN_US)) + geom_histogram() + ggtitle("Percentage of the Population from \n Students' Zipcodes that was Born in the US")
hist26 <- ggplot(t_df, aes(x = POVERTY_RATE)) + geom_histogram() + ggtitle("Poverty Rate")
hist27 <- ggplot(t_df, aes(x = UNEMP_RATE)) + geom_histogram() + ggtitle("Unemployment Rate")
hist28 <- ggplot(t_df, aes(x = MD_FAMINC)) + geom_histogram() + ggtitle("Median Family Income of Students")
hist1 + hist2 + hist3 + hist4 + hist5 + hist11 + hist12 + hist13 + hist14 + hist15 + hist16 + hist17 + hist18 + hist19 + hist20 + hist21 + hist22 + hist23 + hist24 + hist25 + hist26 + hist27 + hist28 + plot_layout(nrow = 6, byrow = FALSE) + plot_annotation(title = 'Histograms of Variables')

pairs(t_df[,-c(1)])

model2 <- lm(ADM_RATE ~ STATEID + AVGFACSAL + COSTT4_A + PFTFAC + FEMALE + PAR_ED_PCT_1STGEN + PCTPELL + INC_PCT_LO + MD_FAMINC + PCT_ASIAN + PCT_BA + PCT_GRAD_PROF + UG25ABV, data = t_df)

ggplot(t_df, aes(fitted(model2), ADM_RATE)) + geom_point() + ggtitle("Response versus Fitted") + geom_smooth(method = 'lm', se = FALSE)

resy <- ggplot(t_df, aes(fitted(model2), residuals(model2))) + geom_point() + ggtitle("Residuals versus Fitted") + geom_smooth(method = 'lm', se = FALSE)
res1 <- ggplot(t_df, aes(PCTPELL, residuals(model2))) + geom_point() + ggtitle("Residuals versus Percentage of Undergraduates \n Receiving Pell Grant") + geom_smooth(method = 'lm', se = FALSE)
res2 <- ggplot(t_df, aes(FEMALE, residuals(model2))) + geom_point() + ggtitle("Residuals versus Proportion of Student Body \n that is Female") + geom_smooth(method = 'lm', se = FALSE)
res3 <- ggplot(t_df, aes(PCT_BA, residuals(model2))) + geom_point() + ggtitle("Residuals versus Percentage of the Population from \n Students' Zipcodes with a \n Bachelor's Degree over the Age 25") + geom_smooth(method = 'lm', se = FALSE)
res4 <- ggplot(t_df, aes(STATEID, residuals(model2))) + geom_point() + ggtitle("Residuals versus States") + geom_smooth(method = 'lm', se = FALSE)
res5 <- ggplot(t_df, aes(COSTT4_A, residuals(model2))) + geom_point() + ggtitle("Residuals versus Average Cost of Attendance") + geom_smooth(method = 'lm', se = FALSE)
res6 <- ggplot(t_df, aes(PFTFAC, residuals(model2))) + geom_point() + ggtitle("Proportion of Full-Time Faculty Members") + geom_smooth(method = 'lm', se = FALSE)
res7 <- ggplot(t_df, aes(AVGFACSAL, residuals(model2))) + geom_point() + ggtitle("Average Faculty Salary") + geom_smooth(method = 'lm', se = FALSE)
res8 <- ggplot(t_df, aes(PAR_ED_PCT_1STGEN, residuals(model2))) + geom_point() + ggtitle("Percentage of \n First-Generation Students") + geom_smooth(method = 'lm', se = FALSE)
res9 <- ggplot(t_df, aes(INC_PCT_LO, residuals(model2))) + geom_point() + ggtitle("Percentage of Aided Students whose \n Family Income is between $0-$30,000") + geom_smooth(method = 'lm', se = FALSE)
res10 <- ggplot(t_df, aes(MD_FAMINC, residuals(model2))) + geom_point() + ggtitle("Median Family Income of Students") + geom_smooth(method = 'lm', se = FALSE)
res11 <- ggplot(t_df, aes(PCT_ASIAN, residuals(model2))) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes that is Asian") + geom_smooth(method = 'lm', se = FALSE)
res12 <- ggplot(t_df, aes(PCT_GRAD_PROF, residuals(model2))) + geom_point() + ggtitle("Percentage of the Population from \n Students' Zipcodes with a \n Professional Degree over the Age 25") + geom_smooth(method = 'lm', se = FALSE)
res13 <- ggplot(t_df, aes(UG25ABV, residuals(model2))) + geom_point() + ggtitle("Percentage of Undergraduates \n Aged 25+") + geom_smooth(method = 'lm', se = FALSE)

(resy) /
  (res1 + res2 + res3) /
  (res4 + res5 + res6) /
  (res7 + res8 + res9) /
  (res10 + res11 + res12) /
  (res13 + plot_spacer() + plot_spacer()) 

summary(full)
summary (model1)

pairs(t_df[,-c(1, 5, 6, 8, 9, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)])

pairs(df[, -c(1, 2, 3)])

model3 <- lm(ADM_RATE ~ STATEID + CONTROL + NUMBRANCH + AVGFACSAL + PAR_ED_PCT_1STGEN + UG25ABV + INC_PCT_LO, data = t_df)
summary(model3)

qqnorm(residuals(model3))
qqline(residuals(model3))

anova(model3, full)