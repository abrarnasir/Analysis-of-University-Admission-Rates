library("tidyverse")
library("patchwork")
library("car")

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)

df <- read.csv("VPdataset.csv")

df <- transform(df, STATEID = as.numeric(factor(STABBR)))
df <- df[, !(names(df) %in% c("UNITID", "INSTNM", "STABBR"))]
df <- df %>% relocate(STATEID)
df <- df %>% relocate(ADM_RATE)

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

full <- lm(ADM_RATE ~., data = df)
summary(full)

pairs(df[, -c(1)])

ggplot(df, aes(fitted(full), ADM_RATE)) + geom_point() + ggtitle("Response versus Fitted")

resy <- ggplot(df, aes(fitted(full), residuals(full))) + geom_point() + ggtitle("Residuals versus Fitted") + geom_smooth(method = 'lm', se = FALSE)
res1 <- ggplot(df, aes(STATEID, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[2])) + geom_smooth(method = 'lm', se = FALSE)
res2 <- ggplot(df, aes(NUMBRANCH, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[3])) + geom_smooth(method = 'lm', se = FALSE)
res3 <- ggplot(df, aes(CONTROL, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[4])) + geom_smooth(method = 'lm', se = FALSE) 
res4 <- ggplot(df, aes(REGION, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[5])) + geom_smooth(method = 'lm', se = FALSE)
res5 <- ggplot(df, aes(HBCU, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[6])) + geom_smooth(method = 'lm', se = FALSE)
res6 <- ggplot(df, aes(PBI, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[7])) + geom_smooth(method = 'lm', se = FALSE)
res7 <- ggplot(df, aes(TRIBAL, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[8])) + geom_smooth(method = 'lm', se = FALSE)
res8 <- ggplot(df, aes(HSI, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[9])) + geom_smooth(method = 'lm', se = FALSE)
res9 <- ggplot(df, aes(WOMENONLY, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[10])) + geom_smooth(method = 'lm', se = FALSE)
res10 <- ggplot(df, aes(COSTT4_A, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[11])) + geom_smooth(method = 'lm', se = FALSE)
res11 <- ggplot(df, aes(AVGFACSAL, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[12])) + geom_smooth(method = 'lm', se = FALSE)
res12 <- ggplot(df, aes(PFTFAC, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[13])) + geom_smooth(method = 'lm', se = FALSE)
res13 <- ggplot(df, aes(PCTPELL, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[14])) + geom_smooth(method = 'lm', se = FALSE)
res14 <- ggplot(df, aes(UG25ABV, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[15])) + geom_smooth(method = 'lm', se = FALSE)
res15 <- ggplot(df, aes(INC_PCT_LO, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[16])) + geom_smooth(method = 'lm', se = FALSE)
res16 <- ggplot(df, aes(PAR_ED_PCT_1STGEN, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[17])) + geom_smooth(method = 'lm', se = FALSE)
res17 <- ggplot(df, aes(FEMALE, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[18])) + geom_smooth(method = 'lm', se = FALSE)
res18 <- ggplot(df, aes(MD_FAMINC, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[19])) + geom_smooth(method = 'lm', se = FALSE)
res19 <- ggplot(df, aes(PCT_WHITE, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[20])) + geom_smooth(method = 'lm', se = FALSE)
res20 <- ggplot(df, aes(PCT_BLACK, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[21])) + geom_smooth(method = 'lm', se = FALSE)
res21 <- ggplot(df, aes(PCT_ASIAN, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[22])) + geom_smooth(method = 'lm', se = FALSE)
res22 <- ggplot(df, aes(PCT_HISPANIC, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[23])) + geom_smooth(method = 'lm', se = FALSE)
res23 <- ggplot(df, aes(PCT_BA, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[24])) + geom_smooth(method = 'lm', se = FALSE)
res24 <- ggplot(df, aes(PCT_GRAD_PROF, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[25])) + geom_smooth(method = 'lm', se = FALSE)
res25 <- ggplot(df, aes(PCT_BORN_US, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[26])) + geom_smooth(method = 'lm', se = FALSE)
res26 <- ggplot(df, aes(POVERTY_RATE, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[27])) + geom_smooth(method = 'lm', se = FALSE)
res27 <- ggplot(df, aes(UNEMP_RATE, residuals(full))) + geom_point() + ggtitle(paste0("Residuals vs. ", names(df)[28])) + geom_smooth(method = 'lm', se = FALSE)
(resy + res1 + res2 + res3) /
  (res4 + res5 + res6 + res7) /
  (res8 + res9 + res10 + res11) /
  (res12 + res13 + res14 + res15) /
  (res16 + res17 + res18 + res19) /
  (res20 + res21 + res22 + res23) /
  (res24 + res25 + res26 + res27)

qqnorm(residuals(full))
qqline(residuals(full))

bc <- boxCox(full)
lambda <- bc$x[which.max(bc$y)]

p <- powerTransform(cbind(df[, -c(1)]) + 0.00000001)
summary(p)

transformed_df <- data.frame(
  df$ADM_RATE ** lambda,
  df$STATEID ** 0.83,
  df$NUMBRANCH ** -5.72,
  log(df$CONTROL),
  df$REGION ** 0.5,
  df$HBCU ** -1.51,
  df$PBI ** -3.44,
  df$TRIBAL ** -36.15,
  df$HSI ** -0.46,
  df$WOMENONLY ** -8.15,
  df$COSTT4_A ** 0.50,
  df$AVGFACSAL ** 0.33,
  df$PFTFAC ** 1,
  df$PCTPELL ** 0.50,
  df$UG25ABV ** 0.33,
  df$INC_PCT_LO ** 0.33,
  df$PAR_ED_PCT_1STGEN ** 0.76,
  df$FEMALE ** 2.00,
  df$MD_FAMINC ** 0.50,
  df$PCT_WHITE ** 2.93,
  df$PCT_BLACK ** 0.57,
  df$PCT_ASIAN ** 0.11,
  df$PCT_HISPANIC ** 0.12,
  df$PCT_BA ** 1,
  df$PCT_GRAD_PROF ** 0.33,
  df$PCT_BORN_US ** 9.47,
  df$POVERTY_RATE ** -0.33,
  df$UNEMP_RATE ** -0.80
)

for(i in 1:28){
  names(transformed_df)[i] <- names(df)[i]
}

#names(transformed_df)[1] <- "Admission Rate"
#names(transformed_df)[2] <- "State Identifier"
#names(transformed_df)[3] <- "Number of Branches"
#names(transformed_df)[4] <- "Control Identifier"
#names(transformed_df)[5] <- "Region Identifier"
#names(transformed_df)[6] <- "Historically Black-Serving Institute Identifier"
#names(transformed_df)[7] <- "Predominantly Black-Serving Institute Identifier"
#names(transformed_df)[8] <- "Tribal-Serving Institute Identifier"
#names(transformed_df)[9] <- "Hispanic-Serving Institute Identifier"
#names(transformed_df)[10] <- "Women-Only Institute Identifier"
#names(transformed_df)[11] <- "Cost of Attendance"
#names(transformed_df)[12] <- "Average Faculty Salary"
#names(transformed_df)[13] <- "Proportion of FT Faculty"
#names(transformed_df)[14] <- "% of UGs Receiving Pell Grant"
#names(transformed_df)[15] <- "% of UGs Aged 25 and Above"
#names(transformed_df)[16] <- "% of Aided Students whose Family Income is between $0-$30,000"
#names(transformed_df)[17] <- "% of First-Generation Students"
#names(transformed_df)[18] <- "Proportion of Female Student Body"
#names(transformed_df)[19] <- "Median Family Income of Students"
#names(transformed_df)[20] <- "% of White Population in Students' Neighborhood"
#names(transformed_df)[21] <- "% of Black Population in Students' Neighborhood"
#names(transformed_df)[22] <- "% of Asian Population in Students' Neighborhood"
#names(transformed_df)[23] <- "% of Hispanic Population in Students' Neighborhood"
#names(transformed_df)[24] <- "% of Population Aged 25+ with a Bachelor's Degree in Students' Neighborhood"
#names(transformed_df)[25] <- "% of Population Aged 25+ with a Professional Degree in Students' Neighborhood"
#names(transformed_df)[26] <- "% of U.S. Born Population in Students' Neighborhood"
#names(transformed_df)[27] <- "Poverty Rate in Students' Neighborhood"
#names(transformed_df)[28] <- "Unemployment Rate in Students' Neighborhood"

model <- lm(ADM_RATE ~ STATEID + CONTROL + NUMBRANCH + AVGFACSAL + PAR_ED_PCT_1STGEN + UG25ABV + INC_PCT_LO, data = transformed_df)
summary(model)

pairs(transformed_df[, c(2, 3, 4, 12, 15, 16, 17)])

ggplot(transformed_df, aes(fitted(model), ADM_RATE)) + geom_point() + ggtitle("Response versus Fitted") + geom_smooth(method = 'lm', se = FALSE)

resty <- ggplot(transformed_df, aes(fitted(model), residuals(model))) + geom_point() + ggtitle("Residuals versus Fitted") + geom_smooth(method = 'lm', se = FALSE)
rest1 <- ggplot(transformed_df, aes(STATEID, residuals(model))) + geom_point() + ggtitle("Residuals vs. STATEID") + geom_smooth(method = 'lm', se = FALSE)
rest2 <- ggplot(transformed_df, aes(CONTROL, residuals(model))) + geom_point() + ggtitle("Residuals vs. CONTROL") + geom_smooth(method = 'lm', se = FALSE)
rest3 <- ggplot(transformed_df, aes(NUMBRANCH, residuals(model))) + geom_point() + ggtitle("Residuals vs. NUMBRANCH") + geom_smooth(method = 'lm', se = FALSE)
rest4 <- ggplot(transformed_df, aes(AVGFACSAL, residuals(model))) + geom_point() + ggtitle("Residuals vs. AVGFACSAL") + geom_smooth(method = 'lm', se = FALSE)
rest5 <- ggplot(transformed_df, aes(PAR_ED_PCT_1STGEN, residuals(model))) + geom_point() + ggtitle("Residuals vs. PAR_ED_PCT_1STGEN") + geom_smooth(method = 'lm', se = FALSE)
rest6 <- ggplot(transformed_df, aes(UG25ABV, residuals(model))) + geom_point() + ggtitle("Residuals vs. UG25ABV") + geom_smooth(method = 'lm', se = FALSE)
rest7 <- ggplot(transformed_df, aes(INC_PCT_LO, residuals(model))) + geom_point() + ggtitle("Residuals vs. INC_PCT_LO") + geom_smooth(method = 'lm', se = FALSE)
(resty + rest1 + rest2 + rest3) /
  (rest4 + rest5 + rest6 + rest7)

qqnorm(residuals(model))
qqline(residuals(model))