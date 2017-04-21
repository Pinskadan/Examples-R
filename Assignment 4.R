#########
##Script name: Assignment 4.R
##Author: Gibson Wirth
##Date Created: November/December 2015
##Purpose: Analyzing IHIS data, for educational purposes
########
##Data sources:
##2005-2014 IHIS Survey
## Explanatory variables:
##  BIRTHYR, AGE, REGION and SEX
## Response variables:
## ARESTLESS and VIG10DMIN
########
## Data preprocessing:
## Subsetting data 2005-2014
## Slicing data by ARESTLESS and VIG10DMIN
## Adjusting weights for survey study
########
## Data integration:
## Already integrated by NHISID
########

## Set working directory
## setwd()

ihis_00005 <- read.csv("C:/Users/Oliver/Desktop/ihis_00005.csv", header=TRUE)
ihisdata <- ihis_00005

# subsetting ARESTLESS data set by removing null, NIU, or "Unknown" variables
ihisdataA <- subset(ihisdata, BIRTHYR < 9996)
ihisdataB <- subset(ihisdataA, ARESTLESS==1 | ARESTLESS==2 | ARESTLESS==3 | ARESTLESS==4 | ARESTLESS==0 )
ihisdataC <- subset(ihisdataB, AFEELINT1MO==1 | AFEELINT1MO==2 | AFEELINT1MO==3 | AFEELINT1MO==4)

# final data set + adjusting weights for surveying
ihisFinal1 <- ihisdataC
ihisFinal1[13] <- ihisFinal1[13]/10

# subsetting VIG10DMIN data set by removing null, NIU, or "Unknown" variables
ihisdataD <- subset(ihisdataA, VIG10DTP==1 | VIG10DTP==2 )
ihisdataE <- subset(ihisdataD, VIG10FTP==1 | VIG10FTP==2 | VIG10FTP==3 | VIG10FTP==3 | VIG10FTP==4 | VIG10FTP==5)
ihisdataF <- subset(ihisdataE, VIG10DMIN < 361 & VIG10DMIN != 0 )

# final data set + adjusting weights for surveying
ihisFinal2 <- ihisdataF
ihisFinal2[13] <- ihisFinal2[13]/10
  

#creating the survey design for the future tests
library(survey)
options(survey.lonely.psu = "adjust")
ihisDesign1 <- svydesign(id=~PSU, strata=~STRATA , weights=~SAMPWEIGHT, nest=TRUE, data = ihisdataC)
ihisDesign2 <- svydesign(id=~PSU, strata=~STRATA, weights=~SAMPWEIGHT, nest=TRUE, data = ihisdataF)

# visualizations of each data field
svyhist(~AGE, ihisDesign1, main='1')
svyhist(~REGION, ihisDesign1, main = '2')
svyhist(~BIRTHYR, ihisDesign1, main = '3')
svyhist(~SEX, ihisDesign1, main = '4')
svyhist(~ARESTLESS, ihisDesign1, main = '5')
svyhist(~AFEELINT1MO, ihisDesign1, main = '6')
svyhist(~VIG10DMIN, ihisDesign2, main='7')

# visualization and comparison of VIG10DMIN and AGE
svyplot(VIG10DMIN~AGE, design=ihisDesign2, style = "bubble", ylab='Duration of Vigorous Activity', xlab='Age', main='Figure 1 - Age vs. Duration of Vigorous Physical Activity')
AgeActivity <- svyglm(VIG10DMIN~AGE, design=ihisDesign2, data=ihisdataF)
summary(AgeActivity)

# visualization and comparison of VIG10DMIN and BIRTHYR
svyplot(VIG10DMIN~BIRTHYR, design=ihisDesign2, style="bubble", ylab='Duration of Vigorous Activity', xlab='Birth Year', main='Figure 2 -Birth Year vs. Duration of Vigorous Physical Activity')
YearActivity <- svyglm(VIG10DMIN~BIRTHYR, design=ihisDesign2, data=ihisdataF)
summary(YearActivity)

# visualization and comparison of VIG10DMIN and REGION
svyboxplot(~VIG10DMIN~factor(REGION), design=ihisDesign2, main='Fig. 3- Duration of Vigorous Activity for each Region', xlab='Region', ylab='Duration of Vigorous Activity')
svyttest(VIG10DMIN~REGION, design=ihisDesign2)
svyttest(VIG10DMIN~as.factor(REGION==2), design=ihisDesign2)
svyttest(VIG10DMIN~as.factor(REGION==3), design=ihisDesign2)

# visualization and comparison of VIG10DMIN and SEX
svyboxplot(~VIG10DMIN~factor(SEX), design=ihisDesign2, main='Fig. 4 - Sex for Duration of Vigorous Activity', xlab='Sex', ylab='Duration of Vigorous Activity')
svyttest(VIG10DMIN~SEX==1, design=ihisDesign2)
svyttest(VIG10DMIN~SEX==2, design=ihisDesign2)


# visualization and comparison of ARESTLESS and AGE
svyboxplot(~AGE~factor(ARESTLESS), design=ihisDesign1, main='Figure 5 - Age for each feeling of restlessness', xlab='Feeling of restlessness', ylab='Age')
AgeRestlessNever <- svyglm(as.factor(ARESTLESS==0)~AGE, design = ihisDesign1, family=quasibinomial)
summary(AgeRestlessNever)
AgeRestlessSome <- svyglm(as.factor(ARESTLESS==2)~AGE, design = ihisDesign1, family=quasibinomial)
summary(AgeRestlessSome)
AgeRestlessAll <- svyglm(as.factor(ARESTLESS==4)~AGE, design = ihisDesign1, family=quasibinomial)
summary(AgeRestlessAll)

# visualization and comparsion of ARESTLESS and BIRTHYR
svyboxplot(~BIRTHYR~factor(ARESTLESS), design=ihisDesign1, main='Figure 6 - Birth Year for each feeling of restlessness', xlab='Feeling of restlessness', ylab='Birth Year')
BirthRestlessNever <- svyglm(as.factor(ARESTLESS==0)~BIRTHYR, design = ihisDesign1, family=quasibinomial)
summary(BirthRestlessNever)
BirthRestlessSome <- svyglm(as.factor(ARESTLESS==2)~BIRTHYR, design = ihisDesign1, family=quasibinomial)
summary(BirthRestlessSome)
BirthRestlessAll <- svyglm(as.factor(ARESTLESS==4)~BIRTHYR, design = ihisDesign1, family=quasibinomial)
summary(BirthRestlessAll)

# visualization and comparison of ARESTLESS and REGION
svyContingency <- svytable(~ihisdataC$REGION+ihisdataC$ARESTLESS, ihisDesign1)
dimnames(svyContingency)[[1]]<- c('Northeast','Midwest','South','West')
dimnames(svyContingency)[[2]]<- c('Never','A little','Some','Most','All')
plot(svyContingency, main='Figure 7 - Region vs. Restlessness', xlab='US Region', ylab='Restlessness')
svychisq(~ARESTLESS+REGION, statistic =c("Chisq"), ihisDesign1)

# visualization and comparsion of ARESTLESS and SEX
svyContingency <- svytable(~ihisdataC$SEX+ihisdataC$ARESTLESS, ihisDesign1)
dimnames(svyContingency)[[1]]<- c('Male','Female')
dimnames(svyContingency)[[2]]<- c('Never','A little','Some','Most','All')
plot(svyContingency, main='Figure 8 - Sex vs. Restlessness', xlab='Sex', ylab='Restlessness')
svychisq(~ARESTLESS+SEX, statistic =c("Chisq"), ihisDesign1)
