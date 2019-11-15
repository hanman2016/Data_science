#setwd("C:/Users/pdubhasi/Documents/UHG/upgrad/capstone project")
# Loading required libraries

library(dplyr)
library(mice)
library(e1071)
library(ggplot2)
library(MASS)
library(caret)
library(car)
library(caTools)
library(cowplot)

# Defining multiplot function to plot mutliple plots in single page
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



############################################################
############# Data preparation and EDA #####################
############################################################

# Importing hospital ratings
Hospital_Rating <- read.csv("Hospital General Information.csv", na.strings = c("Not Available"),stringsAsFactors = F)
sum(is.na(Hospital_Rating))
sapply(Hospital_Rating, function(x) sum(is.na(x)))

colnames(Hospital_Rating)
Hospital_Rating <- Hospital_Rating[,c(1,13)]

# Importing Complications Dataset
Complications <- read.csv("Complications - Hospital.csv", na.strings = c("Not Available"), stringsAsFactors = F)
Complications <- Complications[,c(1,10,13)]
Complications <- subset(Complications, grepl("COMP_HIP_KNEE|PSI_4_SURG_COMP|PSI_90_SAFETY",Measure.ID))
colnames(Complications) <- c("ProviderID", "MeasureID","Score")
Complications <- reshape(Complications, idvar="ProviderID", timevar="MeasureID",direction="wide")

#Gathering Mortality Measures(7)
Mortality_data <- read.csv("Readmissions and Deaths - Hospital.csv", na.strings = c("Not Available"),stringsAsFactors = F)
str(Mortality_data)
Mortality_data <- Mortality_data[,c(1,10,13)]
Mortality_measures <- subset(Mortality_data, grepl("^MORT_",Measure.ID))
colnames(Mortality_measures) <- c("ProviderID", "MeasureID","Score")
Mortality_measures <- reshape(Mortality_measures, idvar="ProviderID", timevar="MeasureID",direction="wide")

Mortality_measures$Score.PSI_4_SURG_COMP <- Complications$Score.PSI_4_SURG_COMP
md.pattern(Mortality_measures)
sapply(Mortality_measures, function(x) sum(is.na(x)))
imputed_Mortality <- mice(Mortality_measures, m=5, maxit = 10, method = 'pmm', seed = 500)
summary(imputed_Mortality)
Mortality_measures <- complete(imputed_Mortality,2)
# Standardizing measures
Mortality_measures_sd <- data.frame(sapply(Mortality_measures[-1], function(x) scale(x)))
Mortality_measures_sd$ProviderID <- Mortality_measures$ProviderID
colnames(Mortality_measures_sd)
#Mortality_measures_sd <- Mortality_measures_sd[,-c(8)]

#### EDA for morality measures vs hospetal ratings.
plot_mort <- merge(Mortality_measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_mort,aes(y=Score.MORT_30_AMI,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(MORT_30_AMI)")
p2<-ggplot(data = plot_mort,aes(y=Score.MORT_30_CABG,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(MORT_30_CABG)")
p3<-ggplot(data = plot_mort,aes(y=Score.MORT_30_COPD,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(MORT_30_COPD)")
p4<-ggplot(data = plot_mort,aes(y=Score.MORT_30_HF,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(MORT_30_HF)")
p5<-ggplot(data = plot_mort,aes(y=Score.MORT_30_PN,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(MORT_30_PN)")
p6<-ggplot(data = plot_mort,aes(y=Score.MORT_30_STK,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(MORT_30_STK)")
p7<-ggplot(data = plot_mort,aes(y=Score.PSI_4_SURG_COMP,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(PSI_4_SURG_COMP)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,cols = 1)
multiplot(p5,p6,p7,cols = 1)
# From the plot we can infer that hospitals must achieve very low death rate to get high rating.


# Gathering Readmission Measures(8)
Readmission_measures <- subset(Mortality_data, grepl("^READM_",Measure.ID))
colnames(Readmission_measures) <- c("ProviderID", "MeasureID","Score")
View(Readmission_measures)
Readmission_measures <- reshape(Readmission_measures, idvar="ProviderID", timevar="MeasureID",direction="wide")
md.pattern(Readmission_measures)
imputed_Readmission <- mice(Readmission_measures, m=5, maxit = 10, method = 'pmm', seed = 500)
summary(imputed_Readmission)
Readmission_measures <- complete(imputed_Readmission,2)
# Standardizing measures
Readmission_measures_sd <- data.frame(sapply(Readmission_measures[-1], function(x) scale(x)))
Readmission_measures_sd$ProviderID <- Readmission_measures$ProviderID
colnames(Readmission_measures)

#### EDA for Readmission measures vs hospetal ratings.
plot_readmsn <- merge(Readmission_measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_AMI,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Unplanned readmission rate(READM_30_AMI)")
p2<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_CABG,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Unplanned readmission rate(READM_30_CABG)")
p3<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_COPD,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Unplanned readmission rate(READM_30_COPD)")
p4<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_HF,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Unplanned readmission rate(READM_30_HF)")
p5<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_HIP_KNEE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Unplanned readmission rate(READM_30_HIP_KNEE)")
p6<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_HOSP_WIDE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Unplanned readmission rate(READM_30_HSP_WIDE)")
p7<-ggplot(data = plot_readmsn,aes(y=Score.READM_30_STK,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate for HeartAttack patients(READM_30_STK)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,cols = 1)
multiplot(p5,p6,p7,cols = 1)
# hospital's readmission rate must be less than 15 to achive 4 or 5 rating. 

# Gathering Safety Measures (8)
SafetyofCare_Measures <- read.csv("Healthcare Associated Infections - Hospital.csv", na.strings = c("Not Available"), stringsAsFactors = F)
SafetyofCare_Measures <- SafetyofCare_Measures[,c(1,10,12)]
SafetyofCare_Measures <- subset(SafetyofCare_Measures, grepl("\\d_SIR",Measure.ID))
colnames(SafetyofCare_Measures) <- c("ProviderID", "MeasureID","Score")
sapply(SafetyofCare_Measures, function(x) sum(is.na(x)))
SafetyofCare_Measures <- reshape(SafetyofCare_Measures, idvar="ProviderID", timevar="MeasureID",direction="wide")
#Adding the missing measures of Safety
SafetyofCare_Measures$Score.COMP_HIP_KNEE <- Complications$Score.COMP_HIP_KNEE
SafetyofCare_Measures$Score.PSI_90_SAFETY <- Complications$Score.PSI_90_SAFETY

sapply(SafetyofCare_Measures, function(x) sum(is.na(x)))
imputed_Safety <- mice(SafetyofCare_Measures, m=5, maxit = 10, method = 'pmm', seed = 500)
summary(imputed_Safety)
SafetyofCare_Measures <- complete(imputed_Safety,2)
# Standardizing measures
SafetyofCare_Measures_sd <- data.frame(sapply(SafetyofCare_Measures[-1], function(x) scale(x)))
SafetyofCare_Measures_sd$ProviderID <- SafetyofCare_Measures$ProviderID

#### EDA for Safety measures vs hospetal ratings.
plot_safety <- merge(SafetyofCare_Measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_safety,aes(y=Score.HAI_1_SIR,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="hospital associated infection rate(HAI_1_SIR)")
p2<-ggplot(data = plot_safety,aes(y=Score.HAI_2_SIR,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="hospital associated infection rate(HAI_2_SIR)")
p3<-ggplot(data = plot_safety,aes(y=Score.HAI_3_SIR,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="hospital associated infection rate(HAI_3_SIR)")
p4<-ggplot(data = plot_safety,aes(y=Score.HAI_4_SIR,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="hospital associated infection rate(HAI_4_SIR)")
p5<-ggplot(data = plot_safety,aes(y=Score.HAI_5_SIR,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="hospital associated infection rate(HAI_5_SIR)")
p6<-ggplot(data = plot_safety,aes(y=Score.HAI_6_SIR,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="hospital associated infection rate(HAI_6_SIR)")
p7<-ggplot(data = plot_safety,aes(y=Score.COMP_HIP_KNEE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="complication rate of HIP/KNEE(COMP_HIP_KNEE)")
p8<-ggplot(data = plot_safety,aes(y=Score.PSI_90_SAFETY,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Death rate of serious complications(READM_30_STK)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,cols = 1)
multiplot(p5,p6,cols = 1)
multiplot(p7,p8,cols = 1)
# hospital's HAI (hospital associated infections) must be less than 1% to get rating among 3,4 & 5.
# Alo the KNEE complications rate should be less than 3 for hospital to get 5 rating.


# Gathering Patient Experience Measures(11)
patient_experience_Measures <- read.csv("HCAHPS - Hospital.csv", na.strings = c("Not Available"),stringsAsFactors = F)
patient_experience_Measures <- patient_experience_Measures[,c(1,9,16)]
patient_experience_Measures <- subset(patient_experience_Measures, grepl("LINEAR_SCORE",HCAHPS.Measure.ID))
colnames(patient_experience_Measures) <- c("ProviderID", "MeasureID","Score")
patient_experience_Measures <- reshape(patient_experience_Measures, idvar="ProviderID", timevar="MeasureID",direction="wide")
sapply(patient_experience_Measures, function(x) sum(is.na(x)))
str(patient_experience_Measures)
patient_experience_Measures <- lapply(patient_experience_Measures, function(x) {as.numeric(x)})
patient_experience_Measures <- as.data.frame(patient_experience_Measures)
md.pattern(patient_experience_Measures)
imputed_experience <- mice(patient_experience_Measures, m=5, maxit = 10, method = 'pmm', seed = 500)
patient_experience_Measures <- complete(imputed_experience,2)
# Standardizing measures
patient_experience_Measures_sd <- data.frame(sapply(patient_experience_Measures[-1], function(x) scale(x)))
patient_experience_Measures_sd$ProviderID <- patient_experience_Measures$ProviderID

#### EDA for patient experience measures.
plot_PEM <- merge(patient_experience_Measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_PEM,aes(y=Score.H_CLEAN_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_CLEAN_LINEAR_SCORE)")
p2<-ggplot(data = plot_PEM,aes(y=Score.H_COMP_1_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_COMP_1_LINEAR_SCORE)")
p3<-ggplot(data = plot_PEM,aes(y=Score.H_COMP_3_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_COMP_3_LINEAR_SCORE)")
p4<-ggplot(data = plot_PEM,aes(y=Score.H_COMP_5_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_COMP_5_LINEAR_SCORE)")
p5<-ggplot(data = plot_PEM,aes(y=Score.H_COMP_7_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_COMP_7_LINEAR_SCORE)")
p6<-ggplot(data = plot_PEM,aes(y=Score.H_HSP_RATING_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_HSP_RATING_LINEAR_SCORE)")
p7<-ggplot(data = plot_PEM,aes(y=Score.H_QUIET_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_QUIET_LINEAR_SCORE)")
p8<-ggplot(data = plot_PEM,aes(y=Score.H_RECMND_LINEAR_SCORE,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="patient's exp measure(H_RECMND_LINEAR_SCORE)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,cols = 1)
multiplot(p5,p6,cols = 1)
multiplot(p7,p8,cols = 1)
# Patients should rate the hospital more 80 or 90% interms of cleanliness for hospital to get 4 or 5 rating.


# Gathering Imaging Measures(5)
Imaging_Measures <- read.csv("Outpatient Imaging Efficiency - Hospital.csv", na.strings = c("Not Available"), stringsAsFactors = F)
Imaging_Measures <- Imaging_Measures[,c(1,9,11)]
Imaging_Measures <- subset(Imaging_Measures, grepl("OP_8|OP_10|OP_11|OP_13|OP_14",Measure.ID))
colnames(Imaging_Measures) <- c("ProviderID", "MeasureID","Score")
Imaging_Measures <- reshape(Imaging_Measures, idvar="ProviderID", timevar="MeasureID",direction="wide")
sapply(Imaging_Measures, function(x) sum(is.na(x)))
md.pattern(Imaging_Measures)
imputed_imaging <- mice(Imaging_Measures, m=5, maxit = 10, method = 'pmm', seed = 500)
summary(imputed_imaging)
Imaging_Measures <- complete(imputed_imaging,2)
# Standardizing measures
Imaging_Measures_sd <- data.frame(sapply(Imaging_Measures[-1], function(x) scale(x)))
Imaging_Measures_sd$ProviderID <- Imaging_Measures$ProviderID

#### EDA for Imaging measures.
plot_imaging <- merge(Imaging_Measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_imaging,aes(y=Score.OP_10,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,20))+labs(x="Hospital rating", y="Outpatient Imaging Efficiency(OP_10)")
p2<-ggplot(data = plot_imaging,aes(y=Score.OP_11,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,10))+labs(x="Hospital rating", y="Outpatient Imaging Efficiency(OP_11)")
p3<-ggplot(data = plot_imaging,aes(y=Score.OP_13,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,10))+labs(x="Hospital rating", y="Outpatient Imaging Efficiency(OP_13)")
p4<-ggplot(data = plot_imaging,aes(y=Score.OP_14,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,10))+labs(x="Hospital rating", y="Outpatient Imaging Efficiency(OP_14)")
p5<-ggplot(data = plot_imaging,aes(y=Score.OP_8,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(20,60))+labs(x="Hospital rating", y="Outpatient Imaging Efficiency(OP_8)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,p5,cols = 1)
# Imaging efficiency measures should be low in values so that patients doesnt have to go through unnecessary scannings.


# Gathering Timely & Effective Measures
Timely_and_Effective_Care_data <- read.csv("Timely and Effective Care - Hospital.csv", na.strings = c("Not Available"), stringsAsFactors = F)
Timely_and_Effective_Care_data <- Timely_and_Effective_Care_data[,c(1,10,12)]

# Gathering Timely Measures(7)
Timely_Measures <- subset(Timely_and_Effective_Care_data, grepl("ED_1b|ED_2b|OP_3b|OP_5|OP_18b|OP_20|OP_21",Measure.ID))
View(Timely_Measures)
colnames(Timely_Measures) <- c("ProviderID", "MeasureID","Score")
Timely_Measures <- reshape(Timely_Measures, idvar="ProviderID", timevar="MeasureID",direction="wide")
sapply(Timely_Measures, function(x) sum(is.na(x)))
md.pattern(Timely_Measures)
imputed_Timely <- mice(Timely_Measures, m=5, maxit = 10, method = 'pmm', seed = 500)
str(Timely_Measures)
Timely_Measures <- lapply(Timely_Measures, function(x) {as.numeric(x)})
Timely_Measures <- as.data.frame(Timely_Measures)
str(Timely_Measures)
imputed_Timely <- mice(Timely_Measures, m=5, maxit = 10, method = 'pmm', seed = 500)
summary(imputed_Timely)
Timely_Measures <- complete(imputed_Timely,2)
# Standardizing measures
Timely_Measures_sd <- data.frame(sapply(Timely_Measures[-1], function(x) scale(x)))
Timely_Measures_sd$ProviderID <- Timely_Measures$ProviderID

#### EDA for Timely Measures.
plot_timely <- merge(Timely_Measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_timely,aes(y=Score.ED_1b,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(100,600))+labs(x="Hospital rating", y="Timley Measure(ED_1b)")
p2<-ggplot(data = plot_timely,aes(y=Score.ED_2b,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,300))+labs(x="Hospital rating", y="Timley Measure(ED_2b)")
p3<-ggplot(data = plot_timely,aes(y=Score.OP_18b,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Timley Measure(OP_18b)")
p4<-ggplot(data = plot_timely,aes(y=Score.OP_20,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Timley Measure(OP_20)")
p5<-ggplot(data = plot_timely,aes(y=Score.OP_21,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Timley Measure(OP_21)")
p6<-ggplot(data = plot_timely,aes(y=Score.OP_3b,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Timley Measure(OP_3b)")
p7<-ggplot(data = plot_timely,aes(y=Score.OP_5,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Timley Measure(OP_5)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,cols = 1)
multiplot(p5,p6,p7,cols = 1)
# Average overall time taken by patient to transfer from emergency to inpatient devision should be less than 300mins to get high ratings.
# Wait time of patient in emergency to get visited by healthcare professional should be less than 30mins.
# Overall time of visit to an emergency should be less than 150mins.


# Gathering Effective  Measures(16)
Effective_Measures <- subset(Timely_and_Effective_Care_data, grepl("IMM_2|IMM_3_OP_27_FAC_ADHPCT|OP_4|OP_22|OP_23|OP_29|OP_30|PC_01|^VTE_1|^VTE_2|^VTE_3|^VTE_6|CAC_3|STK_1$|STK_6|STK_8",Measure.ID))
colnames(Effective_Measures) <- c("ProviderID", "MeasureID","Score")
Effective_Measures <- reshape(Effective_Measures, idvar="ProviderID", timevar="MeasureID",direction="wide")
sapply(Effective_Measures, function(x) sum(is.na(x)))
str(Effective_Measures)
Effective_Measures <- lapply(Effective_Measures, function(x) {as.numeric(x)})
Effective_Measures <- as.data.frame(Effective_Measures)
md.pattern(Effective_Measures)
imputed_Effective <- mice(Effective_Measures, m=5, maxit = 10, method = 'pmm', seed = 500)
summary(imputed_Effective)
Effective_Measures <- complete(imputed_Effective,2)
# Standardizing measures
Effective_Measures_sd <- data.frame(sapply(Effective_Measures[-1], function(x) scale(x)))
Effective_Measures_sd$ProviderID <- Effective_Measures$ProviderID

#### EDA for Effective Measures.
plot_effect <- merge(Effective_Measures,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_effect,aes(y=Score.CAC_3,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(70,100))+labs(x="Hospital rating", y="Effective Measure(CAC_3)")
p2<-ggplot(data = plot_effect,aes(y=Score.IMM_2,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(70,100))+labs(x="Hospital rating", y="Effective Measure(IMM_2)")
p3<-ggplot(data = plot_effect,aes(y=Score.OP_30,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(50,100))+labs(x="Hospital rating", y="Effective Measure(OP_30)")
p4<-ggplot(data = plot_effect,aes(y=Score.PC_01,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,10))+labs(x="Hospital rating", y="Effective Measure(PC_01)")
p5<-ggplot(data = plot_effect,aes(y=Score.STK_8,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(70,100))+labs(x="Hospital rating", y="Effective Measure(STK_8)")
p6<-ggplot(data = plot_effect,aes(y=Score.VTE_2,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(70,100))+labs(x="Hospital rating", y="Effective Measure(VTE_2)")
p7<-ggplot(data = plot_effect,aes(y=Score.VTE_6,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(0,25))+labs(x="Hospital rating", y="Effective Measure(VTE_6)")

multiplot(p1,p2,cols = 1)
multiplot(p3,p4,cols = 1)
multiplot(p5,p6,p7,cols = 1)
# Rate at which Patients assessed and given influenza vaccination should be higher for hospitals with higher rating.
# Rate of prescheduling delivery of mothers should be as min as possible. It is equivalent to 0 for hospital with 5 Rating.


# Merging all data frames
colnames(Mortality_measures)
colnames(Readmission_measures)

Final_data <- merge(Mortality_measures_sd,Readmission_measures_sd, by.x = "ProviderID", by.y = "ProviderID" , all=TRUE)
Final_data <- merge(Final_data,SafetyofCare_Measures_sd, by.x = "ProviderID", by.y = "ProviderID" , all=TRUE)
Final_data <- merge(Final_data,patient_experience_Measures_sd, by.x = "ProviderID", by.y = "ProviderID" , all=TRUE)
Final_data <- merge(Final_data,Imaging_Measures_sd, by.x = "ProviderID", by.y = "ProviderID" , all=TRUE)
Final_data <- merge(Final_data,Timely_Measures_sd, by.x = "ProviderID", by.y = "ProviderID" , all=TRUE)
Final_data <- merge(Final_data,Effective_Measures_sd, by.x = "ProviderID", by.y = "ProviderID" , all=TRUE)

#standardized_df <- completeData
#standardized_df <- data.frame(sapply(standardized_df, function(x) scale(x)))
#standardized_df$ProviderID <- Final_Measures_Data[,1]

# Adding hospital ratings to the final measure set.
Complete_data_all_measures <- merge(Final_data,Hospital_Rating,by.x= "ProviderID",by.y="Provider.ID",all=TRUE)

df <- Complete_data_all_measures[,-c(1)]

str(Complete_data_all_measures)
df$Hospital.overall.rating <- as.factor(df$Hospital.overall.rating)
str(df)

sum(is.na(df))
df <- data.frame(na.omit(df))

######################################################################################
############ Preparing Supervised learning model - By using Random Forest ############
######################################################################################
library(randomForest)

# Shuffle the data
shuffledata <- df[sample(nrow(df)), ]

# Split the data into train and test
ntrain <- as.integer(nrow(shuffledata)*0.8)
traindata <- shuffledata[1:ntrain, ]
testdata <- shuffledata[(ntrain+1):nrow(shuffledata), ]

# Build the random forest
set.seed(71)
data.rf <- randomForest(Hospital.overall.rating~ ., data=traindata, proximity=FALSE,
                        ntree=400, mtry=15, do.trace=TRUE, na.action=na.omit)
data.rf
testPred <- predict(data.rf, newdata=testdata)
summary(testPred)
table(testPred, testdata$Hospital.overall.rating)

conf_final <- confusionMatrix(testPred, testdata$Hospital.overall.rating)
conf_final
# Got the accuracy of 76.32%

########### Model Tuning #######################
# Tuning the Random Forest Model using tuneRF function
x <- df[,1:62]
y <- df[,63]
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=400)
bestmtry

# Identified mtry=22  and building model using that parameter
data.rf <- randomForest(Hospital.overall.rating~ ., data=traindata, proximity=FALSE,
                        ntree=400, mtry=22, do.trace=TRUE, na.action=na.omit)
data.rf

############ Model Evaluation ############
testPred <- predict(data.rf, newdata=testdata, type = "response")
summary(testPred)
table(testPred, testdata$Hospital.overall.rating)

conf_final <- confusionMatrix(testPred, testdata$Hospital.overall.rating)
conf_final
#Accuracy got increased to 79.45%

#####################################################################################
###### Preparing Unsupervised Learning Model - Using K-Means Clustering##############
#####################################################################################

######## Creating group scores for all the groups using factor analysis
######## Factor Analysis - Mortality Grp
fa1 <- factanal(Mortality_measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa1_load <- fa1$loadings

# Sum of Squares of factor loadings
eig1_load <- sum((fa1_load)^2)
Group_Scores <- cbind(Mortality_measures_sd, fa1$scores)
colnames(Group_Scores)
# Creating a data frame with only group scores
Group_Scores <- Group_Scores[,-c(1:7)]

######## Factor Analysis - Readmission Group
fa2 <- factanal(Readmission_measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa2_load <- fa2$loadings
#Sum of Squares of factor loadings
eig2_load <- sum((fa2_load)^2)
Group_Scores <- cbind(Group_Scores, fa2$scores)

######## Factor Analysis - Safety Group
fa3 <- factanal(SafetyofCare_Measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa3_load <- fa3$loadings
#Sum of Squares of factor loadings
eig3_load <- sum((fa3_load)^2)
Group_Scores <- cbind(Group_Scores, fa3$scores)

######## Factor Analysis - Patient Experience Group
fa4 <- factanal(patient_experience_Measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa4_load <- fa4$loadings
#Sum of Squares of factor loadings
eig4_load <- sum((fa4_load)^2)
Group_Scores <- cbind(Group_Scores, fa4$scores)

######## Factor Analysis - Imaging Groups
fa5 <- factanal(Imaging_Measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa5_load <- fa5$loadings
#Sum of Squares of factor loadings
eig5_load <- sum((fa5_load)^2)
Group_Scores <- cbind(Group_Scores, fa5$scores)

######## Factor Analysis - Timely Measures Group
fa6 <- factanal(patient_experience_Measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa6_load <- fa6$loadings
#Sum of Squares of factor loadings
eig6_load <- sum((fa6_load)^2)
Group_Scores <- cbind(Group_Scores, fa6$scores)

######## Factor Analysis - Effective Measures Group
fa7 <- factanal(patient_experience_Measures_sd, factors = 1,rotation = "varimax",scores="regression")
fa7_load <- fa7$loadings
#Sum of Squares of factor loadings
eig7_load <- sum((fa7_load)^2)
Group_Scores <- cbind(Group_Scores, fa7$scores)

colnames(Group_Scores) <- c("ProviderID","Mortality_Score", "Readmission_Score","Safety_Score","PatientExperience_Score","Imaging_Score","Timely_Score","Effective_Score")

##### Calculating Hospital Score using Weights
Group_Scores$HospitalScore <- ( 0.22 * Group_Scores$Mortality_Score + 0.22 * Group_Scores$Readmission_Score 
                              + 0.22 * Group_Scores$Safety_Score + 0.22 * Group_Scores$PatientExperience_Score
                              + 0.04 * Group_Scores$Imaging_Score + 0.04 * Group_Scores$Timely_Score + 0.04 * Group_Scores$Effective_Score)


plot_groupscore <- merge(Group_Scores,Hospital_Rating, by.x = "ProviderID", by.y = "Provider.ID" , all=TRUE)
p1<-ggplot(data = plot_groupscore,aes(y=Mortality_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Mortality_Score")
p2<-ggplot(data = plot_groupscore,aes(y=Readmission_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Readmission_Score")
p3<-ggplot(data = plot_groupscore,aes(y=Safety_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Safety_Score")
p4<-ggplot(data = plot_groupscore,aes(y=PatientExperience_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="PatientExperience_Score")
p5<-ggplot(data = plot_groupscore,aes(y=Imaging_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(-1,1))+labs(x="Hospital rating", y="Imaging_Score")
p6<-ggplot(data = plot_groupscore,aes(y=Timely_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="Timely_Score")
p7<-ggplot(data = plot_groupscore,aes(y=Effective_Score,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+coord_cartesian(ylim=c(-3,3))+labs(x="Hospital rating", y="Effective_Score")
p8<-ggplot(data = plot_groupscore,aes(y=HospitalScore,x=factor(Hospital.overall.rating)))+geom_boxplot(na.rm=TRUE)+labs(x="Hospital rating", y="HospitalScore")


multiplot(p1,p2,p3,p4,cols = 2)
multiplot(p5,p6,p7,p8,cols = 2)

################################################
######## Implementing K-Means algorithm ########
################################################

clust <- kmeans(Group_Scores[-1], centers = 5, iter.max = 120, nstart = 50)

Group_Scores_km <-cbind(Group_Scores,clust$cluster)

colnames(Group_Scores_km)
colnames(Group_Scores_km)[10]<- "PredictedRating"

Group_Scores_km$ActualRating <- as.factor(Hospital_Rating$Hospital.overall.rating)

Group_Scores_km2 <- Group_Scores_km

#Removing the data with Null Rating values, as it is not possible to compare the results.
Group_Scores_km2 <- na.omit(Group_Scores_km2)


############ Model Evaluation ############
table(Group_Scores_km2$ActualRating, Group_Scores_km2$PredictedRating)

conf_final <- confusionMatrix(Group_Scores_km2$ActualRating, Group_Scores_km2$PredictedRating)
conf_final

########################################################################################################
################ Analysis and recomendations for a particular provider #################################
########################################################################################################

### Cluster Analysis
km_clusters<- group_by(Group_Scores_km, PredictedRating)
tab1<- summarise(km_clusters, Mortality_mean=mean(Mortality_Score), Readmission_Mean=mean(Readmission_Score), 
                 Safety_Mean=mean(Safety_Score),Experience_Mean=mean(PatientExperience_Score),
                 Imaging_Mean=mean(Imaging_Score), Timely_Mean = mean(Timely_Score), Effective_Mean=mean(Timely_Score))

#Mean of different group scores based on the cluster that they belong to.
ggplot(tab1, aes(x= factor(PredictedRating), y=Mortality_mean)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(PredictedRating), y=Readmission_Mean)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(PredictedRating), y=Safety_Mean)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(PredictedRating), y=Effective_Mean)) + geom_bar(stat = "identity")

########################## group score details of Provider with ID = 140010. ##########################
Group_Scores_km[Group_Scores_km$ProviderID==140010,]