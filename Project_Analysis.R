data<-read.csv('./TrainingData.csv')

#Handling missing data
data["Speed"][data["Speed"] == ""] <- 0

data

library("ggplot2")

n = 20000
ci = 0.95
alpha = 0.05

###########################################################################################################

print("Q.1")
#Hypothesis Description

print("Null Hypothesis: ") 
print("Alternate Hypothesis: ") 





###########################################################################################################

print("Q.2")
#Hypothesis Description

print("Null Hypothesis: All variances for all accelerometers are same") 
print("Alternate Hypothesis: All variances for all accelerometers are not the same") 

sample <- data[sample(1:nrow(data), size=n),]

ax <- sample$Acceleromete_X
ay <- sample$Acceleromete_Y
az <- sample$Acceleromete_Z

rc <- sample$Road_Condition

ax_anova <- aov(rc~ax, data = sample)

summary (ax_anova)

ay_anova <- aov(rc~ay, data = sample)

summary (ay_anova)

az_anova <- aov(rc~az, data = sample)

summary (az_anova)

print("Since F value of accelerometer is highest, it's readings better explain the road conditions - Null Hypothesis rejected")

###########################################################################################################

print("Q.3")
#Hypothesis Description

print("Null Hypothesis: Bad road conditions have nothing to do with high speed") 
print("Alternate Hypothesis: Bad road conditions are related to high speeds")


ggplot(data, aes(x = Road_Condition, y = Speed)) +
  geom_boxplot(fill = "green", colour = "black") +
  scale_x_discrete() + xlab(" Group") +
  ylab("Speed")


#Alternatively

table(data$Road_Condition)

speed<-data$Speed
rc<- data$Road_Condition

table(rc)[1]

group_by_speed <- aggregate(speed, list(rc), mean, na.rm=TRUE)

typeof(rc)

bc <- data[data$Road_Condition >= 3,]
gc <- data[data$Road_Condition <= 2,]

t.test(bc$Speed, gc$Speed, mu = 0, alt = "greater", conf = 0.95)

#########################################################################################




###########################################################################################################

print("Q.4")
#Hypothesis Description

library(stringr)


TrainingData <- read.csv("./TrainingData.csv")
sample_size = 40000

######################### Question 4 ###########################################

data <- TrainingData[sample(1:nrow(TrainingData),size=sample_size),]


#data$Timer[1]
#minutes =str_extract(data$Timer[1], "([0-9]+):")
#minutes<- as.numeric(substr(minutes,1,1))
#minutes
#minutes*60

#seconds <-as.numeric(substr(str_extract(data$Timer[1], ":([0-9]+)"),2,3))
#seconds



################################# Preprocessing ################################
seconds_array = c()

for (x in 1:nrow(data)) {
  minutes =str_extract(data$Timer[x], "([0-9]+):")
  minutes<- as.numeric(substr(minutes,1,1))
  minutes <-minutes*60
  
  seconds <-as.numeric(substr(str_extract(data$Timer[1], ":([0-9]+)"),2,3))
  
  
  seconds_array[x]=minutes + seconds
}
seconds_array

################################################################################

#### H0: There variance for all groups is same
#### HA: The variance for atleast one group is not same

### Alpha = 0.05 


model <- lm (seconds_array~as.factor(data$Road_Condition))
summary(aov(model))

#### P value is 2e-16 which is less than 0.05 so therefore we reject the null hypothesis
### Therefore we can say there's some correlation between Time of Recording and Road Condition


###########################################################################################################

print("Q.5")
#Hypothesis Description

print("Null Hypothesis: ") 
print("Alternate Hypothesis: ") 


sample <- data[sample(1:nrow(data), size=n),]
sample = na.omit(sample)


ax <- sample$Acceleromete_X
ay <- sample$Acceleromete_Y
az <- sample$Acceleromete_Z
Gx <- sample$Gyroscope_X
Gy <- sample$Gyroscope_Y
Gz <- sample$Gyroscope_Z
gx <- sample$gravity_X
gy <- sample$gravity_Y
gz <- sample$gravity_Z
mx <- sample$Magnetic_X
my <- sample$Magnetic_Y
mz <- sample$Magnetic_Z
ox <- sample$Orientation_X
oy <- sample$Orientation_Y
oz <- sample$Orientation_Z
rc <- sample$Road_Conditio

ax_anova <- aov(rc~ax, data = sample)

res_ax = summary (ax_anova)

res_ax

ay_anova <- aov(sample$Road_Condition~ay, data = sample)

res_ay = summary (ay_anova)

res_ay

az_anova <- aov(sample$Road_Condition~az, data = sample)

res_az = summary (az_anova)

res_az

Gx_anova <- aov(rc~Gx, data = sample)

res_Gx = summary (Gx_anova)

res_Gx

sample

Gy_anova <- aov(sample$Road_Condition~Gy, data = sample)

res_Gy = summary (Gy_anova)

res_Gy

Gz_anova <- aov(sample$Road_Condition~Gz, data = sample)

res_Gz = summary (Gz_anova)

res_Gz


gx_anova <- aov(sample$Road_Condition~gx, data = sample)

res_gx = summary (gx_anova)

res_gx

gy_anova <- aov(sample$Road_Condition~gy, data = sample)

res_gy = summary (gy_anova)

res_gy

gz_anova <- aov(sample$Road_Condition~gz, data = sample)

res_gz = summary (gz_anova)

res_gz


mx_anova <- aov(sample$Road_Condition~mx, data = sample)

res_mx = summary (mx_anova)

res_mx

my_anova <- aov(sample$Road_Condition~my, data = sample)

res_my = summary (my_anova)

res_my

mz_anova <- aov(sample$Road_Condition~mz, data = sample)

res_mz = summary (mz_anova)

res_mz


ox_anova <- aov(sample$Road_Condition~ox, data = sample)

res_ox = summary (ox_anova)

res_ox

oy_anova <- aov(sample$Road_Condition~oy, data = sample)

res_oy = summary (oy_anova)

res_oy

oz_anova <- aov(sample$Road_Condition~oz, data = sample)

res_oz = summary (oz_anova)

res_oz



print("Since F value of accelerometer is highest, it's readings better explain the road conditions - Null Hypothesis rejected")






