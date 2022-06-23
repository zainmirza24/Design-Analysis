---
  title: "HW 6 447"
output:
  pdf_document: default
html_document: default

#6.1)

life = c(22,31,25,32,43,29,35,34,50,55,47,46,44,45,38,40,37,36,60,50,54,39,41,47)
A = rep(c("-","-","-","+","+","+" ), 4)
B = rep(c("-","+","-","+"), each=6)
C = rep(c("-","+"), each=12)
data = data.frame(life, A, B, C)
#recode the data: x=1 at level +; X=-1 at level -
coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}

#a. 
data1 = within(data, {cA=coded(A); cB=coded(B); cC=coded(C)}) #adding three columns
life.lm = lm(life~cA*cB*cC, data1)
summary(life.lm)
2*coef(life.lm)[-1] #main effect
##we can see that main effect cB and cC are large, as well as the interaction effect between cA and cC. 

#b.
life.aov = aov(life ~ A*B*C, data)
summary(life.aov) #ANOVA test; P-val's for B,C + interaction A:C are less than alpha 

#c. 
life.lm1 = lm(life ~ cB+cA*cC, data1)
life.lm1 #regression line prediction; formula as follows: 
##40.833 + 5.667(cB) + .167(cA) + 3.417(cC) - 4.417(cA)(cC) = life
summary(life.lm1)

#d. 
res = data$life-fitted(life.lm1)
res
library(car)
qqPlot(res) #substantial normal distribution 
plot(fitted(life.lm1), res) #random distribution, no curvature + equal variance 

#e. 
with(data, interaction.plot(A, C, life))
with(data, interaction.plot(B, A, life)) 
##We want the highest response(life), therefore A-:C+ and A-:B+ are what I would recommend 

#6.3)

mse = summary(life.aov)[[1]][8,3]
n=3;a=b=c=2;N=a*b*c*n
alpha=0.05
sqrt(mse/N)
#consturct CI for regression coefficient
se=sqrt(mse/N)
df=a*b*c*(n-1)
hat.beta1=life.lm$coefficients[4] #2A 3B 4C 5AB 6AC 7BC 8ABC
CI.beta=hat.beta1+c(-1,1)*qt(alpha/2,df,lower.tail = F)*se
CI.beta
2*CI.beta #CI for main effect A
##the results of this analysis agree with the conclusions from the analysis of variance. 

#6.5)

#a.
library(readxl)
data = data.frame(read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Math 447 D&A/6.5.xlsx"))
data
coded=function(x)
{
  ifelse(x==1,"+","-")
}
data1 = within(data,{cA=coded(Bit.Size);cB=coded(Cutting.Speed)})
data1.aov = aov(Vibration~cA*cB,data1)
summary(data1.aov) 
##p-val. for cA, cB, and interaction between cA and cB are all less than alpha, thus we can reject H0, confirming Bit Size, Cutting Speed, and the interaction effect are all significant. 

#b.
plot(data1.aov)
##The qqPlot shows a normal distribution based on a majority of the points aligned along the dotted diagonal line in the graph. 
##In the Residuals vs Fitted plot, we can see there isn't any unusual patterns, bell shape curves, etc. Thus our scatter plot is also normal.

#c. 
with(data1,interaction.plot(cA,cB,Vibration))
##We want a small vibration since high vibration can cause damage. Using this information I believe it is best to cA - and cB +. 

#6.6)

getwd()
life1 = c(22,32,35,55,44,40,60,39)
A = rep(c("-","-","-","+","+","+" ), 4)
B = rep(c("-","+","-","+"), each=6)
C = rep(c("-","+"), each=12)
data1 = data.frame(life1, A, B, C)
#recode the data: x=1 at level +; X=-1 at level -
coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}

#a. 
data2 = within(data1, {cA=coded(A); cB=coded(B); cC=coded(C)}) #adding three columns
life.lm2 = lm(life1 ~ cA*cB*cC, data2)
summary(life.lm2)
2*coef(life.lm2)[-1] #main effect
##we can see that main effect cB and cC are large, the interaction effect between cA and cC, and the interaction effect between cA:cB:cC. 

#b. 

data3 = read.table("6.6.txt", header = T)
SSpq=((8)*(4)*(40.875-41)^2)/(8+4)
SSpq
#full model
data3.aov=aov(Life ~ A * B * C + I(A^2)+I(B^2)+I(C^2), data3)
summary(data3.aov)
#effects B, C, AC are significant at 5%. There is no effect of curvature.

#c. 
life.lm3 = lm(life1 ~ cB+cC+cA*cC, data2)
life.lm3 #regression line prediction; formula as follows: 
##40.875 + 2.1250(cB) + .2083(cA) + 1.625(cC) - 4.0417(cA)(cC) = life
summary(life.lm3)


#d. 
res = data3$Life-fitted(life.lm3)
res
qqPlot(res) #substantial normal dist. 
plot(fitted(life.lm3), res) #random dist. and no curvature

#6.22)

#a.
data4 = read.table("6.22.txt", header = T)
library(ggplot2)
data4.aov = aov(UEC ~ LaserPower + PulseFrequency + CellSize + WritingSpeed, data4)
summary(data4.aov)
data4.lm <- lm(UEC ~ LaserPower + PulseFrequency + CellSize + WritingSpeed, data4)
summary(data4.lm)
##all except Pulse Frequency seem to be significant to the affect of our UEC. 

#b. 
library(car)
res.6.22 <- data4$UEC - fitted(data4.lm)
qqPlot(res.6.22) 
plot(fitted(data4.lm), res.6.22)
par(mfrow = c(1, 3))
plot(data4$LaserPower, res.6.22, xlim = c(-2, 2))
plot(data4$CellSize, res.6.22, xlim = c(-2, 2))
plot(data4$WritingSpeed, res.6.22, xlim = c(-2, 2))
par(mfrow = c(1,1)) 
##model looks adequate 

#6.25)

data5 = data.frame(read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Math 447 D&A/6.25.xlsx"))
data5
coded=function(x)
{
  ifelse(x==1,"+","-")
}
z = within(data5,{cA=coded(Temperature);cB=coded(Pressure);cC=coded(Concentration);cD=coded(Stirring.Rate)})
z.aov=aov(Filtration.Rate ~ cA*cB*cC*cD, z)
summary(z.aov)
z.lm=lm(Filtration.Rate~Temperature*Pressure*Concentration*Stirring.Rate,data5)
summary(z.lm)
summary(aov(Filtration.Rate ~ (cA+cB+cC+cD)^2, z)) #sparsity principal
#half normal probability plot
install.packages("gplots", repos = "http://cran.us.r-project.org")
library("gplots")
qqnorm(z.aov,label=T)
#normal plot
qqnorm(z.aov,full=T)
z.aov1=aov(Filtration.Rate ~ cA*cC*cD, z) #stat testing
summary(z.aov1) #refined model
z.lm2=lm(Filtration.Rate~Temperature+Concentration+Stirring.Rate+Temperature*Concentration*Stirring.Rate, data5)
summary(z.lm2)
##Based on the graph and plot above, the main effects of cA,cC,cD and cA:cC and cA:cD are significant.
##Because p-value's for  A,C,D,AC,AD are less than alpha (.05), we reject H0, thus they are significant. 

