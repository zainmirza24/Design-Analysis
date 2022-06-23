#4.3)
##a
dataset = read.table("4.3447.txt" , header = TRUE)
dataset

##H0: mu1 = mu2 = mu3 = mu4 
##Ha: @ least one is different from rest 

alpha = .05 

##treatment  = Chemical
##blocking = bolt


test.aov = aov(Strength~factor(Bolt) + factor(Chemist), dataset)
summary(test.aov)

##P-val. for chemicals equals .121

##alpha is less than P-val. Because of this, we fail reject the null hypothesis, 
#and can confirm that the chemicals do not affect the strength of the cloth. 

#4.7)
#a.

dataset2 = read.table("4.7447.txt" , header = TRUE)
dataset2

##H0: mu1=mu2=mu3=mu4
##Ha: @ least one is not equal

alpha = .05

##treatment = Tip
##blocking = Coupon 

test.aov = aov(Hardness~factor(Coupon) + factor(Tip), dataset2)
summary(test.aov)

##P-val. for Tip equals .000871

####alpha is greater than P-val. Because of this, we will reject the null 
#hypothesis, and can confirm that the Tip can affect the hardness. 


#b.

#Fisher's LSD method
#four treatments
a=4;b=5;N=20
alpha = .05
ybar.trt = as.vector(with(dataset2, tapply(Hardness, Tip, function(x) mean(x))))
summary(ybar.trt)
MSe = summary(test.aov)[[1]][3,3]
MSe
##H0: mu1 = mu2 
##Ha: mu1 != mu2

t0 = (ybar.trt[1]-ybar.trt[2])/sqrt(2*MSe/b)
t0
pval=2*pt(abs(t0),(a-1)*(b-1), lower.tail=F)
pval

##LSD is less conservative, but more powerful. 
#LSD test compares one pairwise mean whereas Tukey's method compares all 
#pairwise means. 

##Since alpha is less than pval, we will fail to reject the null hypothesis, 
#accepting H0. Thus we conclude there is strong evidence the means are equal 
#for Tip 1 and Tip 2

##H0: mu1 = mu3
##Ha: mu1 != mu3

t0 = (ybar.trt[1]-ybar.trt[3])/sqrt(2*MSe/b)
t0
pval=2*pt(abs(t0),(a-1)*(b-1), lower.tail=F)
pval

##Since alpha is less than pval, we will fail to reject H0, Thus we can conclude 
#that there is significant evidence to claim the means are equal for Tip 1 and Tip 3.


##H0: mu1 = mu4
##Ha: mu1 != mu4

t0 = (ybar.trt[1]-ybar.trt[4])/sqrt(2*MSe/b)
t0
pval=2*pt(abs(t0),(a-1)*(b-1), lower.tail=F)
pval

##Since alpha is greater than pval, we will reject H0, Thus we can conclude that 
#there is significant evidence to claim the means are not equal for Tip 1 and 
#Tip 4.

##H0: mu2 = mu3
##Ha: mu2 != mu3

t0 = (ybar.trt[2]-ybar.trt[3])/sqrt(2*MSe/b)
t0
pval=2*pt(abs(t0),(a-1)*(b-1), lower.tail=F)
pval

##Since alpha is greater than pval, we will  reject H0, Thus we can conclude 
#that there is significant evidence to claim the means are not equal for Tip 2 
#and Tip 3.


##H0: mu2 = mu4
##Ha: mu2 != mu4

t0 = (ybar.trt[2]-ybar.trt[4])/sqrt(2*MSe/b)
t0
pval=2*pt(abs(t0),(a-1)*(b-1), lower.tail=F)
pval

#Since alpha is greater than pval, we will  reject H0, Thus we can conclude 
  #that there is significant evidence to claim the means are not equal for Tip 2 
  #and Tip 4.


##H0: mu3 = mu4
##Ha: mu3 != mu4

t0 = (ybar.trt[3]-ybar.trt[4])/sqrt(2*MSe/b)
t0
pval=2*pt(abs(t0),(a-1)*(b-1), lower.tail=F)
pval

##Since alpha is greater than pval, we will  reject H0, Thus we can conclude 
  #that there is significant evidence to claim the means are not equal for Tip 3 
  #and Tip 4.

#c. 

test.lm=lm(Hardness~factor(Coupon)+factor(Tip),data=dataset2)
test.lm
RES=residuals(test.lm)
RES
library(car)
qqPlot(RES)

##residuals 

plot(fitted(test.lm), RES)
plot(dataset2$Tip, RES)
plot(dataset2$Coupon, RES)


##Based on the qqPLOT, we can see that our points are distant from the line. 
#Thus we can confirm our data is not normally distributed.

#4.13)
#a. 

##H0: ratio control does not affect the average cell voltage. 

##Ha: ratio control does affect average cell voltage.

dataset3= read.table("4.13447.txt" ,  header=TRUE)
dataset3
test2.aov=aov(Average~factor(TimePeriod)+factor(Algorithm), dataset3)
summary(test2.aov)

##p-val. is greater than alpha(.05), thus we fail to reject H0. 

#b. 

test2.lm=lm(Average~factor(TimePeriod)+factor(Algorithm),data=dataset3)
test2.lm
RES=residuals(test2.lm)
RES
library(car)
qqPlot(RES)

##residual plots

plot(fitted(test2.lm), RES)
plot(dataset3$Algorithm, RES)
plot(dataset3$TimePeriod, RES)

##Based on the qqPLOT, we can see our residual points are distant from the line. 
#Thus we can confirm our data is not normally distributed in this table.   

#c. 

##I would select algorithm 2 because it has the lowest amount of pot noise. 
#Our ratio control already has little to no effect on the average cell voltage.

#4.18)
#a.
##Code not knitting
###Missingchemial=dataset 
###Missingchemical["8", "Strength"]= NA
##x=predict(aov(Strength~factor(Bolt)+factor(Chemist),Missingchemical),newdata=dataset["8", ])
###x


#b. 
##Code not knitting 
###I need to do a summary(aov)

#4.22)
##H0: mu1=mu2=...=mu5
##Ha: @ least one mu !=

ing=read.table("4.22447.txt", sep = ",",  header= TRUE)
ing
##summary(aov(Time~ factor(Batch) + factor(Day) + Catalyst, ing)) ---> knitting issues

##alpha is greater than p-val. Thus we reject H0, and confirm there is difference between the Catalyst.







