title: "HW 5 447"
output:
  pdf_document: default
html_document: default

#5.4)

#a.

##H0: There is no significance in interaction between Feed Rate and Depth of Cut
##Ha: There is significance 

dataset = read.table("5.4447.txt" , header = TRUE)
dataset
dataset$FeedRate=as.factor(dataset$FR); #factor A; a levels
dataset$DepthCut=as.factor(dataset$DC); #factor B; b levels
summary(dataset)

dataset.aov=aov(SF ~ DC* FR,dataset)
summary(dataset.aov) ; #P-values for part D. 

##p-val is less than alpha, thus we can say both the main effects and interaction effect were significant. 

with(dataset, interaction.plot(FR,DC,SF,type="b",pch=19, fixed=T,xlab="Feed Rate",ylab="Average Surface Finish"))
with(dataset, interaction.plot(DC,FR,SF,type="b",pch=19, fixed=T,xlab="Depth of Cut",ylab="Average Surface Finish"))

#b.

plot(dataset.aov)

##residual points are condensed closely along the line. Thus it is clear to say that the distribution appears normal. 

#c.

##FR @ .20 
dataset.20=subset(dataset, FR==.20)
dataset.20
mm=with(dataset.20, aggregate(SF,list(FR=FR),mean))
mm

##FR @ .25
dataset.25=subset(dataset, FR==.25)
dataset.25
mm.25=with(dataset.25, aggregate(SF,list(FR=FR), mean))
mm.25

##FR @ .30
dataset.30=subset(dataset, FR==.30)
dataset.30
mm.30=with(dataset.30, aggregate(SF,list(FR=FR), mean))
mm.30

#d. 

##P-values already listed in Part a.

#5.5)

alpha=0.05
a=3 
b=4
n=3
meanFR.20=mean(dataset.20$SF)
meanFR.25=mean(dataset.25$SF)
MSE.FR=summary(dataset.aov)[[1]][4,3]
SE.FR=qt(1-alpha/2,a*b*(n-1))*sqrt(2*MSE.FR/n)
mean.DIF=meanFR.20-meanFR.25
FR.ci=c(mean.DIF-SE.FR, mean.DIF+SE.FR)

#5.7)

#a. 
alpha = .05
dataset2 = read.table("5.7447.txt" , header = TRUE)
dataset2

##H0 CC: Copper Content percentage is not significant to affect warping 
##Ha CC: "" is significant 


dataset2$CopperContent=as.factor(dataset2$CopperContent)


##H0 Temp: Temperature is not significant to affect warping
##Ha Temp: "" is significant

dataset2$Temperature=as.factor(dataset2$Temperature)

summary(dataset2)

##H0: There is no interaction between the factors
##Ha: "" is interaction 

dataset2.aov=aov(Warping ~ CopperContent * Temperature, dataset2)
summary(dataset2.aov)

##P-val. for CC is less than alpha, thus we reject H0. Concluding there is evidence that Copper Content percentage is indeed significant

##P-val. for Temperature is also less than alpha, thus we reject H0. Confirming also that there is significance for Temperature effect. 

##P-val. for the interaction = 0.13275, thus we fail to reject H0. Concluding that there is no interaction between the factors. 

#b. 

plot(dataset2.aov)

##residual points are further away from the line. Thus it is clear to say that the distribution does not appear normal. 

#c. 

TukeyHSD(dataset2.aov,which="CopperContent")

##The average amounts of warping for 60% and 80% copper content do not significantly differ from each other.
##Whereas the average amount of warping with 40% and 100% copper content differ the most from all the other CC levels.
##if low warping is desirable, I would seek 40% of CC because it has the lowest mean warping. 

#d. 

##It does not change our answer; temperature can fluctuate as much as it wants to but the CC percentage stays the same so you're bound to have a lesser warp if you use a lesser CC percentage.

#5.22)
##want to define a vector list like list=c(1,2), then want to define a new column with i.e. Block=c(rep(list, 6)). I think this would give a vector matching trial 1, trial 2, and then it should apply that pattern 6 times to make a total of 12 entries for the block column. Code is not knitting for some reason on this problem. 
