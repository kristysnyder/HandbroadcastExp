#######Pres/ab species richness data#####
rm(list=ls())
handbroadcast<-read.csv(file = "HandbroadcastData_nativepresab.csv",header = TRUE)

names(handbroadcast)

#take columns in percent with n for row 1
speciesdesignation=handbroadcast[1,]
#take out that row
handbroadcast=handbroadcast[-1,]
names(handbroadcast)
for(i in 9:46) handbroadcast[,i]=as.numeric(handbroadcast[,i])
#for each column with na then make 0
for(i in 9:46) handbroadcast[which(is.na(handbroadcast[,i])),i]=0

names(handbroadcast)

#separate native and nonnatives, creates new columns
handbroadcast$native=rowSums(handbroadcast[,which(speciesdesignation=="n")])
handbroadcast$intro=rowSums(handbroadcast[,which(speciesdesignation=="i")])

#calculate total and percent cover, create new columns
handbroadcast$total=handbroadcast$native+handbroadcast$intro
handbroadcast$nativeprop=handbroadcast$native/handbroadcast$total
handbroadcast$introprop=handbroadcast$intro/handbroadcast$total

#check that new columns have been added
names(handbroadcast)

qqnorm(handbroadcast$nativepercent)
qqline(handbroadcast$nativepercent)
qqnorm(handbroadcast$intropercent)
qqline(handbroadcast$intropercent)

hist(handbroadcast$nativepercent)
hist(handbroadcast$intropercent)
hist(log(handbroadcast$nativepercent))
hist(log(handbroadcast$intropercent))

#Subset june and july surveys
june<-handbroadcast[handbroadcast$Survey=="1",]
july<-handbroadcast[handbroadcast$Survey=="2",]

#plot of native annuals by aspect
library(ggplot2)
#native prop compared by diversity
ggplot(handbroadcast, aes(x = Diversity, y = nativeprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Diversity)))
#native prop compared by biochar
ggplot(handbroadcast, aes(x = Biochar, y = nativeprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Biochar)))
#native prop compared by diversity
ggplot(handbroadcast, aes(x = Annual, y = nativeprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Annual)))
#native prop compared by aspect
ggplot(handbroadcast, aes(x = Aspect, y = nativeprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

#ggplot with interaction
ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = nativeprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Annual)))
ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = nativeprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Survey)))

ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = introprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = introprop)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Survey)))

#########richness models######
library(nlme)

#use model 1
model.1=gls(nativeprop~Diversity*Aspect*Annual, data=handbroadcast)
anova(model.1)

model.2=gls(nativeprop~Diversity*Aspect*Annual*Biochar, data=handbroadcast)
anova(model.2)

model.3=gls(nativeprop~Diversity*Aspect*Annual*Biochar*Survey, data=handbroadcast)
anova(model.3)

AIC(model.1)
AIC(model.2)
AIC(model.3)


#########Handbroad by native/invasive#######
rm(list=ls())
handbroadcast<-read.csv(file = "HandbroadcastData_native.csv",header = TRUE)

names(handbroadcast)

#take columns in percent with n for row 1
speciesdesignation=handbroadcast[1,]
#take out that row
handbroadcast=handbroadcast[-1,]
names(handbroadcast)
for(i in 10:47) handbroadcast[,i]=as.numeric(handbroadcast[,i])
#for each column with na then make 0
for(i in 10:47) handbroadcast[which(is.na(handbroadcast[,i])),i]=0

names(handbroadcast)

#separate native and nonnatives, creates new columns
handbroadcast$native=rowSums(handbroadcast[,which(speciesdesignation=="n")])
handbroadcast$intro=rowSums(handbroadcast[,which(speciesdesignation=="i")])

#calculate total and percent cover, create new columns
handbroadcast$total=handbroadcast$native+handbroadcast$intro
handbroadcast$nativepercent=handbroadcast$native/handbroadcast$total
handbroadcast$intropercent=handbroadcast$intro/handbroadcast$total

#check that new columns have been added
names(handbroadcast)

qqnorm(handbroadcast$nativepercent)
qqline(handbroadcast$nativepercent)
qqnorm(handbroadcast$intropercent)
qqline(handbroadcast$intropercent)



#Subset june and july surveys
june<-handbroadcast[handbroadcast$Survey=="1",]
july<-handbroadcast[handbroadcast$Survey=="2",]



#plot of native annuals by aspect
library(ggplot2)
ggplot(handbroadcast, aes(x = Diversity, y = nativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = nativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = nativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Survey)))

ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = intropercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = intropercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Survey)))

hist(june$annualnativepercent)
ggplot(june, aes(x = interaction(Biochar), y = nativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

ggplot(july, aes(x = interaction(Biochar, Annual), y =annualnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

ggplot(handbroadcast, aes(x = Diversity, y = annualintropercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = Diversity, y = perennialnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = Diversity, y = annualintropercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
########native vs invasive models-using proportion of native####
library(vegan)
library(nlme)
#Use model 17
model.17=gls(nativepercent~Diversity*Aspect*Annual, data=handbroadcast)
anova(model.17)
TukeyHSD(aov(model.17))

model.18=gls(nativepercent~Diversity*Aspect*Annual*Biochar, data=handbroadcast)
anova(model.18)
TukeyHSD(aov(model.18))

model.19=gls(nativepercent~Diversity*Aspect*Annual*Biochar*Survey, data=handbroadcast)
anova(model.19)
TukeyHSD(aov(model.19))

model.20=gls(intropercent~Diversity*Aspect*Annual, data=handbroadcast)
anova(model.20)
TukeyHSD(aov(model.20))

model.21=gls(intropercent~Diversity*Aspect*Annual*Biochar, data=handbroadcast)
anova(model.21)
TukeyHSD(aov(model.21))

model.22=gls(intropercent~Diversity*Aspect*Annual*Biochar*Survey, data=handbroadcast)
anova(model.22)
TukeyHSD(aov(model.22))

# 17 has lowest AIC
AIC(model.17)
AIC(model.18)
AIC(model.19)
AIC(model.20)
AIC(model.21)
AIC(model.22)



##########NMDS by Full Treat########
names(handbroadcast)
speciesgroup=handbroadcast[,10:47]
library(vegan)
species.matrix=as.matrix(speciesgroup)
species_nmds=metaMDS(speciesgroup, k=13, try=20) 
#lowdiversity
adonis(species.matrix~handbroadcast$FullTreat)
plot(species_nmds, type="p")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.L.A.B",display="sites",label=F,col="blue4")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.L.NA.B",display="sites",label=F,col="deepskyblue3")

ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.L.A.NB",display="sites",label=F,col="darkturquoise")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.L.NA.NB",display="sites",label=F,col="darkcyan")

ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.L.NA.B",display="sites",label=F,col="yellow")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.L.A.B",display="sites",label=F,col="darkred")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.L.NA.NB",display="sites",label=F,col="darkorange1")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.L.A.NB",display="sites",label=F,col="purple")

####high diversity
plot(species_nmds, type="p")

ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.H.A.B",display="sites",label=F,col="blue4")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.H.NA.B",display="sites",label=F,col="deepskyblue3")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.H.A.NB",display="sites",label=F,col="darkturquoise")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="1.H.NA.NB",display="sites",label=F,col="darkcyan")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.H.NA.B",display="sites",label=F,col="yellow")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.H.A.B",display="sites",label=F,col="darkred")

ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.H.NA.NB",display="sites",label=F,col="darkorange1")
ordispider(species_nmds,groups=handbroadcast$FullTreat,show.groups="2.H.A.NB",display="sites",label=F,col="purple")


###3D NMDS######
library (vegan3d)
library(geometry)
ordirgl (species_nmds)
orglspider(species_nmds,groups=handbroadcast$Aspect,display="site",label=F, col=(1:4))


#######extra stuff



##########Native or introduced data separated by annual or perennial species######
handbroadcast<-read.csv(file = "HandbroadcastData_nativeann2.csv",header = TRUE)

names(handbroadcast)

#take columns in percent with n for row 1
speciesdesignation=handbroadcast[1,]
#take out that row
handbroadcast=handbroadcast[-1,]
names(handbroadcast)
for(i in 10:47) handbroadcast[,i]=as.numeric(handbroadcast[,i])
#for each column with na then make 0
for(i in 10:47) handbroadcast[which(is.na(handbroadcast[,i])),i]=0

names(handbroadcast)

#separate native and nonnatives, creates new columns
handbroadcast$annualnative=rowSums(handbroadcast[,which(speciesdesignation=="n.a")])
handbroadcast$perennialnative=rowSums(handbroadcast[,which(speciesdesignation=="n.p")])
handbroadcast$annualintro=rowSums(handbroadcast[,which(speciesdesignation=="i.a")])
handbroadcast$perennialintro=rowSums(handbroadcast[,which(speciesdesignation=="i.p")])

#calculate total and percent cover, create new columns
handbroadcast$total=handbroadcast$annualnative+handbroadcast$annualintro+handbroadcast$perennialnative+handbroadcast$perennialintro
handbroadcast$annualnativepercent=handbroadcast$annualnative/handbroadcast$total
handbroadcast$perennialnativepercent=handbroadcast$perennialnative/handbroadcast$total
handbroadcast$annualintropercent=handbroadcast$annualintro/handbroadcast$total
handbroadcast$perennialintropercent=handbroadcast$perennialintro/handbroadcast$total

#check that new columns have been added
names(handbroadcast)

plot(handbroadcast$annualnativepercent)
hist(handbroadcast$annualnativepercent)
hist(log(handbroadcast$annualnativepercent))
hist(sqrt(handbroadcast$annualnativepercent))


qqnorm(handbroadcast$annualnativepercent)
qqline(handbroadcast$annualnativepercent)
qqnorm(handbroadcast$annualintropercent)
qqline(handbroadcast$annualintropercent)

qqnorm(sqrt(handbroadcast$annualnativepercent))
qqline(sqrt(handbroadcast$annualnativepercent))


hist(handbroadcast$annualintropercent)
hist(handbroadcast$perennialnativepercent)
hist(handbroadcast$perennialintropercent)


#sqrt transformation kind of works, many zeros
hist(sqrt(handbroadcast$annualnativepercent))

handbroadcast$sqrtannualnativepercent=sqrt(handbroadcast$annualnativepercent)

#Subset june and july surveys
june<-handbroadcast[handbroadcast$Survey=="1",]
july<-handbroadcast[handbroadcast$Survey=="2",]


#plot of native annuals by aspect
library(ggplot2)
ggplot(handbroadcast, aes(x = Diversity, y = perennialnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = sqrtannualnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = interaction(Annual, Biochar), y = sqrtannualnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Survey)))



hist(june$sqrtannualnativepercent)
ggplot(june, aes(x = interaction(Biochar, Annual), y = sqrtannualnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

ggplot(july, aes(x = interaction(Biochar, Annual), y = sqrtannualnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

####Use this one!!
ggplot(handbroadcast, aes(x = Diversity, y = sqrtannualnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))

ggplot(handbroadcast, aes(x = Diversity, y = annualintropercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = Diversity, y = perennialnativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))
ggplot(handbroadcast, aes(x = Diversity, y = annualintropercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))


########ANOVA of %cover and diversity * aspect######
model.1=lm(annualnativepercent~Diversity*Aspect, data=handbroadcast)
anova(model.1)
TukeyHSD(aov(model.1))

model.2=lm(annualintropercent~Diversity*Aspect, data=handbroadcast)
anova(model.2)
TukeyHSD(aov(model.2))

#sig
model.3=lm(perennialnativepercent~Diversity*Aspect, data=handbroadcast)
anova(model.3)
TukeyHSD(aov(model.3))

model.4=lm(perennialintropercent~Diversity*Aspect, data=handbroadcast)
anova(model.4)
TukeyHSD(aov(model.4))

########ANOVA of %cover and diversity + aspect######

model.5=lm(annualnativepercent~Diversity+Aspect, data=handbroadcast)
anova(model.5)
TukeyHSD(aov(model.5))

model.6=lm(annualintropercent~Diversity+Aspect, data=handbroadcast)
anova(model.6)
TukeyHSD(aov(model.6))

model.7=lm(perennialnativepercent~Diversity+Aspect, data=handbroadcast)
anova(model.7)
TukeyHSD(aov(model.7))

model.8=lm(perennialintropercent~Diversity+Aspect, data=handbroadcast)
anova(model.8)
TukeyHSD(aov(model.8))

##########diversity and aspect only##########

model.9=lm(annualnativepercent~Diversity*Aspect, data=handbroadcast)
summary(model.9)

model.10=lm(annualintropercent~Diversity, data=handbroadcast)
summary(model.10)

model.11=lm(perennialnativepercent~Diversity, data=handbroadcast)
anova(model.11)

model.12=lm(perennialintropercent~Diversity, data=handbroadcast)
anova(model.12)

# with + interaction
model.13=lm(annualnativepercent~Aspect, data=handbroadcast)
summary(model.13)

model.14=lm(annualintropercent~Aspect, data=handbroadcast)
summary(model.14)

model.15=lm(perennialnativepercent~Aspect, data=handbroadcast)
summary(model.15)

model.16=lm(perennialintropercent~Aspect, data=handbroadcast)
summary(model.16)



###########AIC of models
AIC(model.1)
AIC(model.5)
AIC(model.9)
AIC(model.13)

AIC(model.2)
AIC(model.6)
AIC(model.10)
#lowest
AIC(model.14)

#lowest
AIC(model.3)
AIC(model.7)
AIC(model.11)
AIC(model.15)

AIC(model.4)
AIC(model.8)
AIC(model.12)
#lowest
AIC(model.16)




##############ANOVA and TUKEY of treatments########
model1=lm(handbroadcast$annualnativepercent~handbroadcast$FullTreat)
anova(model1)
TukeyHSD(aov(model1))

model2=lm(handbroadcast$annualintropercent~handbroadcast$FullTreat)
anova(model2)
TukeyHSD(aov(model2))

model3=lm(handbroadcast$perennialintropercent~handbroadcast$FullTreat)
anova(model3)
TukeyHSD(aov(model3))


model4=lm(handbroadcast$perennialnativepercent~handbroadcast$FullTreat)
anova(model4)
TukeyHSD(aov(model4))


