library(readxl)
data <- read_excel("Dropbox/ILO/BLOK 3/ontwerponderzoek/VR in de klas/data/data2.xlsx", 
                   sheet = "Blad1", col_types = c("skip", 
                                                  "skip", "skip", "skip", "text", 
                                                  "text", "numeric", "skip", "skip", 
                                                  "skip", "numeric", "skip", "skip", 
                                                  "numeric"), n_max = 155)
data$GROEP <- as.factor(data$GROEP)

#NA verwijderen
data <-  data[!(data$t0=="" | is.na(data$t0) | is.na(data$t1G)),]

#Niveau kolom aanmaken
data$niveau[data$Studie=="E6-3V"] <- "VWO"
data$niveau[data$Studie=="T3-3V"] <- "VWO"
data$niveau[data$Studie=="E6-3H"] <- "HAVO"
data$niveau[data$Studie=="T3-3H"] <- "HAVO"

data$niveau <- as.factor(data$niveau)

#data backup maken
write.csv(data, file = "VRdata.csv")

View(data)
summary(data)

#Twee nieuwe dataframes op niveau (havo, vwo)
vwo <- subset(data, niveau=="VWO")
summary(vwo)
havo <- subset(data, niveau=="HAVO")
summary(havo)

library(car)
leveneTest(data$t0,data$GROEP,center = mean)
leveneTest(data$t0,data$GROEP,center = median)

# Boxplots voor de nulmeting
boxplot(t0~GROEP,data=data, main="Nulmeting", 
        xlab="groep", ylab="score per groep",col= rainbow(3))

# Boxplot voor de score op de vegetatievraag
boxplot(data$t1V~data$GROEP, 
        main="Boxplot voor de de score op Vegetatiezone vraag per groep", 
        xlab="groep", 
        ylab="score per groep",
        col= rainbow(3), 
        horizontal = FALSE)

# Boxplot voor de score op de geomorfologievraag
boxplot(data$t1G~data$GROEP, 
        main="Boxplot voor de de score op Geomorfologie vraag per groep", 
        col= rainbow(3), 
        horizontal = FALSE)

library(kableExtra)
library(psych)

#beschrijving van de statistiek op nulmeting
describeBy(data$t0,group = data$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore nulmeting") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#beschrijving van de statistiek op Vegetatiescore
describeBy(data$t1V,group = data$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore vegetatiezonevraag") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#beschrijving van de statistiek op de Geomorfologiescore
describeBy(data$t1G,group = data$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore geomorfologievraag") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#beschrijving van de statistiek op de geomorfologie voor VWO
describeBy(vwo$t1G,group = vwo$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore geomorfologie voor VWO") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#beschrijving van de statistiek op de geomorfologie voor HAVO
describeBy(havo$t1G,group = havo$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore geomorfologie voor HAVO") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#beschrijving van de statistiek op de vegetatie voor VWO
describeBy(vwo$t1V,group = vwo$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore vegetatie voor VWO") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#beschrijving van de statistiek op de vegetatie voor HAVO
describeBy(havo$t1V,group = havo$GROEP, mat = TRUE) %>% #create dataframe
  select(Groep=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis) %>% 
  kable(align=c("lrrrrrrrr"), digits=2, row.names = FALSE,
        caption="Statistiek van de Toetsscore vegetatie voor HAVO") %>% 
  kable_styling(bootstrap_options=c("bordered", "responsive","striped"), full_width = FALSE)

#one way ANOVA
summary(aov(t1V~GROEP, data))
summary(aov(t1G~GROEP, data))

summary(aov(t1V~GROEP, vwo))
summary(aov(t1V~GROEP, havo))
summary(aov(t1G~GROEP, vwo))
summary(aov(t1G~GROEP, havo))


#histogram t0
hist(data$t0[data$GROEP=="GEO"], main='Nulmeting groep G',xlab = "score", xlim= c(0,6),col = "green")
hist(data$t0[data$GROEP=="VEG"], main='Nulmeting groep V',xlab = "score", xlim= c(0,6),col = "blue")
hist(data$t0[data$GROEP=="CONTROLE"], main='Nulmeting controlegroep',xlab = "score", xlim= c(0,6),col = "red")



