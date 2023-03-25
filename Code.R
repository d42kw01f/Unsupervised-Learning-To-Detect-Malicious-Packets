# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Installing packages.
install.packages('tidyverse')
install.packages('moments')
install.packages('scatterplot3d')
install.packages('factoextra')

## Importing packages.
library(tidyverse)
library(ggplot2)
library(factoextra)
library(scatterplot3d)
library(moments)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
### You may need to change/include the path of your working directory
dat <- read.csv("PATH\\MLData2023.csv", stringsAsFactors = TRUE)
### Separate samples of non-malicious and malicious events
dat.class0 <- dat %>% filter(Class == 0) # non-malicious
dat.class1 <- dat %>% filter(Class == 1) # malicious
### Randomly select 300 samples from each class, then combine them to form a working dataset
set.seed(10554442)
rand.class0 <- dat.class0[sample(1:nrow(dat.class0), size = 300, replace = FALSE),]
rand.class1 <- dat.class1[sample(1:nrow(dat.class1), size = 300, replace = FALSE),]
### Your sub-sample of 600 observations
mydata <- rbind(rand.class0, rand.class1)
dim(mydata) # Check the dimension of your sub-sample
str(mydata)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Part 1 

### view `mydata` data frame.
view(mydata)

### Covert `Class` into a catogorical value
mydata$Class <- as.factor(mydata$Class)
str(mydata)

#This for loop answers to part 1 question 1 and question 2
for(i in 1:ncol(mydata)){
  Feature.Name <- names(mydata[i])
  if (unlist(lapply(mydata[i], is.numeric)) == FALSE){
    cat("[*]", Feature.Name, "(Categorial or Binary Feature)")
    cat("\n")
    CategoricalNumbers <- mydata %>%
      pull(i) %>%
      table() %>%
      prop.table() * 100
    print(CategoricalNumbers)
    cat("----------------------------------------------------------------------------------\n")
  }
  else {
    cat("[*]", Feature.Name, "(Numerical Feature)")
    cat("\n")
    Numberic.Value.Summaries <- mydata %>%
      pull(i) %>%
      summary()
    Skweness <- mydata %>%
      pull(i) %>%
      skewness() %>%
      round(4)
    na_sum <- mydata %>% 
      pull(i) %>% 
      is.na() %>% 
      sum()
    cat("\t")
    cat("Min. 1st Qu.  Median    Mean 3rd Qu.    Max.")
    cat("\n")
    cat("\t", Numberic.Value.Summaries)
    cat(" -- ")
    cat("na:", na_sum)
    cat(" -- ")
    cat("Skweness:", Skweness)
    cat("\n----------------------------------------------------------------------------------\n")
  }
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Question 2:
tidy.mydata <- mydata
## IPV6.Traffic ---

### Replace Empty strings with NA values.
tidy.mydata$IPV6.Traffic <- replace(mydata$IPV6.Traffic, mydata$IPV6.Traffic==' ', NA)
tidy.mydata$IPV6.Traffic <- replace(tidy.mydata$IPV6.Traffic, tidy.mydata$IPV6.Traffic=='-', NA)
str(tidy.mydata$IPV6.Traffic)

## But since, this column only contains `FALSE` and `NA` values, it is better to remove the entire column.
tidy.mydata <- mydata[,-3]
str(tidy.mydata)

## Operating.System ---
## Checking th Operating System Column
str(tidy.mydata$Operating.System)

## Replacing the `-` value to NA
tidy.mydata$Operating.System <- replace(tidy.mydata$Operating.System, tidy.mydata$Operating.System=="-", NA)

### Replace '-' strings with NA values.
tidy.mydata$Operating.System <- replace(mydata$Operating.System, mydata$Operating.System==' ', NA)
str(tidy.mydata)

#################################################################################################################################################
#Dealing with Outliers:- 

## OutLiers Presentage Function:
outliersper <- function(x){
  length(which(x >  mean(x) + 3 * sd(x) | x < mean(x) - 3 * sd(x))  ) / length(x)
}

# raw 487 looks like an outlier
ol.num.mydata <- tidy.mydata %>%
  select_if(is.numeric);ol.num.mydata
str(ol.num.mydata)
m.data <- lm(Assembled.Payload.Size~Source.IP.Concurrent.Connection, data=ol.num.mydata)
plot(m.data)

#Dealing with outliers -> Function to find outlier percentage ----
OutlierPerecentage <- function(x){
  Q1 <- quantile(x, 0.25, na.rm=T)
  Q3 <- quantile(x, 0.75, na.rm=T)
  IQR <- Q3 - Q1
  bench <- Q3 + 1.5*IQR
  extreme.threshold.upper <- (IQR * 3) + Q3
  extreme.threshold.lower <- (IQR * 1.5) - Q1
  result1 <- length(x[x > extreme.threshold.upper]);result1
  result2 <- length(x[x < extreme.threshold.lower]);result2
  Num.Outliers <- result1+result2
  Precentage <- (Num.Outliers * 100) / length(x)
  cat('Precentage of outliers -> ', Precentage)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Assembled Payload Size----

#Checking any outliers with boxplot
boxplot(tidy.mydata$Assembled.Payload.Size)
summary(tidy.mydata$Assembled.Payload.Size)

#Get outlier percentage
outliersper(tidy.mydata$Assembled.Payload.Size)
OutlierPerecentage(tidy.mydata$Assembled.Payload.Size)

# remove 800 value
tidy.mydata <- tidy.mydata %>%
  mutate(Assembled.Payload.Size=ifelse(Assembled.Payload.Size<0,NA,Assembled.Payload.Size))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> DYNRiskA.Score----
#try get the idea about download speed and ping time to server.
boxplot(tidy.mydata$DYNRiskA.Score)
summary(tidy.mydata$DYNRiskA.Score)

#Get outlier percentage
outliersper(tidy.mydata$DYNRiskA.Score)
OutlierPerecentage(tidy.mydata$DYNRiskA.Score)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> How.Many.Times.File.Seen----

#Checking any outliers with boxplot
boxplot(tidy.mydata$Response.Size)
summary(tidy.mydata$Response.Size)

#getting the percentage of outliers
outliersper(tidy.mydata$Response.Size)
OutlierPerecentage(tidy.mydata$Response.Size)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Source Ping Time----

#Checking any outliers with boxplot
boxplot(tidy.mydata$Source.Ping.Time)
summary(tidy.mydata$Source.Ping.Time)

#Getting the percentage of outliers
outliersper(tidy.mydata$Source.Ping.Time)
OutlierPerecentage(tidy.mydata$Source.Ping.Time)

#filtering outliers.
tidy.mydata %>%
  filter(Source.Ping.Time>450)

#Get the mean values of the this feature
Mean.Value.Of.Source.Ping.Time <- round(mean(tidy.mydata$Source.Ping.Time),0); Mean.Value.Of.Source.Ping.Time 

#removing values which are more than 450
tidy.mydata <- tidy.mydata %>%
  mutate(Source.Ping.Time=ifelse(Source.Ping.Time>450,Mean.Value.Of.Source.Ping.Time,Source.Ping.Time))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Characters.in.URL----

#boxplot Calls.To.Low.level.System.Libraries
boxplot(tidy.mydata$Connection.Rate)
summary(tidy.mydata$Connection.Rate)

#percentage of outlier
outliersper(tidy.mydata$Connection.Rate)
OutlierPerecentage(tidy.mydata$Connection.Rate)

tidy.mydata %>%
  filter(Connection.Rate>1500)

# There is values in this feature which is higher than other values. However, I don't have enough information to classify as an outlier and remove it.

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Server Response Packet Time(ms)----

#Get the boxplot to see outliers
boxplot(tidy.mydata$Server.Response.Packet.Time)
summary(tidy.mydata$Server.Response.Packet.Time)

#get the percentage of ouliers
outliersper(tidy.mydata$Server.Response.Packet.Time)
OutlierPerecentage(tidy.mydata$Server.Response.Packet.Time)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Packet Size----

boxplot(tidy.mydata$Packet.Size)
summary(tidy.mydata$Packet.Size)
#get the percentage of ouliers
outliersper(tidy.mydata$Packet.Size)
OutlierPerecentage(tidy.mydata$Packet.Size)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Dealing with outliers -> Packet TTL----

boxplot(tidy.mydata$Packet.TTL)
summary(tidy.mydata$Packet.TTL)
#get the percentage of ouliers
outliersper(tidy.mydata$Packet.TTL)
OutlierPerecentage(tidy.mydata$Packet.TTL)

tidy.mydata %>%
  filter(Packet.TTL<40)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Question 2 ----

#Write to a csv file
write.csv(tidy.mydata, "PATH\\mydata.csv")

######################################################### PCA #####################################################################################

#Question 3 ----

# Removing NA Values:
sum(is.na(tidy.mydata))
tidy.mydata <- na.omit(tidy.mydata);sum(is.na(num_tidy_data))

#Extracting only continues values----
num_tidy_data <- tidy.mydata %>%
  select_if(is.numeric) %>%
  cbind(tidy.mydata$Class)

# Changing the column names:
names(num_tidy_data)[names(num_tidy_data) == "tidy.mydata$Class"] <- "Class"

#Check the first few line of the num.mydata dataframe. 
head(num_tidy_data)


#Calculate the PCA with prcomp function.
the.pca <- prcomp(num_tidy_data[, 1:8], scale=TRUE)

#Checking the.pca 
the.pca
str(the.pca)

#Get the summary of the the.pca
summary(the.pca)

#Ladings of the first 4 components
the.pca$rotation[,1:4]
the.pca$scale

## ---------------------------------------------------------------------
# principal components
plot(the.pca, type="l")

#---OR---
fviz_eig(the.pca)

#---OR---
biplot(the.pca, scale = 0)
pca.var <- the.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

#2.3.3. barplot with threshold ----
barplot(pca.var.per, main="The Scree Plot", xlab="Principal Component",
        ylab="Percent Variation") +
  abline(12.5, 0, col = "red") #creating the red line thought the 12.5 value.

fviz_pca_ind(the.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## ---------------------------------------------------------------------

#View the correlation of the extracted numeric data set.
view(cor(num_tidy_data[,1:8]))

# Extract PC scores for better plotting----
num.mydata.pca <- cbind(num_tidy_data, the.pca$x[,1:2])
head(num.mydata.pca)

## ---------------------------------------------------------------------
# Please note that this plot will take sometime to plot.
pairs(num.mydata.pca[,1:8],
      pch=21,
      col=as.numeric(num.mydata.pca$Class)+1,  
      bg=alpha(as.numeric(num.mydata.pca$Class)+1,0.4), 
      cex=1.5,  
      upper.panel=NULL,  
      labels=gsub("[[:punct:]]"," ",colnames(num.mydata.pca[,1:8])))  



## 3D plot with PC1, PC2,PC3
scatterplot3d(the.pca$x[,1:3],pch=21,
              color=as.numeric(num.mydata.pca$Class)+1,
              bg=alpha(as.numeric(num.mydata.pca$Class)+1,0.4),                  
              cex.symbols=2,
              col.grid="steelblue",
              col.axis="steelblue",
              angle=45);


## Biplot for the questioin 5---
## ---------------------------------------------------------------------
ggplot(data=num.mydata.pca, aes(PC1, PC2, col = num.mydata.pca$Class, fill=num.mydata.pca$Class)) +
  stat_ellipse(geom = "polygon", col="black", alpha=0.45) +
  geom_point(shape = 21, col = "black")


fviz_pca_var(the.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
  
fviz_pca_biplot(the.pca,
                axes = c(1,2),   
                col.ind=num.mydata.pca$Class,  
                fill.ind=num.mydata.pca$Class, 
                alpha=0.8, #adding the transparency to the Biplot
                pointsize=4, 
                pointshape=21,# Defining shape of the points
                col.var="red", #adding the color to the Points 
                label="var",  
                repel=TRUE,  #Avoiding the overlapping
                addEllipses=TRUE,  
                legend.title=list(colour="Class",fill="Class",alpha="Class"))
  
