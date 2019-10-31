#loading data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv",header =  TRUE) #original file, will be using test.Survived for Analysis


#New dataframe, Adds a column,"Survived" to "train.csv" ,using this for analysis
test.Survived <- data.frame(Survived = rep("None",nrow(test)), test[,] )

data.combined <- rbind(train, test.Survived)#combined train and test.Survived

#str(data.combined)#Gives us the structure of the obj,can use summary() as well,different use, more stat

data.combined$Survived <- as.factor(data.combined$Survived) #converts Survived into a factor
data.combined$Pclass <- as.factor(data.combined$Pclass)#converts Pclass into a factor

#table(data.combined$Survived) # table() can show us how the data spread
#table(data.combined$Pclass) shows 1st,2nd,3rd class spread


#--------------

library(ggplot2)#loads ggplot2 


#Hypothesis, Rich people(Pclass= 1) have a higher survival rate

train$Pclass <- as.factor(train$Pclass) #Converts Pclass to factor || Can also use factor()

#ggplot example
# ggplot(data  ,aes(x,y) ) 
# 
# 

ggplot(train, aes(x=Pclass, fill =factor(Survived))) + geom_bar(width = .5) +
  xlab("Passenger Class") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Plot shows that the UpperClass had a higher percentage survive than other classes  
#------------------------------------------------------

# head(as.character(train$Name))  Will output the first few Names
# tail(as.character(train$Name)) Outputs last few names    as.character(), on the fly data transformation

#How many unique names in data.combined
length( unique(as.character(data.combined$Name)) )
#data.combined has 1309 rows but only 1307 unique names, so we gotta find the duplicates


dup_names <- as.character((data.combined [ which(duplicated(as.character(data.combined$Name))),"Name" ]) )
#dup_names has a list of characters, that are duplicate names|| which() returns the indexes that are true

#Compare dup_names with data.combined to look at the duplicate names
data.combined[  which(data.combined$Name %in% dup_names) , ] #displays the duplicates
#after looking at the data, doesnt seem to be bad data, continue  
#------------------------------------

#If we look at the names we see that there is the prefix of Miss,Mrs,Mr. Perhaps there is some useful
# data in this column, could gain some valuable insight

library(stringr)

#Contains a list of all Misses,  str_detect finds "Miss" in data.combined$Name, 
#Misses <- data.combined[which(str_detect(data.combined$Name, "Miss"))   ,]

#males <- data.combined[which(data.combined$Sex == "male"),] finds data that are Sex=="male" makes new dataframe

#--------------------------
#Now we want to expand on the relationship between Survived and PClass as well as adding
#a "Title" column based on the name(Miss,Mr,etc), exploring a potential 3 dimensional relationship


#Creates a utility function for Title extraction
#takes in name and return Title(Mr. , Mrs. , etc )
extractTitle <- function(Name1){
  Name <- as.character(Name1)
  
  #grep() returns 0 or 1
  if( length(grep("Miss.", Name)) >0){
    return("Miss.")
  }
  else if (length(grep("Mrs.",Name)) > 0){
    return("Mrs.")
  }
  else if (length(grep("Mr.",Name)) > 0){
    return("Mr.")
  }
  else if (length(grep("Master.",Name)) > 0){
    return("Master.")
  }
  else{
    return("Other")
  }
  
}

Titles <- NULL #Temp 
for(i in 1:nrow(data.combined)) {  #for loop,goes through data.combined, gets the title,adds it to Title
  Titles<- c(Titles, extractTitle(data.combined[i,"Name"]) ) #c(list,value) will add value to list
}
data.combined$Title <- as.factor(Titles) #creates new column in data.combined, adds temp data


#We only have Survived data for the first 891 rows, the rest need to be "predicted"
#Thus only use the first 891 rows
ggplot(data.combined[1:891,],aes(x=Title,fill= Survived)) + geom_bar(width = .5) +
  xlab("Passenger Title") + ylab("Total Count") + ggtitle("Passenger Class") + 
  labs(fill="Survived",subtitle = "Red=Dead || Green=Survived") + facet_wrap(~Pclass)
  




#3 way relationship between PClass, Survival, and Sex
#Only using Row1-891 because We dont know have survival data from that point on
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived)) +
  geom_bar(width=.5) +xlab("Sex") + ylab("Total Count") +labs(fill ="Survived")+ ggtitle("Pclass")+
  facet_wrap(~Pclass)


#Basic Stats Summary
summary(data.combined$Age)
summary(data.combined$Age[1:891])
#From this we learn that we are missing 263 passengers age,177 are from the train dataset

ggplot(data.combined[1:891,], aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 10) + xlab("Age") +ylab("Total Count")+labs(fill="Survived")+
  ggtitle("Pclass Gender") + facet_wrap(~Pclass + Sex)


#Attempting to use Title as a "proxy" for the missing ages
boys <- data.combined[which(data.combined$Title=="Master."),]
summary(boys$Age)

#Taking a look at the Misses age distribution
misses <- data.combined[which(data.combined$Title=="Miss."),]
summary(misses$Age)

#Plot of Age Dist. of Misses by PClass
ggplot(misses[misses$Survived != "None",], aes(x=Age, fill = Survived))+
  facet_wrap(~Pclass)+ geom_histogram(binwidth = 10) +
  ggtitle("Age Distribution for Misses by PClass")+xlab("Age")+ylab("Total Count")

#Appears that female children have a distinct survival rate ,
#Possible option to add to our feature engineering 

#New table: Misses that are traveling alone (No Sibling/Spouse, Parents/Children)
misses.alone <- misses[which(misses$SibSp==0 & misses$Parch ==0),]
summary(misses.alone$Age)

#We see that only 4 misses traveling alone are under this age
#14.5 was the age most of the boys(Master) were under so it is safe to assume that if a Miss
# is traveling alone then she is most likley older(26ish median age)
length(which(misses.alone$Age <= 14.5))


summary(data.combined$SibSp)
#Heavily skewed to 0, most have 0 siblings/spouse
#Lets see if we can turn this into a factor
unique(data.combined$SibSp) # We only have 7 values so i think we can turn this into a factor
#making it a factor allows visualization and analysis to be easier+ more effective 

ggplot(data.combined[1:891,], aes(x=SibSp, fill = Survived))+
  geom_histogram(binwidth = 1) + facet_wrap(~Pclass + Title)+
  ggtitle("PClass, Title") + xlab("SibSP")+ylab("Total Count")+
  labs(fill = "Survived")

#We will use the same idea for the "parch" variable
ggplot(data.combined[1:891,], aes(x=Parch, fill = Survived))+
  geom_histogram(binwidth = 1) + facet_wrap(~Pclass + Title)+
  ggtitle("PClass, Title") + xlab("Parch")+ylab("Total Count")+
  labs(fill = "Survived")

#Feature Engineering for family size
temp.Sibsp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)

#Creating a family size feature (includes the passenger)
data.combined$family.size <- as.factor(temp.Sibsp + temp.Parch +1)

ggplot(data.combined[1:891,], aes(x=family.size , fill = Survived))+
  geom_bar() + facet_wrap(~Pclass + Title)+
  ggtitle("PClass, Title") + xlab("Family Size")+ylab("Total Count")+
  labs(fill = "Survived")


#Exploring the Ticket Variable
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
head(data.combined$Ticket)

#Lets explore and see if we can find any structure in the ticket format

ticket.first.char <-  ifelse(data.combined$Ticket== ""," ",   substr(data.combined$Ticket,1,1))
length( unique(ticket.first.char)) #We get 16 unique first char
#making this var into a factor for visualization/analytical purposes
data.combined$ticket.first.char <- as.factor(ticket.first.char)

#Visualizing the First Char from Ticket
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill = Survived))+
  geom_bar() + 
  ggtitle("Survival Rates by ticket.first.char") + xlab("Ticket First Character")+ylab("Total Count")+
  labs(fill = "Survived")+ facet_wrap(~Pclass)
#it looks like there is no structure to ticket 



# Exploring ticket fares
str(data.combined$Fare)
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined[1:891,], aes(x=Fare,fill=Survived))+
  geom_bar(width = 10) + facet_wrap(~Pclass+Title)+
  ggtitle("Fare distribution by Title/Class") + xlab("Ticket Fare")+ylab("Total Count")+ 
  labs(fill = "Survived") + ylim(0,50)
 


#Analyzing cabin variable
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:15] #Look at the first few --> We see alot of empty values

#replaces all of our empty values with "C" as a filler value 
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "Z"
data.combined$Cabin[1:15]


# Looking at the first letter of the cabin variable
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))#substring from character 1 to 1
str(cabin.first.char)
levels(cabin.first.char) # We find that we have 9 levels 

data.combined$cabin.first.char <- cabin.first.char #add to data.combined for plotting



#Looking at survivability by the first letter of the Cabin variable
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar(width = 1) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Now we will look at the Cabin surviviability grouped by PClass
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char grouped by PClass") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Checking our Embarked Variable
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Testing for a trend between Embarked and Survivability grouped by Pclass and title
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

