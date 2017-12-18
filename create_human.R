#Final assignment. Data wrangling. Álvaro Germán Torres Mora; alvaro.torres@helsinki.fi. December 18, 2017
#The data set has information obtained by the United Nations Development Program (UNDP) on various relevant topics of Human development such as Education, labour, political participation, mortality and the like. The data set contains 8 variables and 155 observations

#2. Read the "Human development" and "Gender inequality" datas into R. Here are the links to the datasets:
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")
#3. Explore the datasets: see the structure and dimensions of the data. Create summaries of the variables
str(hd)
dim(hd)
str(gii)
dim(gii)
summary(hd)
summary(gii)
#4.Look at the meta files and rename the variables with (shorter) descriptive names
colnames(hd) <- c("HDIR", "CTRY", "HDI", "LIF.EXP.BIRTH", "EXPECT.EDUCATION", "MEANEDUCATION", "GNI.PC", "GNI-HDIR")
colnames(gii) <- c("GIIR", "CTRY", "GII", "MAT.MORT.RATIO", "ADOL.BIRTH.RATE", "REPRF.PARLIAMENT", "POP.SEC.EDUC.FEM", "POP.SEC.EDUC.MAL", "LAB.FOR.PART.FEM", "LAB.FOR.PART.MAL")
str(hd)
str(gii)
#5.Mutate the "Gender inequality" data and create two new variables
gii <- mutate(gii, EDUCATION.RATIO = POP.SEC.EDUC.FEM / POP.SEC.EDUC.MAL) 
gii <- mutate(gii, LABOUR.RATIO = LAB.FOR.PART.FEM / LAB.FOR.PART.MAL) 
#6.Join together the two datasets using the variable Country as the identifier
join_by <- c("CTRY")
human <- inner_join(hd, gii, by = join_by)
str(human)
dim(human)
write.csv(human, file = "human.csv", row.names = FALSE)


#1. Mutate the data: transform the Gross National Income (GNI) variable to numeric 
library(stringr)
library(dplyr)
str(human$GNI.PC)
GNIDEF <- str_replace(human$GNI.PC, pattern=",", replace ="") %>% as.numeric
human <- mutate(human, GNIDEF)
str(human$GNIDEF)

#2. Excluding unneeded variables

keep <- c("CTRY", "EDUCATION.RATIO", "LABOUR.RATIO", "LIF.EXP.BIRTH", "EXPECT.EDUCATION", "GNIDEF", "MAT.MORT.RATIO", "ADOL.BIRTH.RATE", "REPRF.PARLIAMENT")
human <- select(human, one_of(keep))

#3.Removing all rows with missing values

complete.cases(human)
data.frame(human[-1], comp = complete.cases(human))
human_ <- filter(human, complete.cases(human))

#4. Removing the observations which relate to regions instead of countries
tail(human_, n=10)
last <- nrow(human) -7
human_ <- human_ [1:155,]

#5. Defining the row names of the data by the country names and removing the country name column from the data
rownames(human_) <- human_$CTRY
human_ <- select(human_, -CTRY)
str(human_)

#Saving:
write.table(human_, file = "human_.csv", sep = ",", row.names = TRUE, col.names = TRUE)
