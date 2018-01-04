#====================================================
#     -----List of packages explored-----
#1.dplyr  
#2.data.table
#3.ggplot2
#4.reshape2
#5.readr
#6.tidyr
#7.lubridate
#8.purrr
#9.stringr
#10.broom
#11.tibble
#==================1.dplyr===================================
#filter#select#arrange#mutate#summarise(with group_by).
library(dplyr)
data("mtcars")
data("iris")
mydata<-mtcars
head(mydata)
#creating a local dataframe
mynewdata<-tbl_df(mydata)
myirisdata<-tbl_df(iris)#table is in tabular structure

filter(mynewdata,cyl>4 & gear>4)
filter(mynewdata,cyl>4)
filter(myirisdata,Species %in% c('setosa','virginica'))

select(mynewdata,cyl,mpg,hp)
select(mynewdata,-cyl,-mpg)
select(mynewdata,-c(cyl,mpg))
select(munewdata,cyl:gear)

#chaining or pipelining
mynewdata%>%select(cyl,wt,gear)%>%filter(wt>2)
mynewdata%>%select(cyl,wt,gear)%>%arrange(wt)
mynewdata%>%select(cyl,wt,gear)%>%arrange(desc(wt))
#mutate-creating new columns
mynewdata%>%select(mpg,cyl)%>%mutate(newvariable=mpg*cyl) #or
newvariable<-mynewdata%>%mutate(newvariable=mpg*cyl)
#summarize-to find insights from data
myirisdata%>%group_by(Species)%>%summarise(Average=mean(Sepal.Length,na.rm=TRUE))
myirisdata%>%group_by(Species)%>%summarise_each(funs(mean,n()),Sepal.Length,Sepal.Width)
#rename
mynewdata%>%rename(miles=mpg)


#================2.data.table=========================
#fast manipulation compared to data.frame
#DT[i,j,by] subset rows using i to calculate j which is grouped by 
data("airquality")
mydata<-airquality
head(airquality,6)
data(iris)
myiris<-iris
library(data.table)
mydata<-data.table(mydata)
myiris<-data.table(myiris)
mydata[2:4,]
myiris[Species=="setosa"]
myiris[Species%in%c('setosa','virginica')] 
mydata[,Temp]
mydata[,.(Temp,Month)]
#returns sum of selected column
mydata[,sum(Ozone,na.rm=TRUE)]
mydata[,.(sum(Ozone,na.rm=TRUE),sd(Ozone,na.rm=TRUE))]
#print aand plot
myiris[,{print(Sepal.Length)
plot(Sepal.Width)
NULL}]
#grouping by a variable
myiris[,.(sepalsum=sum(Sepal.Length)),by=Species]
#select a column for computation,hence need to set the key on column
setkey(myiris,Species)
#select all the rows associaed with this data point
myiris['setosa']
myiris[c('setosa','virginica')]


#==================3.ggplot2==========================
#can be more powerful if grouped with cowplot,gridExtra
library(ggplot2)
library(gridExtra)
library(cowplot)
df<-ToothGrowth
df$dose<-as.factor(df$dose)
head(df)
#boxplot
bp<-ggplot(df,aes(x=dose,y=len,col=dose))+
geom_boxplot()+
theme(legend.position='none')
bp
#add grid lines
bp+background_grid(major="xy",minor="none") #can use theme() instead
#scatterplot
sp<-ggplot(mpg,aes(x=cty,y=hwy,color=factor(cyl)))+
geom_point(size=2.5)
sp
#barplot
bp<-ggplot(diamonds,aes(clarity,fill=cut))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=70,vjust=0.5))
#compare two plots
plot_grid(sp,bp,labels=c("A","B"),ncol=2,nrow=1)
#histogram
ggplot(diamonds,aes(x=carat))+
  geom_histogram(binwidth=0.25,fill='steelblue')+
  scale_x_continuous(breaks=seq(0,3,by=0.5))


#=====================4.reshape=========================
#melt and cast
ID<-c(1,2,3,4,5)
Names<-c('Ram','Shyam','Ghanshyam','Radheshyam','jaishyam')
DateofBirth<-c(1993,1992,1993,1994,1992)
Subject<-c('Maths','Biology','Science','Psychology','Physics')
thisdata<-data.frame(ID,Names,DateofBirth,Subject)
data.table(thisdata)
library(reshape2)
mt<-melt(thisdata,id=(c('ID','Names')))
mt
mcast<-dcast(mt,DateofBirth+Subject~variable)
mcast


#==================5.readr============================
#helps read various forms of data in R
#Delimated files with: read_delim(),read_csv(),read_tsv(),read_csv2()
#Fixed width files:read_fwf(),read_table()
#Web log files with: read_log()
library(readr)
read_csv('test.csv',col_names=TRUE)
read_csv("iris.csv",col_types=list(
  Sepal.Length=col_double(),
  Sepal.Width=col_double(),
  Petal.Length=col_double(),
  Petal.Width=col_double(),
  Species=col_factor(c("setosa","versicolor","virginica"))))
#in you omit column,it will take care of it automatically
read_csv("iris.csv",col_types=list(Species=col_factor(c("setosa","versicolor","virginica"))))
#Note: write_csv is a lot faster than write.csv()


#===================6.tidyr=============================
#tidyr and dplyr go hand in hand
#gather,spread(),separate(),unite()
library(tidyr)
names<-c('A','B','C','D','E','A','B')
weight<-c(55,49,76,71,65,44,34)
age<-c(21,20,25,29,33,32,38)
Class<-c('Maths','Science','Social','Physics','Biology',
         'Economics','Accounts')
tdata<-data.frame(names,age,weight,Class)
tdata
long_t<-tdata%>%gather(Key,Value,weight:Class)
#---------------------------------------------
Humidity<-c(37.79,42.34,52.16,44.57,43.83,44.59)
Rain<-c(0.97,1.10,1.064475,0.95318,0.988,0.9396)
Time<-c("27/01/2017 15:44","23/02/2017 23:24",
        "31/03/2017 19:17","20/01/2017 08:45","23/02/2017
        07:46","31/01/2017 01:55")
d_set<-data.frame(Humidity,Rain,Time)
d_set
#separate function to separate date,month,year
separate_d<-d_set%>%separate(Time,c('Date','Month','Year'))
separate_d
#unite-reverse of separate
unite_d<-separate_d%>%unite(Time,c(Date,Month,Year),
                            sep="/")
unite_d
#spread function - reverse of gather
wide_t<-long_t%>% spread(Key,Value)
wide_t


#==================7.lubridate======================
#make easy parsing in date and time.
#update,duration,date extraction functions.

library(lubridate)
n_time<-now()
n_update<-update(n_time,year=2013,month=10)
n_update
#add days,months,year,seconds
d_time<-now()
d_time+ddays(1)
d_time+dweeks(2)
d_time+dyears(3)
d_time+dhours(2)
d_time+dminutes(50)
d_time+dseconds(30)
#extract date,time
n_time$hour<-hour(now())
n_time$year<-year(now())
n_time$month<-month(now())
n_time$second<-second(now())
#check the extracted dates in separate column
new_data<-data.frame(n_time$hour,n_time$second,
                     n_time$month,n_time$year)
new_data
#for best use all the pakages in conguction 


#==================8.purrr============================
library(purrr)
my_list<-list(
  c(1,2,6),
  c(4,7,1),
  c(9,1,5)
)
#find mean of each element
my_list[[1]] %>% mean() # (1+2+6)/3=3
my_list[[2]] %>% mean() #same as mean(my_list[[1]])
my_list[[3]] %>% mean()
#Alternate option
my_list%>%map(mean)   #same as mean(my_list,mean)
#specific map
my_list%>%map_int(mean) #we except result in int
my_list%>%map_dbl(mean) #double since we dnt know result is always integer
my_list%>%map(~.*2) #own function. . stand for elements in the list.
#other maps
my_list%>%map(is.numeric) #T T T
my_list%>%map_lgl(is.numeric) #T T T
my_list%>%map_char(is.numeric)#"T","T","T"
my_list%>%map_int(is.numeric)#1,1,1
#==================9.stringr==========================
library(stringr)
str_trim("  Hello World   ")
#[1] "Hello World"
str_pad("old",width=3,side="left",pad="g")
#[1] "old"
toupper("Hello Word")
#[1] "HELLO WORD"
tolower("Hello Word")
#[1] "hello word"
x<-c(1:10,20,30)
x[!x %in% boxplot.stats(x)$out] #to remove outliers
#[1]  1  2  3  4  5  6  7  8  9 10
#-------------------------------------------------------
today<-as.Date(Sys.Date()) #enters todays date
xmas<-as.Date("2017-12-25")
daysleft<- today-xmas
daysleft #9 days on 2108-01-03
format(xmas,"%d/%m/%y")  #"25/12/17"

#%d day of month(0-31)
#%a abbreviated day of week
#%m month as a number(1-12)
#abbreviate name of month
#%B full name of month
#%y two digit year
#%Y four digit year

#===============10.broom===================================
#Three ways we can get information out of model objects
#tidy() : component-level statistic
#augment(): observation-level statistics (example: residuals,fitted vaues)
#glance(): model-level statistics (only one value for the component) )

#Ojects of models are messy
library(broom)
data("mtcars")
names(mtcars)
lmfit<-lm(mpg~wt+qsec,mtcars) #linear fit
summary(lmfit)  #messy
tidy(lmfit)
augment(lmfit) #each row is an observation from the original data
glance(lmfit) #always one row data frame
#broom takes model objects and turns them into 
#tidy data frames that can be used with tidy tools.
#can be used with many models#
#Means you can take data and manuplate it plot it by dplyr and ggplot2


#==========================11.tibble===============================================
tdf=tibble(x=1e2,y=rnorm(1e2)) #creates dataframe ==data_frame(x=1:e4,y=rnorm(1e4))
#they print nicely
tdf
print(tbl,n=30) #controlls only 30 rows
#does not do partial matching. Base R does
#type consistency i.e it returns always same type
#==================================================================================



