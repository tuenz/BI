install.packages("readxl")
library(readxl)
data <- read_excel(path = "fev.xlsx")

data
names(data)
str(data)

data$Sex <- factor(data$Sex)
data$Smoker <- factor(data$Smoker)

find_na <- function(data){
  sum =0
  for(i in 1:nrow(data)){
    for (j in 1:ncol(data))
    {
      sum<-sum+is.na(data[i,j])
    }
  }
  print(sum)
}

find_na(data)


install.packages("ggplot2")
library(ggplot2)

plot0 <- ggplot(data = data) +
  geom_bar(aes(x = "", fill = Sex)) +
  coord_polar(theta = "y")+
  theme_void() +
  labs(title = "Gender distributions")

plot0

plot00 <- ggplot(data = data) +
  geom_bar(aes(x = "", fill = Smoker)) +
  coord_polar(theta = "y")+
  theme_void() +
  labs(title = "Gender distributions")

plot00


plot1 <- ggplot(data = data) + 
  geom_point(mapping=aes(x = Age, y = FEV), color = "blue", size = 2)+
  theme_dark() + 
  labs(x = "Возраст", y = "ОФВ",title = "Зависимость между возрастом и ОФВ")
plot1

plot11 <- ggplot(data = data) + 
  geom_point(mapping=aes(x = Height, y = FEV), color = "red", size = 2, shape=2)+
  theme_light() + 
  labs(x = "Рост", y = "ОФВ",title = "Зависимость между ростом и ОФВ")
plot11

plot12 <-ggplot(data = data) + geom_point(mapping=aes(x = Age, y = FEV, color = Smoker, shape = Sex ),size=2)+
  labs(x = "Возраст", y = "ОФВ", title = "Зависимость между возрастом и ОФВ в зависимости от пола и поля Smoker")+
  theme_light() + 
  facet_grid(Sex ~ Smoker)
plot12

plot13 <- ggplot(data=data) + 
  stat_summary(mapping=aes(x = Age, y = FEV, color = Sex),fun = "mean", geom = "line", size = 2) + 
  facet_wrap( ~ Smoker)+
  theme_light()
plot13

########################################################часть 2-3
plot2 <- ggplot(data = data,aes(x=Smoker,fill=Sex))+
  geom_bar(position='dodge')+
  theme_light()
plot2

#plot30 <- ggplot(data = data, aes(x = "", y = Age, fill = Smoker)) + geom_violin() + 
 # labs(title = "Распределение количества курильщиков по возрасту", x = "Количество", y = "Возраст")+
  #scale_y_continuous(breaks= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19, 20))
#plot30


plot31 <- ggplot(data=data, aes(x = Age, fill = Smoker)) + geom_bar() +   facet_grid(. ~ Smoker)
plot31

plot32 <- ggplot(data=data, aes(x = Age, fill = Smoker)) + geom_bar() +   facet_grid(. ~ Sex)
plot32

#plot12 <- ggplot(data=data, aes(x = FEV, fill = Smoker)) + geom_histogram(binwidth = 1) +   facet_grid(Sex ~ .)
#plot12
