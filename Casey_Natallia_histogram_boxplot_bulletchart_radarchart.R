library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(dplyr)
library(ggExtra)
library(lattice)
#install.packages("reshape2")
library(reshape2)
#histogram

lattice.options(axis.padding=list(factor=0.5))
df<-read.csv('birth-rate.csv')
head(df)
my.settings <- list(par.main.text = list(just = "left", col = 'dimgray',x = grid::unit(5, "mm")), axis.line = list(col='transparent'))
histogram(df$X2008,breaks=10,  xlab = "Births per 1,000 population",ylab = "Percent of total(%)",border = 'white',par.settings=my.settings, main="DISTRIBUTION OF BIRTH RATES, 2008",col = c("orange"))

#boxplot

df1<-read.csv('crimeratesbystate-formatted.csv')
head(df1)
myvars <- c("state", "robbery", "burglary")
df1 <- df1[myvars]
head(df1)
df1<-melt(data = df1, id.vars = c("state"), measure.vars = c("burglary", "robbery"))
head(df1)
ggplot(df1, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot(outlier.size = 2.5)+
  theme_classic()+
  scale_fill_manual(values=c("gray", "orange"))+ 
    ggtitle("\nBURGLARY AND ROBBERY DISTRIBUTIONS\n")+
  ylab("Rates ( per 100,000 population)")+
  theme_bw()+  removeGrid()+
  theme(panel.border = element_blank(), title=element_text(color = 'dimgray'), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 11), axis.title.y=element_text(size=12, color = 'dimgray'))+
  theme(axis.line = element_line(color = 'white'))+
  theme(axis.title.x = element_blank())

#bulletplot

library(plotly)
library("readxl")
df4<-read_excel('sales_devices.xlsx')
head(df4)
df41<-df4[df4$devices == 'alarm clock', ] 
head(df41)
df42<-df4[df4$devices == 'camera', ] 
head(df42)
df43<-df4[df4$devices == 'printer', ] 
head(df43)
t <- list(
  family = "sans serif",
  size = 20,
  color = 'dimgray', 
  adj=0)
fig <- plot_ly() 
fig <- fig %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = sum(df43$actual_sales),
    delta = list(reference = sum(df43$expected_sales)),
    domain = list(x = c(0.25, 1), y = c(0.08, 0.29)),
    title =list(text = "printer sales"),
    gauge = list(
      shape = "bullet",
      axis = list(range = c(0, 500)),
      threshold = list(
        line= list(color = "black", width = 2),
        thickness = 0.55,
        value = sum(df43$expected_sales)),
      steps = list(
        list(range = c(0, sum(df43$actual_sales)*60*0.01), color = "gray"),
        list(range = c(sum(df43$actual_sales)*60*0.01, sum(df43$actual_sales)*80*0.01), color = "lightgray")),
      bar = list(color = "orange"))) 

#radar chart

fig <- fig %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = sum(df42$actual_sales),
    delta = list(reference = sum(df42$expected_sales)),
    domain = list(x = c(0.25, 1), y = c(0.4, 0.6)),
    title = list(text = "camera sales"),
    gauge = list(
      shape = "bullet",
      axis = list(range = list(NULL, 500)),
      threshold = list(
        line = list(color = "black", width= 2),
        thickness = 0.55,
        value = sum(df42$expected_sales)),
      steps = list(
        list(range = c(0, sum(df42$actual_sales)*60*0.01), color = "gray"),
        list(range = c(sum(df42$actual_sales)*60*0.01, sum(df42$actual_sales)*80*0.01), color = "lightgray")),
      bar = list(color = "orange"))) 
fig <- fig %>%
  add_trace(
    type =  "indicator",
    mode = "number+gauge+delta",
    value = sum(df41$actual_sales),
    delta = list(reference = sum(df41$expected_sales)),
    domain = list(x = c(0.25, 1), y = c(0.7, 0.9)),
    title = list(text = "alarm clock sales"),
    gauge = list(
      shape = "bullet",
      axis = list(range = list(NULL, 500)),
      threshold = list(
        line = list(color = "black", width = 2),
        thickness = 0.55,
        value = sum(df41$expected_sales)),
      steps = list(
        list(range = c(0, sum(df41$actual_sales)*60*0.01), color = "gray"),
        list(range = c(sum(df41$actual_sales)*60*0.01, sum(df41$actual_sales)*80*0.01), color = "lightgray")),
      bar = list(color = "orange"))) 
fig<-fig %>%
  layout(
    title="\nACTUAL VS EXPECTED DEVICE SALES",
    font = t

  
)
fig

# RADAR 
#install.packages('fmsb')

library(fmsb)
df6<-read.csv('crimeratesbystate-formatted.csv')
head(df1)
myvars <- c( 'state','murder', 'robbery', 'aggravated_assault',
             'burglary', 'larceny_theft', 'motor_vehicle_theft')
df6 <- df6[myvars]
df6<-df6 %>% filter(state == 'United States ')
df6
my<-c( 'murder', 'robbery', 'aggravated_assault',
      'burglary', 'larceny_theft','motor_vehicle_theft')
df6 <- df6[my]
df6
df6 <- rbind(df6, '2' = c(0,0,0,0,0,0))
df6
df6 <- rbind(df6, '2' = c(2286.3,2286.3,2286.3,2286.3,2286.3, 2286.3))
df6
df6 <- rbind(df6, '2' = c(0,0,0,0,0))
df6 <- rbind(df6, '2' = c(5.6,140.7,291.1,726.7,2286.3,416.7))
df6
newdata <- df6[-c(1, 2), ] 
names(newdata)[names(newdata) == "motor_vehicle_theft"] <- "motor vehicle theft"
names(newdata)[names(newdata) == "larceny_theft"] <- "larceny theft"
names(newdata)[names(newdata) == "aggravated_assault"] <- "aggravated asault"
                         
head(newdata)
radarchart(newdata,  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , 
           seg=5,axistype=1,cglcol="grey", caxislabels=c("0", "500", "1000", "1500","2000", ""),cglty=1,plwd = 2, axislabcol="grey", cglwd=0.5,
           
           #custom labels
           vlcex=0.8 )
mtext(side = 3, line = 2.5, at = 0, cex = 1.75, "CRIME IN THE UNITED STATES", font = 0.1, col = 'dimgray')
