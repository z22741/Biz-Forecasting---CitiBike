rm(list=ls()) 

library(fpp2)
library(fpp)
library(gtools)
library(ggplot2)
library(zoo)

# Load Data
bike_data = data.frame(matrix(ncol = 2, nrow = 0))
for (i in c(2014:2019)){
  for (j in c(1:4)){
    # import file
    file_name = paste0("Dataset/", i, " - Q", j, ".csv")
    if(!file.exists(file_name)){
      break()
    }
    temp_df = read.csv(file_name)
    bike_data = smartbind(bike_data, temp_df)
  }
}
# Check NA value
sum(is.na(bike_data$Date))

# Change "Date" from factor to Date
df_len = length(bike_data$Date)
bike_data$Date = as.character(bike_data$Date)
bike_data$Date = c(
  as.Date(bike_data$Date[1:912], format = "%m/%d/%Y"),
  as.Date(bike_data$Date[913:1004], format = "%m/%d/%y"),
  as.Date(bike_data$Date[1005:1080], format = "%m/%d/%Y"),
  as.Date(bike_data$Date[1081:1096], format = "%m/%d/%y"),
  as.Date(bike_data$Date[1097:1461], format = "%m/%d/%y"),
  as.Date(bike_data$Date[1462:1551], format = "%m/%d/%Y"),
  as.Date(bike_data$Date[1552:1826], format = "%m/%d/%y"),
  as.Date(bike_data$Date[1827:df_len], format = "%m/%d/%Y")
  ) 
# Check outlier of Date
nrow(bike_data[ bike_data$Date > "2019-03-31",])
d <- try( as.Date( bike_data$Date, format= "%Y-%m-%d" ) )
if( class( d ) == "try-error" || is.na( d ) ) print( "That wasn't correct!" )

# Keep Date & 24H Pass
keep_col_pass = c(c(colnames(bike_data)[1], colnames(bike_data)[8]))
pass_bike_data = bike_data[ , (names(bike_data) %in% keep_col_pass)]

# Keep total membership
member = bike_data[, c(colnames(bike_data)[1],
                        colnames(bike_data)[6],
                        colnames(bike_data)[10],
                        colnames(bike_data)[11])]
names(member) = c("Date", "Mem1", "Mem2", "Mem3")
member[is.na(member)] = 0
member$Total_mem = member$Mem1 + member$Mem2 + member$Mem3
member = member[, c("Date", "Total_mem")]
# Check outlier of member
summary(member)
ggplot(data = member, aes(Date, Total_mem), size = 1) +
  geom_point() 
member[ member$Total_mem > 10^6,]
# Fixed outlier
member[ member$Total_mem > 10^6, "Total_mem" ] = 147199
summary(member)
# Change col name
col_name = c("Date", "Pass_24H")
names(pass_bike_data) = col_name
 
# Create time series data
ts_bike = ts(zoo(pass_bike_data$Pass_24H, order.by=pass_bike_data$Date),  frequency=1)
autoplot(ts_bike)


# Trend on membership
member_M = member %>% group_by(Date=floor_date(Date, "month")) %>%
  summarize(Total_mem=sum(Total_mem))
member_W = member %>% group_by(Date=floor_date(Date, "week")) %>%
  summarize(Total_mem=sum(Total_mem))

ggplot(data = member_M, aes(Date, Total_mem/1000)) +
  geom_point(size = 0.5, colour = "gray30")+
  geom_line(colour = "steelblue")

ggplot(data = member_W, aes(Date, Total_mem/100)) +
  geom_line()
