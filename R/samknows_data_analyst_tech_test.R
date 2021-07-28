library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)


# Reading files ==============================================================
detail_csv <- read.csv(file = 'details_for_each_person.csv')
download_csv <- read.csv("download_speed_measurements.csv")
upload_csv <- read.csv("upload_speed_measurements.csv")

# Q2 - Data Import and Cleaning ==============================================

# 1. Calculate the average download speed for each person
download_df <- download_csv %>% 
  filter(month(time_of_measurement) == 1) %>% 
  group_by(person_id) %>% 
  summarise(average_download_speed = 
              mean(measured_download_speed_in_Mbps, na.rm = T)) %>% 
  ungroup() 

# 2. Calculate the average upload speed for each person
upload_df <- upload_csv %>% 
  filter(did_test_complete_successfully == "TRUE",
         month(time_of_measurement) == 1) %>% 
  group_by(person_id) %>% 
  summarise(average_upload_speed = 
              mean(measured_upload_speed_in_Mbps, na.rm = T)) %>% 
  ungroup()

# 3. Combine detail table, download table and upload table
## filter the city
com_df <- list(detail_csv, download_df, upload_df) %>%
  reduce(left_join, by = "person_id") %>% 
  filter(city %in% c("Samsville", "Databury")) %>% 
  select(c("person_id", "city", "type_of_broadband_connection",
           "name_of_isp", "average_download_speed", "average_upload_speed"))

# Optional ======================================================

# 1. Extract date information from time_of_measurement column
# 2. Calculate the average download speed each day for each person
# 3. Take the 60th percentile
six_per_df <- download_csv %>% 
  filter(month(time_of_measurement) == 1) %>% 
  mutate(date = gsub("T.*", "", time_of_measurement)) %>% 
  group_by(person_id, date) %>% 
  summarise(average_download_speed = 
              mean(measured_download_speed_in_Mbps, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(person_id) %>% 
  arrange(average_download_speed) %>% 
  summarize(percent = quantile(average_download_speed, 0.6))

op1_df <- merge(com_df, six_per_df, by = "person_id")

# output data
# write.csv(op1_df,"output\\data_import_and_cleaning.csv", row.names = FALSE)


# Q3 - Data Quality ==========================================================

# 1. Check city name: there are 2 rows with the city name of Irrelevantsford
diff_city <- detail_csv %>% 
  select(city) %>% count(city)

# 2. Check missing value ========================
## 1. there is no missing value for detail_csv
detail_csv %>% summarise_all(~sum(is.na(.)))

## 2. A function for checking missing value rate
miss_check <- function(df){
  miss_df <- df %>%
    group_by(person_id) %>% 
    summarise_at(vars(-group_cols()), ~ mean(is.na(.))) %>% 
    ungroup()
  return(miss_df)
}

## 3. Missing value rates for upload table and download table
upload_miss <- miss_check(upload_csv)
download_miss <- miss_check(download_csv)

# Output data
# write.csv(upload_miss,"output\\upload_miss_rate.csv", row.names = FALSE)
# write.csv(download_miss,"output\\download_miss_rate.csv", row.names = FALSE)

# 3. Mislabeled data ===========================
## 1.Most of mislabeled data happen in "ADSL" connection type
mislabeled_df <- com_df %>% 
  filter(
    (type_of_broadband_connection == "ADSL" & 
       average_upload_speed >=1 & average_download_speed >= 25) |
      (type_of_broadband_connection == "VDSL" & 
         average_upload_speed >= 20 & average_download_speed >= 100 &
         average_upload_speed < 1 & average_download_speed < 25) |
        (type_of_broadband_connection == "Fibre" & 
           average_upload_speed > 1000 & average_download_speed > 1000 &
           average_upload_speed < 20 & average_download_speed < 100)
    )

## 2.Correct mislabeled data
correct_df <- com_df %>% 
  mutate(type_of_broadband_connection = 
           ifelse(
             average_download_speed < 25 & 
               average_upload_speed < 1, "ADSL", 
             ifelse(
             (average_download_speed >= 25) & 
               (average_download_speed < 100) &
               (average_upload_speed >= 1) & 
               (average_upload_speed < 20), "VDSL", "Fibre"
           )))

# Q4 - Data summary and presentation ==========================================

# a) Summary tables =============================

# 1.Differences in download and upload speed between ISPs
## Useus has higher download and upload speed
isp_diff <- correct_df %>% 
  group_by(name_of_isp) %>%
  summarise(average_download= mean(average_download_speed, na.rm = T),
            average_upload = mean(average_upload_speed, na.rm = T))

# write.csv(isp_diff,"output\\diff_download_and_upload_speed_between_ISPs.csv", row.names = FALSE)

# 2. Differences in download and upload speed between ISPs for 
#    each connection type
## Useus has higher download and upload speed for most of the 
## connection types, except for VDSL.
con_isp_diff <- correct_df %>% 
  select(name_of_isp, type_of_broadband_connection,
         average_download_speed,
         average_upload_speed
  ) %>% 
  group_by(name_of_isp, type_of_broadband_connection) %>%
  mutate(number_of_connection = n()) %>% 
  ungroup() %>% 
  group_by( 
           type_of_broadband_connection, 
           name_of_isp,number_of_connection) %>%
  
  summarise(average_download= mean(average_download_speed, na.rm = T),
            average_upload = mean(average_upload_speed, na.rm = T)) %>% 
  ungroup() 

write.csv(
  con_isp_diff,
  "output\\diff_download_and_upload_speed_between_ISPs_for_connection_type.csv", 
  row.names = FALSE)

# 3. Differences in download and upload speed between ISPs by city
## Fibrelicious in Databury city has higher upload and download speed,
## while Useus in Samsville city has higher upload and download speed.
isp_city <- correct_df %>% 
  group_by(name_of_isp, city) %>%
  summarise(average_download= mean(average_download_speed, na.rm = T),
            average_upload= mean(average_upload_speed, na.rm = T))

write.csv(isp_city,
          "output\\diff_download_and_upload_speed_between_ISPs_by_city.csv", 
          row.names = FALSE)

# 4. Differences in download and upload speed between
#    ISPs by city by connection type
con_isp_city <- correct_df %>% 
    group_by(name_of_isp, type_of_broadband_connection, city) %>%
  summarise(average_download= mean(average_download_speed, na.rm = T),
            average_upload= mean(average_upload_speed, na.rm = T))

write.csv(
  con_isp_city,
  "output\\diff_download_and_upload_speed_between_ISPs_by_city_by_connection_type.csv", 
  row.names = FALSE)

# b) Visualization ==========================

# 1.Creating a grouped and stacked bar chart for the visualization
barwidth = 0.35

# 2. Split data frame by ISP and add a bar layer for each ISP.
split_df <- function(isp){
  df <- con_isp_city %>% filter(name_of_isp == isp) %>% 
    mutate(row_id = ifelse(type_of_broadband_connection == "ADSL", 3,
                           ifelse(type_of_broadband_connection == "VDSL",
                                  2, 1))) %>% 
    group_by(row_id) %>% arrange(desc(city)) %>% 
    mutate(pos = 
             as.integer(cumsum(average_download) -
                          average_download / 2), # calculating label position
           average_download = as.integer(average_download)) %>% 
    ungroup()
  
  # Customize the order of connection type for showing in the bar chart
  df$type_of_broadband_connection<- 
    factor(df$type_of_broadband_connection, 
           levels=c("Fibre", "VDSL", "ADSL"))
  
  return(df)
  
}

fibre <- split_df("Fibrelicious")
useus <- split_df("Useus")

# 3. Findings:
## Bars with black text represent the ISP of Fibrelicious,
## while bars with blue text represent the ISP of Useus.
## Fibrelicious has higher download speed for the connection types of ADSL and VDSL
## Download speed is higher in Samsville city
ggplot() + 
  geom_bar(data = fibre,
           mapping = aes(x = row_id,
                         y = average_download, fill = city),
           stat="identity", 
           position='stack', 
           width = barwidth) +
  geom_text(data = fibre, 
            aes(x = type_of_broadband_connection, 
                y = pos, label = average_download, color = name_of_isp))+
  scale_x_discrete(labels=c("ADSL", 
                            "VDSL", 
                            "Fibre"))+
  geom_bar(data = useus,
           mapping = aes(x = row_id + barwidth + 0.01,
                         y = average_download, fill = city),
           stat="identity", 
           position='stack', 
           width = barwidth)+
  scale_color_manual(values = c("black", "blue")) +
  geom_text(data = useus, 
            aes(x = row_id + barwidth + 0.01, y = pos, 
                label = average_download, color = name_of_isp)) +
    labs(x = "connection type", y = "average download speeds") + 
  theme(panel.background = element_blank()) 
  
  

# c) Live in Databury with a Fibre connection ====

# 1. Fibrelicious will have a better download speed (5.4137 faster), 
#    while Useus will have a better upload speed (4.42237 faster).
db_fib <- con_isp_city %>% filter(city == "Databury" & 
                                type_of_broadband_connection == "Fibre")

# 2. Compare the percentage of faster speed compared to 
#    the mean of upload and download speed
#    Useus is better

abs(diff(db_fib$average_download)) / mean(db_fib$average_download) # 0.04998016
abs(diff(db_fib$average_upload)) / mean(db_fib$average_upload) # 0.1136941

# Optional =====================================================
# 1. Extract time and calculate average download speed
df_1 <- download_csv %>% 
  filter(month(time_of_measurement) == 1) %>% 
  mutate(time = str_match(time_of_measurement, "T\\s*(.*?)\\s*Z")[,2]) %>% 
  group_by(person_id, time) %>% 
  summarise(average_download_speed =
              mean(measured_download_speed_in_Mbps, na.rm = T)) %>% 
  ungroup() 

# 2. Get ISP name from detail_csv
df_2 <- merge(df_1, 
              detail_csv %>% select(person_id, name_of_isp),
              by = "person_id") %>% 
  filter(!is.na(average_download_speed)) %>% 
  select(-person_id) %>% 
  unique()

# 3. The time of day when the download speeds are the highest:
#    Fibrelicious: 17:17:26
#    Useus: 12:26:07
fast <- df_2 %>% 
  group_by(name_of_isp) %>% 
  slice(which.max(average_download_speed))

# write.csv(fast, "output\\highest_download_speed.csv", row.names = FALSE)

# 4. The time of day when the download speeds are the lowest:
## The lowest speed is 0 and it occurs in every hour of a day for both ISPs. 
## Thus the occurrence of different hours is regarded as the 
## standard of the lowest. There are more occurrences at 6 PM,
## which means download and upload speeds are the lowest at 6 PM.
slow <- df_2 %>% 
  filter(average_download_speed == min(average_download_speed)) %>% 
  mutate(hour = hour(hms(as.character(time)))) %>% 
  group_by(name_of_isp) %>% 
  count(hour) %>% 
  slice(which.max(n))

# write.csv(slow, "output\\lowest_download_speed.csv", row.names = FALSE)
  