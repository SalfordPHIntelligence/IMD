library(tidyverse)


#data sources are:
#Indices of Deprivation 2025, file 7 all ranks and scores
#Patients registered at a GP Practice. Each quarter a file of LSOA to Practice is released (Jan, Apr, July, Oct)
#download and unzip the file labelled "Patients Registered at a GP Practice - MONTH YEAR: LSOA 2021 (all persons-females-males)"
#https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice
#save in your working directory in a subfolder 'data'

#set up ----

#Change to match the Sub-ICB of interest, these can be found here: https://www.nhsbsa.nhs.uk/sicbls-icbs-and-other-providers/organisation-and-prescriber-changes/sub-icb-locations
subicb<-"01G"
subicb_name<-"Salford"

#change this to match your working directory
setwd("~Primary care_IMD/")

##Read data----
#Read in data - see above
gp_details<-read.csv("data/gp-reg-pat-prac-map.csv")
gp_lsoa<-read.csv("data/gp-reg-pat-prac-lsoa-all.csv")
imd <-read.csv("https://assets.publishing.service.gov.uk/media/691ded56d140bbbaa59a2a7d/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv")

# find ICB code based on sub-ICB code above
icb <- gp_details %>%
  filter(SUB_ICB_LOCATION_CODE == subicb) %>%  
  slice(1) %>%   
  pull(ICB_CODE)

# find ICB name based on sub-ICB code above
icb_name <- gp_details %>%
  filter(SUB_ICB_LOCATION_CODE == subicb) %>%  
  slice(1) %>%   
  pull(ICB_NAME)


#colour palette basedn on that used by MHCLG in IMD release
imd_colours = c("#08407C", "#0968A7", "#298DBC", "#4EB2D2", "#7BCBC5", "#AADCB6", "#CCEAC6", "#E0F4DB", "#EEFCD6", "#FAFCF4")

## Process data ----
#Select and rename IMD2025 fields
names(imd)[1:7] <- c("LSOA_CODE","LSOA_NAME","LA_CODE","LA_NAME","SCORE","RANK","DECILE")
names(imd)[17:19] <- c("HEALTH_SCORE","HEALTH_RANK","HEALTH_DECILE")
imd<-imd%>%
  select(1:7,17:19)

# extract GP practice details date
report_date<-gp_lsoa[2,2]

# unique list of GPs according to PCN
GP_TO_PCN<-gp_details%>%
  select(PRACTICE_CODE, PRACTICE_NAME, PCN_CODE, PCN_NAME)

#Calculate population weighted deprivation for practices and pcns
gp_deprivation<-left_join(gp_lsoa,imd,by="LSOA_CODE")%>%
  left_join(GP_TO_PCN,by="PRACTICE_CODE")%>%
  filter(SCORE!="NA")%>%
  group_by(PRACTICE_CODE)%>% 
  mutate(GP_weighted_IMD = weighted.mean(`SCORE`,`NUMBER_OF_PATIENTS`))%>%
  group_by(PRACTICE_CODE)%>% 
  mutate(GP_weighted_health_IMD = weighted.mean(`HEALTH_SCORE`,`NUMBER_OF_PATIENTS`))%>%
  group_by(PCN_CODE)%>% 
  mutate(PCN_weighted_IMD = weighted.mean(`SCORE`,`NUMBER_OF_PATIENTS`))%>%
  group_by(PCN_CODE)%>% 
  mutate(PCN_weighted_health_IMD = weighted.mean(`HEALTH_SCORE`,`NUMBER_OF_PATIENTS`))%>%
  select(PRACTICE_CODE, `PRACTICE_NAME.x`, PCN_CODE, PCN_NAME, GP_weighted_IMD, GP_weighted_health_IMD, PCN_weighted_IMD, PCN_weighted_health_IMD)

#PCNs within Sub-ICB ----

pcn_deprivation<-gp_deprivation%>%
  select(3:4,7:8)%>%
  distinct(PCN_CODE, .keep_all = TRUE)

gp_deprivation<-gp_deprivation%>%
  distinct(PRACTICE_CODE, .keep_all = TRUE)

#filter for practices in selected sub_ICB
locality_gp_details<-gp_details%>%
  filter(SUB_ICB_LOCATION_CODE == subicb)

#list of practice codes in sub_icb
locality_code<-c(locality_gp_details$PRACTICE_CODE)

#lsoa details for practice codes in sub_icb
locality_gp_lsoa<-gp_lsoa%>%
  filter(PRACTICE_CODE %in% locality_code)%>%
  left_join(imd,by="LSOA_CODE")%>%
  left_join(locality_gp_details,by="PRACTICE_CODE")

#pcn by decile count
pcn<-locality_gp_lsoa%>%
  select(PCN="PCN_NAME",count="NUMBER_OF_PATIENTS",decile="DECILE")%>%
  filter(decile!="NA")%>%
  arrange(decile)


pcn_wide<-pivot_wider(pcn,names_from = "decile",values_from = "count",values_fn=sum)
pcn_long<-pivot_longer(pcn_wide,cols = 2:11, names_to = "decile")
pcn_long$decile <- as.numeric(pcn_long$decile)
pcn_long<-pcn_long%>%
  left_join(pcn_deprivation,by=c("PCN"="PCN_NAME"))


locality_lsoa<-locality_gp_lsoa%>%
  select(PCN="PCN_NAME",count="NUMBER_OF_PATIENTS",decile="DECILE", rank="RANK")%>%
  filter(decile!="NA")

locality_lsoa$decile <- as.numeric(locality_lsoa$decile)
locality_lsoa<-locality_lsoa%>%
  arrange(decile)

##charts ----
pcn_cols<-ggplot(data=pcn_long, aes(x=reorder(PCN,PCN_weighted_IMD), y=value, fill=factor(decile))) +
  geom_col(position = position_fill(reverse = TRUE))+ scale_fill_brewer(palette = "RdYlGn")+coord_flip()+
  labs(x = "PCN: Most deprived at the top",
       y = "Percentage of patients")+
  scale_y_continuous(labels = scales::label_percent())+
  theme(axis.title.y = element_text(vjust= 0.8))+
  labs(fill = "Decile",
       title = paste0("Deprivation by PCN in ",subicb_name), 
       subtitle = paste0("Share of PCN population living in each IMD 2025 deprivation decile: ",report_date),
       caption = "Source: NHS Digital / MHCLG")

pcn_cols
ggsave(paste0(subicb,"_PCN_deciles",report_date,".png"),pcn_cols, units = "cm", width=25, height=10)


pcn_cols2<-ggplot(data=pcn_long, aes(x=reorder(PCN,PCN_weighted_IMD), y=value, fill=factor(decile))) +
  geom_col(position = position_fill(reverse = TRUE))+ scale_fill_manual(values=imd_colours)+coord_flip()+
  labs(x = "PCN: Most deprived at the top",
       y = "Percentage of patients")+
  scale_y_continuous(labels = scales::label_percent())+
  theme(axis.title.y = element_text(vjust= 0.8))+
  labs(fill = "Decile",
       title = paste0("Deprivation by PCN in ",subicb_name), 
       subtitle = paste0("Share of PCN population living in each IMD 2025 deprivation decile: ",report_date),
       caption = "Source: NHS Digital / MHCLG")

pcn_cols2
ggsave(paste0(subicb,"_PCN_deciles_(MHCLG_colours)",report_date,".png"),pcn_cols2, units = "cm", width=25, height=10)



#PCNs within ICB ----

icb_gp_details<-gp_details%>%
  filter(ICB_CODE == icb)

icb_code<-c(icb_gp_details$PRACTICE_CODE)

icb_gp_lsoa<-gp_lsoa%>%
  filter(PRACTICE_CODE %in% icb_code)%>%
  left_join(imd,by=c("LSOA_CODE"="LSOA_CODE"))%>%
  left_join(icb_gp_details,by="PRACTICE_CODE")

icb_pcn<-icb_gp_lsoa%>%
  select(PCN="PCN_NAME",count="NUMBER_OF_PATIENTS",decile="DECILE")%>%
  filter(decile!="NA")%>%
  arrange(decile)


icb_pcn_wide<-pivot_wider(icb_pcn,names_from = "decile",values_from = "count",values_fn=sum)
icb_pcn_long<-pivot_longer(icb_pcn_wide,cols = 2:11, names_to = "decile")
icb_pcn_long$decile <- as.numeric(icb_pcn_long$decile)
icb_pcn_long<-icb_pcn_long%>%
  left_join(pcn_deprivation,by=c("PCN"="PCN_NAME"))


icb_lsoa<-icb_gp_lsoa%>%
  select(PCN="PCN_NAME",count="NUMBER_OF_PATIENTS",decile="DECILE", rank="RANK")%>%
  filter(decile!="NA")

icb_lsoa$decile <- as.numeric(icb_lsoa$decile)
icb_lsoa<-icb_lsoa%>%
  arrange(decile)

##charts----

icb_cols<-ggplot(data=filter(icb_pcn_long,PCN!="Unallocated"), aes(x=reorder(PCN,PCN_weighted_IMD), y=value, fill=factor(decile))) +
  geom_col(position = position_fill(reverse = TRUE))+ 
  scale_fill_brewer(palette = "RdYlGn")+
  coord_flip()+
  labs(x = "PCN: Most deprived at the top",
       y = "Percentage of patients")+
  scale_y_continuous(labels = scales::label_percent())+
  theme(axis.title.y = element_text(vjust= 0.8))+
  labs(fill = "Decile",
       title = paste0("Deprivation by PCN in ",icb_name), 
       subtitle = paste0("Share of PCN population living in each IMD 2025 deprivation decile: ",report_date),
       caption = "Source: NHS Digital / MHCLG")


icb_cols
ggsave(paste0("GM_PCN_deciles",report_date,".png"),icb_cols, units = "cm", width=25, height=25)

icb_cols2<-ggplot(data=filter(icb_pcn_long,PCN!="Unallocated"), aes(x=reorder(PCN,PCN_weighted_IMD), y=value, fill=factor(decile))) +
  geom_col(position = position_fill(reverse = TRUE))+ 
  scale_fill_manual(values = imd_colours)+
  coord_flip()+
  labs(x = "PCN: Most deprived at the top",
       y = "Percentage of patients")+
  scale_y_continuous(labels = scales::label_percent())+
  theme(axis.title.y = element_text(vjust= 0.8))+
  labs(fill = "Decile",
       title = paste0("Deprivation by PCN in ",icb_name), 
       subtitle = paste0("Share of PCN population living in each IMD 2025 deprivation decile: ",report_date),
       caption = "Source: NHS Digital / MHCLG")


icb_cols2
ggsave(paste0("GM_PCN_deciles_(MHCLG_colours)",report_date,".png"),icb_cols2, units = "cm", width=25, height=25)

##table----
PCN_table<-icb_pcn_long%>%
  select(PCN,PCN_CODE,PCN_weighted_IMD,PCN_weighted_health_IMD)%>%
  distinct()%>%
  arrange(-PCN_weighted_IMD)
write.csv(PCN_table,paste0("GM_PCN_IMD_score_",report_date,".csv"),row.names = FALSE)


#GP practices within sub_icb ----


gp<-locality_gp_lsoa%>%
  select(GP="PRACTICE_CODE",count="NUMBER_OF_PATIENTS",decile="DECILE")%>%
  filter(decile!="NA")%>%
  arrange(decile)


gp_wide<-pivot_wider(gp,names_from = "decile",values_from = "count",values_fn=sum)
gp_long<-pivot_longer(gp_wide,cols = 2:11, names_to = "decile")
gp_long$decile <- as.numeric(gp_long$decile)
gp_long<-gp_long%>%
  left_join(gp_deprivation,by=c("GP"="PRACTICE_CODE"))

##charts----
gp_cols<-ggplot(data=gp_long, aes(x=reorder(PRACTICE_NAME.x,GP_weighted_IMD), y=value, fill=factor(decile))) +
  geom_col(position = position_fill(reverse = TRUE))+ 
  scale_fill_brewer(palette = "RdYlGn")+
  coord_flip()+
  labs(x = "GP Pratice: Most deprived at the top",
       y = "Percentage of patients")+
  scale_y_continuous(labels = scales::label_percent())+
  theme(axis.title.y = element_text(vjust= 0.8))+
  labs(fill = "Decile",
         title = paste0("Deprivation by GP Practice in ",subicb_name), 
       subtitle = paste0("Share of GP population living in each IMD 2025 deprivation decile: ",report_date),
       caption = "Source: NHS Digital / MHCLG")

gp_cols

ggsave(paste0(subicb_name,"_practice_deciles",report_date,".png"),gp_cols, units = "cm", width=25, height=25)


gp_cols2<-ggplot(data=gp_long, aes(x=reorder(PRACTICE_NAME.x,GP_weighted_IMD), y=value, fill=factor(decile))) +
  geom_col(position = position_fill(reverse = TRUE))+ 
  scale_fill_manual(values = imd_colours)+
  coord_flip()+
  labs(x = "GP Pratice: Most deprived at the top",
       y = "Percentage of patients")+
  scale_y_continuous(labels = scales::label_percent())+
  theme(axis.title.y = element_text(vjust= 0.8))+
  labs(fill = "Decile",
       title = paste0("Deprivation by GP Practice in ",subicb_name), 
       subtitle = paste0("Share of GP population living in each IMD 2025 deprivation decile: ",report_date),
       caption = "Source: NHS Digital / MHCLG")

gp_cols2

ggsave(paste0(subicb_name,"_practice_deciles_(MHCLG_colours)",report_date,".png"),gp_cols2, units = "cm", width=25, height=25)

##table----
GP_table<-gp_long%>%
  select(PRACTICE_NAME.x,GP,GP_weighted_IMD,GP_weighted_health_IMD)%>%
  distinct()%>%
  arrange(-GP_weighted_IMD)
write.csv(GP_table,paste0(subicb_name,"_GP_IMD_score_",report_date,".csv"),row.names = FALSE)


#practice in sub_ICB by LA of residence----
locality_la <-locality_gp_lsoa%>%
  select(PRACTICE="PRACTICE_NAME.x",LA="LA_NAME",COUNT="NUMBER_OF_PATIENTS")%>%
  pivot_wider(names_from = "LA",values_from = "COUNT",values_fn=sum)%>%
  pivot_longer(cols = !1, names_to = "LA")

locality_la[is.na(locality_la)] <- 0
by_patients <- locality_la %>% group_by(PRACTICE)%>%
  arrange(PRACTICE,desc(value))%>%
  filter(value!=0)
write.csv(by_patients,paste0(subicb_name,"_GP_to_LA_of_residence_",report_date,".csv"),row.names = FALSE)

