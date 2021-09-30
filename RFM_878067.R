# RFM Model

#RFM is a method used to analyze customer value. RFM is based on three dimensions:
  
#* *Recency*: How recently a customer has made a purchase;
#* *Frequency*: How often a customer makes a purchase;
#* *Monetary* Value: How much money a customer spends on purchases.


#Customer value comes:
#Have purchase more recently
#-a customer who has interacted more recently is likely to be more responsive to the company communications
#• Purchase more often
#- a customer who interacts more frequently is likely more loyal to the company
#• Spend more
#- a customer who spends more is obviously more valuable

#As active customers i consider if the last purchase is after  01/01/2019 :

rfm_study_period <- df_7_tic_clean_final %>%
                      filter(TIC_DATE > as.Date("01/01/2019",
                                                format = "%d/%m/%Y")) #-- Active Clients

rfm_study_period  

## Recency

rfm_recency <- rfm_study_period  %>%
                  filter(DIREZIONE == 1) %>% 
                  group_by(ID_CLI)       %>% 
                  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))
rfm_recency 

#Calculated the difference in days:
rfm_recency$RECENCY <- difftime(as.Date("30/04/2019",
                                        format = "%d/%m/%Y"),       
                                     rfm_recency$LAST_PURCHASE_DATE,
                                units = "days")

rfm_recency

#It is divided into groups using the percentiles of each of the RFM measures.:
# `Low`: below the 25th percentile of the distribution;
# `Medium`: from 25th to 75th percentile;
# `High`: above 75th percentile;

rfm_recency <- within(rfm_recency,
                      REC_CLASS <- cut(as.numeric(rfm_recency$RECENCY),
                                       breaks = quantile(rfm_recency$RECENCY,
                                                         probs = c(0, .25, .75, 1)), 
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))        
rfm_recency

recency_label  <- as.data.frame(table(rfm_recency$REC_CLASS))
recency_label$perc<-recency_label$Freq/sum(recency_label$Freq)

#Plot
ggplot(data = recency_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                  
  labs(title = "Recency distribution",
       x     = "Recency Classes",
       y     = "Total Purchase") +                
  theme_minimal() +                              
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)#molti più medium, da considerare la classe percentile

## Frequency

# Calculate for each customer how many purchases he has made:
rfm_frequency <- rfm_study_period                                      %>%
  filter(DIREZIONE == 1)                             %>% 
  group_by(ID_CLI)                                   %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))
rfm_frequency 

# Divided  in 3 different group:
  
#* `Low`: below 2 total purchases;
#* `Medium`: from 2 to 5 purchases;
#* `High`: above 5 (to 101) purchases;

rfm_frequency <- within(rfm_frequency,
                        FREQ_CLASS <- cut(rfm_frequency$TOT_PURCHASE,
                                          breaks = c(0, 2, 5, 101),            
                                          include.lowest = T,
                                          right = F,
                                          labels = c("low", "medium", "high")))

table(rfm_frequency$FREQ_CLASS)
# Construct the number of id_cli for each class frequency:
frequency_label <- as.data.frame(table(rfm_frequency$FREQ_CLASS))

# Plot:
ggplot(data = frequency_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                  
  labs(title = "Frequency Distribution",
       x     = "Frequency Classes",
       y     = "Total Purchases") +                
  theme_minimal() +                              
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)

## Monetary

rfm_monetary <- rfm_study_period                            %>%
                  filter(DIREZIONE == 1)                    %>% 
                  group_by(ID_CLI)                          %>% 
                  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
                   ungroup()                                 %>%
                   as.data.frame()                           %>%
                   arrange(desc(IMPORTO_LORDO))

rfm_monetary 
# Divided  in 3 different group:
#  `Low`: below 2 total purchases;
# `Medium`: from 2 to 5 purchases;
# `High`: above 5 (to 101) purchases;


rfm_monetary <- within(rfm_monetary,
                       MON_CLASS <- cut(rfm_monetary$SPESA,
                                        breaks = quantile(rfm_monetary$SPESA,
                                                          probs = c(0, .25, .75, 1)),
                                        include.lowest = T,

                                        labels = c("low", "medium", "high"))) 
rfm_monetary 
table(rfm_monetary$MON_CLASS)

monetary_label <- as.data.frame(table(rfm_monetary$MON_CLASS))
# Plot
ggplot(data = monetary_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                   
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "Monetary Distribution",
       x     = "Monetary Classes",
       y     = "Total Amount") +                 
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)



# Merge all three RFM dataset in one:
  
#r RFM Merge
rfm <- merge(rfm_frequency, #-- Frequency
             rfm_monetary,  #-- Monetary
             by = "ID_CLI") #-- Key for Merge
rfm <- merge(rfm,           #-- Frequency + Monetary
             rfm_recency,   #-- Recency
             by = "ID_CLI") #-- Key for Merge


#For establish customer loyalty status, new classifications are created by combining recency and frequency based on the percentile:

rfm$RF <- NA
for(i in c(1:nrow(rfm))){
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "One-Timer"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "One-Timer"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "Leaving"
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Engaged"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Engaged"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Leaving"
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Top"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Top"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Leaving Top"
}
table(rfm$RF)


#Plot:

rf_df <- as.data.frame(rbind(c("Top",         "High",   "Low",    16248),
                              c("Top",         "High",   "Medium", 16248),
                              c("Leaving Top", "High",   "High",   592),
                              c("Engaged",     "Medium", "Low",    36316),
                              c("Engaged",     "Medium", "Medium", 36316),
                              c("Leaving",     "Medium", "High",   27187),
                              c("One Timer",   "Low",    "Low",    32763),
                              c("One Timer",   "Low",    "Medium", 32763),
                              c("Leaving",     "Low",    "High",   27187)))
colnames(rf_df) <-  c("Level", "Frequency", "Recency", "Value")
rf_df$Frequency <- factor(rf_df$Frequency,
                          levels = c("High", "Medium", "Low"))
                          
rf_df$Recency <- factor(rf_df$Recency,
                          levels = c("High", "Medium", "Low"))
rf_df$Value <- as.numeric(rf_df$Value)
ggplot(rf_df, aes(x = Frequency, y = Recency, fill = Value)) + 
  geom_tile() +
  geom_text(aes(label = Level)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()
rf <- as.data.frame(table(rfm$RF))
rf
# Plot
ggplot(data = rf,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "RF Distribution",
       x     = "RF Classes",
       y     = "Total Clients") +                 
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Engaged", "Leaving", "Leaving Top",
                              "One Timer", "Top")) + 
  guides(fill = FALSE)


# Joins with monetary to create RFM class: 
rfm$RFM <- NA
for(i in c(1:nrow(rfm))){
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Cheap"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Silver"
  
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Gold"
  
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Gold"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Diamond"
}

table(rfm$RFM)
# Plot:
rfm_df <- as.data.frame(rbind(c("Top", "High", "Diamond", 10984),
                              c("Top", "Medium", "Gold", 5585),
                              c("Top", "Low", "Silver", 10306),
                              c("Leaving Top", "High", "Gold", 5585),
                              c("Leaving Top", "Medium", "Silver", 10306),
                              c("Leaving Top", "Low", "Bronze", 25932),
                              c("Engaged", "High", "Silver", 10306),
                              c("Engaged", "Medium", "Bronze", 25932),
                              c("Engaged", "Low", "Copper", 20938),
                              c("Leaving", "High", "Bronze", 25932),
                              c("Leaving", "Medium", "Copper", 20938),
                              c("Leaving", "Low", "Tin", 24967),
                              c("One Timer", "High", "Copper", 20938),
                              c("One Timer", "Medium", "Tin", 24967),
                              c("One Timer", "Low", "Cheap", 14394)))
colnames(rfm_df) <- c("RF", "Monetary", "Level", "Value")
rfm_df$RF <- factor(rfm_df$RF,
                    levels = c("Top", "Leaving Top",
                               "Engaged", "Leaving", "One Timer"))
rfm_df$Monetary <- factor(rfm_df$Monetary,
                          levels = c("Low", "Medium", "High"))
rfm_df$Value <- as.numeric(rfm_df$Value)
# Plot
ggplot(rfm_df, aes(x = RF, y = Monetary, fill = Value)) + 
  geom_tile() +
  geom_text(aes(label = Level)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()

rfm_plot <- as.data.frame(table(rfm$RFM))

# Plot
ggplot(data = rfm_plot,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity") +                   
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "RFM Distribution",
       x     = "RFM Classes",
       y     = "Total Clients") +                 
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Bronze", "Cheap", "Copper", "Diamond",
                              "Gold", "Silver", "Tin")) + 
  guides(fill = FALSE)
