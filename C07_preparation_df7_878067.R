## Dataset 7

# df7: Purchases and refunds transactions of each customer
  
# Variables are:
  
#- `ID_SCONTRINO`: identify the transaction (all products have same ID);
#- `ID_CLI`: identify the client (*Foreign Key*);
#- `ID_NEG`: identify the reference store (*Foreign Key*);
#- `ID_ARTICOLO`: identify the purchased or refund item;
#- `COD_REPARTO`: identify the business unit corresponding to the item;
#- `DIREZIONE`: identify the purchase (1) or refund (-1);
#- `IMPORTO_LORDO`: identify the gross amount as the sum of net amount and the discount applied;
#- `SCONTO`: identify the discount applied (negative if refund);
#- `DATETIME`: datetime of the transaction.
#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

df7_overview <- df_7_tic_clean_final %>% 
  summarise(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### GENERAL OVERVIEW by COD_REPARTO ###

df7_overviewbycodreparto <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO) %>%
  summarise(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overviewbycodreparto

### Variable DIREZIONE ###

df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###


df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## Plot 

  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    labs(title = " Tickets for hours",
         x     = " Hours",
         y     = "Total amount")+
    theme_minimal()


## Plot percent

  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    labs(title = " Tickets for hours (%)",
         x     = " Hours",
         y     = "Total amount")+
    theme_minimal()



### Variable COD_REPARTO ###


df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## Plot 

  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    labs(title = " Tickets for department code",
         x     = "Department code",
         y     = "Total tickets")+
    theme_minimal()


## Plot percent

  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    labs(title = "Tickets for department code (%)",
         x     = "Department code",
         y     = "Total tickets")+
    theme_minimal()


### Variable TIC_DATE_TYP ###

df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## Plot 

  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    labs(title = "  Tickets for date",
         x     = "Type date",
         y     = "Total tickets")+
    theme_minimal()


## Plot percent

  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    labs(title = " Tickets for date (%)",
         x     = " Type date ",
         y     = "Total amount")+
    theme_minimal()


### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarise(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## Plot 

  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    labs(title = " Count gross amount",
         x     = "Gross amount",
         y     = "Amount")+
    theme_minimal()

## Plot percent

  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    labs(title = " Discounted Tickets",
         x     = "Discount",
         y     = "Amount")+
    theme_minimal()


#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_dist_importosconto_cod_rep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>% 
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO)) %>%  
  ungroup() %>%
  as.data.frame()
df7_dist_importosconto_cod_rep

# Plot
ggplot(data = df7_dist_importosconto_cod_rep,
       aes(fill = DIREZIONE,
           x = COD_REPARTO,
           y = IMPORTO_LORDO)) + 
  geom_bar(stat = "identity") +   
  labs(title = "Gross amount by departement code",
       x     = "Departement code",
       y     = "Gross amount")+
  theme_minimal() 

# SCONTO

ggplot(data = df7_dist_importosconto_cod_rep,
       aes(fill = DIREZIONE,
           x = COD_REPARTO,
           y = SCONTO)) + 
  
  geom_bar(stat = "identity") +  
  
  labs(title = "Discount by departement code",
       x     = "Departement code",
       y     = "Discount")+
  theme_minimal() 

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

df_7_tic_clean_final$ID_ARTICOLO <-as.factor( df_7_tic_clean_final$ID_ARTICOLO)

df7_dist_id_articolo <-  df_7_tic_clean_final                                      %>%
  filter(DIREZIONE == 1)                            %>% 
  group_by(ID_ARTICOLO)                             %>% 
  summarise(NUM_VENDITE = n_distinct(ID_SCONTRINO)) %>%
  ungroup()                                         %>%
  as.data.frame()                                   %>%
  arrange(desc(NUM_VENDITE))
df7_dist_id_articolo
# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

df7_dist_importosconto_id_cli <- df_7_tic_clean_final                      %>%
  filter(DIREZIONE == 1)          %>% 
  group_by(ID_CLI)                %>% 
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO)) %>% 
  ungroup()                       %>%
  as.data.frame()                 %>%
  arrange(desc(IMPORTO_LORDO))
df7_dist_importosconto_id_cli 

# compute the distribution of customers by number of purchases (as described in the slides)
df7_dist_tot_purch <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1)                             %>% 
  group_by(ID_CLI)                                   %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>% 
  arrange(desc(TOT_PURCHASE))                            
df7_dist_tot_purch 


ggplot(data = df7_dist_tot_purch 
       , aes(x = TOT_PURCHASE, y = ID_CLI)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = "  Total number of Purchases",
       x     = " Total Purchases",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)



# compute the days for next purchase curve (as described in the slides)
data_for_next_purchase <- df_7_tic_clean_final %>%
                            filter(DIREZIONE == 1) %>% 
                            select(ID_CLI,
                                    ID_ARTICOLO,
                                    TIC_DATE,
                                    DIREZIONE)      %>% 
                            arrange(ID_CLI)
data_for_next_purchase

df_np <- data_for_next_purchase %>%
  group_by(ID_CLI, TIC_DATE)%>%
  summarise(NUM_OBJ = n())%>%
  mutate(Diff = TIC_DATE - lag(TIC_DATE))

x <- as.data.frame(table(df_np$Diff))
x$Perc <- x$Freq/sum(x$Freq)

# Plot percent
ggplot(x, 
       aes(x = as.numeric(Var1),
           y = cumsum(Perc))) +
  labs(title = "Next Purchases Curve ",
       x = "Last Purchases Date in Days",
       y = "Cumulative Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +   
  scale_x_continuous(breaks = seq(0, 400, 25)) +     
  geom_vline(xintercept = 75, linetype = "dotted") +
  geom_line(size = 1)

#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)

#Information about this file:
#More items are sold during holidays and weekends, when customers have more time and money. 
#In addition, purchases are made mainly in the time slot after 17:00.