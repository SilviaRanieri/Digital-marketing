## Dataset 5

# df5: Categorization of the marketing email communications
  
# Variables are:
  
#- `ID_CAMP`: identify the email campaign (**Key**);
#- `TYP_CAMP`: identify the type email campaign.
#- `CHANNEL_CAMP`: channel of campaign submission.

#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)
df_5_camp_cat_clean 
#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

#Information about this file:
#Type of camapign are distribution: Product (43,8%), Personalized (19,9%), National (17,6%), Newsletter (12,8%) and Local (0,69%).
