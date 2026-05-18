# Dink daaraan om LCA/Mixture modelling te gebruik 

install.packages("mice")
install.packages("mitml")
install.packages("‘BaylorEdPsych’")
library(readxl)
library(dplyr)
library(mice)
library(stringr)


business_risk_unlabeled_capstone_data_final <- read_excel("business_risk_unlabeled_capstone_data_final.xlsx")
data_final <- business_risk_unlabeled_capstone_data_final[-1,]

temp_missing<-data_final[!complete.cases(data_final), ]

data_final_numeric <- data_final[sapply(data_final, is.numeric)]
data_final_numeric <- subset(data_final_numeric, select = -c(client_reference_hash))
# data_final_numeric <- data_final_numeric[ , -which(names(data_final_numeric) %in% c("client_id","client_reference_hash"))]

imp_numeric <- mice(data_final_numeric, m=5, method="pmm", seed=42)
imp_numeric$loggedEvents
imp_numeric_data<-imp_numeric$data
densityplot(imp_numeric)

all_MI_data <- complete(imp_numeric, "long")


######Testing if categorical has pattern with missingness #####

# creates indicator variables for the categorical variables. 
# so then the 1 and 0 becomes the Y variables/output that we use to see if the categorical variables
# predict the missingness. see if MAR or maybe MCAR with categorical variables


data_final_all_imp <-subset(data_final, select = -c(client_id, client_reference_hash))

data_final_all_imp$region <- str_to_title(data_final_all_imp$region)
data_final_all_imp$region[data_final_all_imp$region=="gauteng"] <- "Gauteng"
data_final_all_imp$region[data_final_all_imp$region=="Free-State"] <- "Free State"
data_final_all_imp$region[data_final_all_imp$region=="Mpum."] <- "Mpumalanga"
data_final_all_imp$region[data_final_all_imp$region=="Kzn"] <- "Kwazulu-Natal"

data_final_all_imp$sector[is.na(data_final_all_imp$sector)] <- "Other/Missing"

data_final_all_imp$employee_band <- str_to_title(data_final_all_imp$employee_band)
data_final_all_imp$region[data_final_all_imp$region=="Med"] <- "Medium"


data_final_all_imp$acquisition_channel[is.na(data_final_all_imp$acquisition_channel)] <- "Other/Missing"
n_distinct(data_final_all_imp$region)
n_distinct(data_final_all_imp$sector)
n_distinct(data_final_all_imp$employee_band)
n_distinct(data_final_all_imp$acquisition_channel)


data_test <- data_final_all_imp %>%
  mutate(
    miss_1 = as.numeric(is.na(quick_ratio)),
    miss_2 = as.numeric(is.na(ebit_margin)),
    miss_3 = as.numeric(is.na(employee_growth)),
    miss_4 = as.numeric(is.na(web_traffic_growth))
  )


model_miss_1 <- glm(miss_1 ~ sector + region + primary_product + acquisition_channel+ employee_band + marketing_segment_code, 
                data = data_test, 
                family = binomial)

summary(model_miss_1)
# Lyk my market_segment --> codeD03 is related to if quick_ratio is missing

model_miss_2 <- glm(miss_2 ~ sector + region + primary_product + acquisition_channel+ employee_band + marketing_segment_code, 
                    data = data_test, 
                    family = binomial)

summary(model_miss_2)
# Lyk my market_segment --> codeC91 is related to if ebit_margin is missing


model_miss_3 <- glm(miss_3 ~ sector + region + primary_product + acquisition_channel+ employee_band + marketing_segment_code, 
                    data = data_test, 
                    family = binomial)

summary(model_miss_3)
# lyk ook soos MCAR 

model_miss_4 <- glm(miss_4 ~ sector + region + primary_product + acquisition_channel+ employee_band + marketing_segment_code, 
                    data = data_test, 
                    family = binomial)

summary(model_miss_4)
# lyk my region --> western cape is related to if web_traffic_growth is missing 




# factorise vir die MI
cat_cols <- c("sector","region","primary_product","primary_product",
              "acquisition_channel","employee_band","marketing_segment_code")

# data_final_all_imp[cat_cols] <- lapply(data_final_all_imp[,cat_cols], as.factor)

data_final_all_imp$sector <- as.factor(data_final_all_imp$sector)
data_final_all_imp$region <- as.factor(data_final_all_imp$region)
data_final_all_imp$primary_product <- as.factor(data_final_all_imp$primary_product)
data_final_all_imp$acquisition_channel <- as.factor(data_final_all_imp$acquisition_channel)
data_final_all_imp$employee_band <- as.factor(data_final_all_imp$employee_band)
data_final_all_imp$marketing_segment_code <- as.factor(data_final_all_imp$marketing_segment_code)
BaylorEdPsych::LittleMCAR(data_final_all_imp)

imp_all <- mice(data_final_all_imp, m=25, method="pmm", seed=42)
imp_all$loggedEvents
imp_all_data <- imp_all$data
densityplot(imp_all)
all_MI_data_final <- complete(imp_all, "long")

summary(imp_all$imp$ebit_margin)


# =========================
# EXPORT CLEAN IMPUTED DATA
# =========================

# Option 1: Extract one completed dataset (usually first imputation)
clean_data_final <- complete(imp_all, 1)

# Preview
summary(clean_data_final)
str(clean_data_final)

# Save to CSV
write.csv(clean_data_final,
          "business_risk_clean_imputed_dataset.csv",
          row.names = FALSE)

