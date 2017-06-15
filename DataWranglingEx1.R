#Load
refine_xlsx = readxl::read_xlsx("refine.xlsx")
write.csv(refine_xlsx,file = "refine_original.csv",row.names = FALSE)
df_refine = read.csv("refine_original.csv")
library(tidyr)
library(dplyr)

#Clean column company
df_refine$company = sapply(df_refine$company,tolower)

indices_ph = grep("ps$",df_refine$company)
indices_ak = grep("^ak",df_refine$company)
indices_vh = grep("en$",df_refine$company)
indices_ver = grep("ver$",df_refine$company)

df_refine$company[indices_ph] = "philips"
df_refine$company[indices_ak] = "akzo"
df_refine$company[indices_vh] = "van houten"
df_refine$company[indices_ver] = "unilever"

#Separate the product code and product number into separate columns
df_refine =
separate(data = df_refine, col = Product.code...number, into = c("product_code ", "product_number"),sep = "\\-", remove = FALSE)

#Add product_category
product_letter <- function(x) {
  
  if(x=='p'){return("Smartphone")}
  if(x=='v'){return("TV")}
  if(x=='x'){return("Laptop")}
  if(x=='q'){return("Tablet")}
}

product_category = sapply(df_refine$product_code,product_letter)
df_refine = cbind(df_refine,product_category)

#swap product_category to second column
df_refine = df_refine[,c(1,9,2:8)]

#Unite columns adress,city,country into a new column full_adress sep by comma
df_refine=
unite(df_refine,col="full_adress",address:country, sep=",", remove=FALSE)

#Dummy variables for company and product
company_philips = as.integer(df_refine$company == "philips")
company_akzo = as.integer(df_refine$company == "akzo")
company_van_houten = as.integer(df_refine$company == "van houten")
company_unilever = as.integer(df_refine$company == "unilever")

product_smartphone = as.integer(df_refine$product_category == "Smartphone")
product_tv = as.integer(df_refine$product_category == "TV")
product_laptop = as.integer(df_refine$product_category == "Laptop")
product_tablet = as.integer(df_refine$product_category == "Tablet")

df_refine <- cbind(df_refine,company_philips,company_akzo,company_van_houten,company_unilever,
      product_smartphone,product_tv,product_laptop,product_tablet)

#Final result as CSV
write.csv(df_refine,file = "refine_clean.csv",row.names = FALSE)  
 