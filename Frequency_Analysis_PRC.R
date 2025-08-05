library(gdata)
library(gamlss)

library(dplyr) 
library(gam)

library(latex2exp) # for teX expression

library(QRM)
library(chron)
library(lubridate)
# test for GPD as in Villase?or-Alva and Gonz?lez-Estrada (2009)
#library(gPdtest)

# for fitting distributions
library(fitdistrplus)

# for vuong non nested models
library(nonnest2)

# for block.daig matrixes
#library(siam)

library(fExtremes)
library(boot)
library(gPdtest)


## Set up Directories ##

## Set Directories: 
setwd("")

data_dir = "./PRC/" 
output = "./PRC_output/"


##### FUNCTIONS #####

create_matrix_frequency_severity_classification_PRC = function(df.losses){
  
  
  RiskType = unique(df.losses$Type.of.breach)
  
  df = data.frame(Number = rep(0,length(RiskType)),
                  Median  = 0)
  
  rownames(df) =  RiskType
  for( i in 1:length(RiskType)){
    selected = subset(df.losses, df.losses$Type.of.breach == RiskType[i])
    df[RiskType[i],"Number"] = nrow(selected)
    df[RiskType[i],"Median"] = median(selected$Total.Records)
  }
  
  df$Likelihood = ""
  df$Severity = ""
  
  quantile_likelihood = quantile(df$Number,probs = c(0.33,0.66))
  quantile_severity = quantile(df$Median,probs = c(0.33,0.66))
  for (t in RiskType){
    if (df[t,"Number"]<= quantile_likelihood[1]){
      df[t,"Likelihood"] = "Rare"
    }
    else if ((df[t,"Number"]> quantile_likelihood[1]) & (df[t,"Number"]<=quantile_likelihood[2])) {
      df[t,"Likelihood"]= "Unlikely"
    } else if (df[t,"Number"]> quantile_likelihood[2]) {
      df[t,"Likelihood"]= "Likely"
      
    }
    
    if (df[t,"Median"]<= quantile_severity[1]){
      df[t,"Severity"] = "Low"
    }
    else if ((df[t,"Median"]> quantile_severity[1]) & (df[t,"Median"]<=quantile_severity[2])) {
      df[t,"Severity"]= "Medium"
    } else if (df[t,"Median"]> quantile_severity[2]) {
      df[t,"Severity"]= "High"
      
    }
    
  }
  
  df.losses$Case_Type_Matrix_FrequencySeverity = ""
  for (t in RiskType){
    mask = df.losses$Type.of.breach == t
    df.losses$Case_Type_Matrix_FrequencySeverity[mask] = paste0(df[t,'Likelihood'],df[t,'Severity'])
    
  }
  
  unique(df.losses$Case_Type_Matrix_FrequencySeverity)
  return(df.losses)
}



forecast_matrix_frequency_severity_classification = function(df.losses, df.out_of_sample){
  
  RiskType = unique(df.losses$Case_Type_Advisen)
  
  
  df = data.frame(Number = rep(0,length(RiskType)),
                  Median  = 0)
  
  rownames(df) =  RiskType
  for( i in 1:length(RiskType)){
    selected = subset(df.losses, df.losses$Case_Type_Advisen == RiskType[i])
    df[RiskType[i],"Number"] = nrow(selected)
    df[RiskType[i],"Median"] = median(selected$Losses)
  }
  
  df$Likelihood = ""
  df$Severity = ""
  
  quantile_likelihood = quantile(df$Number,probs = c(0.33,0.66))
  quantile_severity = quantile(df$Median,probs = c(0.33,0.66))
  for (t in RiskType){
    if (df[t,"Number"]<= quantile_likelihood[1]){
      df[t,"Likelihood"] = "Rare"
    }
    else if ((df[t,"Number"]> quantile_likelihood[1]) & (df[t,"Number"]<=quantile_likelihood[2])) {
      df[t,"Likelihood"]= "Unlikely"
    } else if (df[t,"Number"]> quantile_likelihood[2]) {
      df[t,"Likelihood"]= "Likely"
      
    }
    
    if (df[t,"Median"]<= quantile_severity[1]){
      df[t,"Severity"] = "Low"
    }
    else if ((df[t,"Median"]> quantile_severity[1]) & (df[t,"Median"]<=quantile_severity[2])) {
      df[t,"Severity"]= "Medium"
    } else if (df[t,"Median"]> quantile_severity[2]) {
      df[t,"Severity"]= "High"
      
    }
    
  }
  
  df.out_of_sample$Case_Type_Matrix_FrequencySeverity = ""
  for (t in RiskType){
    mask = df.out_of_sample$Case_Type_Advisen == t
    df.out_of_sample$Case_Type_Matrix_FrequencySeverity[mask] = paste0(df[t,'Likelihood'],df[t,'Severity'])
    
  }
  return(df.out_of_sample)
  
  
}

#df.losses = df.severity
create_matrix_type_importance_classification_PRC = function(df.losses, disruption, exfiltration_malware,low_level_scanning, other,  sectors ){
  RiskType =c(disruption, exfiltration_malware,low_level_scanning)
  
  df.sector = data.frame(Sector = unique(df.losses$Type.of.organization),
                         Median = 0)
  rownames(df.sector) = unique(df.losses$Type.of.organization)
  for( sector in unique(df.losses$Type.of.organization)){
    mask = df.losses$Type.of.organization == sector
    df.sector[sector,"Median"] = median(df.losses$Total.Records[mask])
    
  }
  
  quantile_sectors = quantile(df.sector$Median,prob = c(0.33,0.66))
  df.losses$Case_Type_Matrix_TypeImportance = ""
  
  for(t in RiskType){
    for(sector in unique(df.losses$Type.of.organization)){
      
      mask = df.losses$Type.of.breach == t & df.losses$Type.of.organization == sector
      if (sector == ""){
        sector = 16
      }
      if (df.sector[sector,"Median"] <= quantile_sectors[1]){
        if (t %in% disruption){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "DisruptionLow"
          
        } else if (t %in% exfiltration_malware){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationLow"
          
        }else if (t %in% low_level_scanning){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "LowLevelLow"
          
        } else if(t %in% other){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "OtherLow"
          
        }
        
        
      } else if ((df.sector[sector,"Median"] > quantile_sectors[1]) && (df.sector[sector,"Median"] <= quantile_sectors[2])){
        if (t %in% disruption){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "DisruptionMedium"
          
        } else if (t %in% exfiltration_malware){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationMedium"
          
        } else if (t %in% low_level_scanning){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "LowLevelMedium"
          
        } else if(t %in% other){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "OtherMedium"
          
        }
        
        
      } else if (df.sector[sector,"Median"] > quantile_sectors[2]){
        
        if (t %in% disruption){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "DisruptionHigh"
          
        } else if (t %in% exfiltration_malware){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationHigh"
          
        } else if (t %in% low_level_scanning){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "LowLevelHigh"
          
        } else if(t %in% other){
          df.losses$Case_Type_Matrix_TypeImportance[mask] = "OtherHigh"
          
        }
        
        
      }
      
    }
    
  }
  return(df.losses)
  
  
}



forecast_matrix_type_importance_classification = function(df.losses, df.out_of_sample, disruption, exfiltration_malware,low_level_scanning, other,sectors ){
  
  RiskType =c(disruption, exfiltration_malware,low_level_scanning, other)
  
  df.sector = data.frame(Sector = unique(df.losses$NAIC_Sector),
                         Median = 0)
  rownames(df.sector) = unique(df.losses$NAIC_Sector)
  for( sector in unique(df.losses$NAIC_Sector)){
    mask = df.losses$NAIC_Sector == sector
    df.sector[sector,"Median"] = median(df.losses$Losses[mask])
    
  }
  
  quantile_sectors = quantile(df.sector$Median,prob = c(0.33,0.66))
  df.out_of_sample$Case_Type_Matrix_TypeImportance = ""
  
  for(t in RiskType){
    for(sector in unique(df.out_of_sample$NAIC_Sector)){
      
      mask = df.out_of_sample$Case_Type_Advisen == t & df.out_of_sample$NAIC_Sector == sector
      if (sector == ""){
        sector = 16
      }
      if(is.na(df.sector[sector,"Median"]) ){
        
        if (t %in% disruption){
          df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "DisruptionHigh"
          
        } else if (t %in% exfiltration_malware){
          df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationHigh"
          
        }else if (t %in% low_level_scanning){
          df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "LowLevelLow"
          
        } else if(t %in% other){
          df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "OtherLow"
          
        }
        
      }else{ 
        if (df.sector[sector,"Median"] <= quantile_sectors[1]){
          if (t %in% disruption){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "DisruptionLow"
            
          } else if (t %in% exfiltration_malware){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationLow"
            
          }else if (t %in% low_level_scanning){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "LowLevelHigh"
            
          } else if(t %in% other){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "OtherHigh"
            
          }
          
          
        } else if ((df.sector[sector,"Median"] > quantile_sectors[1]) && (df.sector[sector,"Median"] <= quantile_sectors[2])){
          if (t %in% disruption){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "DisruptionMedium"
            
          } else if (t %in% exfiltration_malware){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationMedium"
            
          } else if (t %in% low_level_scanning){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "LowLevelMedium"
            
          } else if(t %in% other){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "OtherMedium"
            
          }
          
          
        } else if (df.sector[sector,"Median"] > quantile_sectors[2]){
          
          if (t %in% disruption){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "DisruptionHigh"
            
          } else if (t %in% exfiltration_malware){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "ExfiltrationHigh"
            
          } else if (t %in% low_level_scanning){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "LowLevelHigh"
            
          } else if(t %in% other){
            df.out_of_sample$Case_Type_Matrix_TypeImportance[mask] = "OtherHigh"
            
          }
          
          
        }
        
      }
    }
    
  }
  
  return(df.out_of_sample)
  
  
  
}
###### CREATE SEVERITY COVARIATES #####

## FIND Thresholds
# Load The PRC Data Breach Chronology file
df.severity = read.csv(paste0(data_dir,"PRC Data Breach Chronology - 1.13.20.csv"))

df.severity = df.severity[!is.na(df.severity$Total.Records),]


df.severity = df.severity %>% mutate(across(Type.of.breach, as.character))
df.severity = df.severity %>% mutate(across(Type.of.organization, as.character))

df.severity = df.severity[df.severity$Total.Records>0,]


RiskTypes = unique(df.severity$Type.of.breach)

df.stats = df.severity %>%
  group_by(Type.of.breach) %>%
  summarise(Mean = mean(Total.Records),
            N = length(Total.Records),
            Median = median(Total.Records),
            StDev = (var(Total.Records))^0.5,
            Skew = skewness(Total.Records),
            Kurt = kurtosis(Total.Records)) %>%
  as.data.frame()

write.csv(x = df.stats,
          file = paste0(output,"Stats_PRC.csv"),
          row.names = F)



threshold = data.frame(Year = c(2012:2019),
                       Threshold = NA,
                       Value = NA)

aggregate(df.severity$Total.Records, by = list(df.severity$Type.of.breach), FUN = mean)$x

cc = df.severity %>% group_by(Type.of.breach) %>% summarise(Mean = mean(Total.Records), .groups = 'drop') %>% as.data.frame()


years = sort(unique(df.severity$Year.of.Breach))

years = years[years>=2008]
for(j in 1:8){
  
  window = c(years[j]:years[j+4])
  covariates_severity = subset(df.severity, df.severity$Year.of.Breach %in% window)
  # Find threshold
  losses = covariates_severity$Total.Records
  
  i=1
  found = F
  thr = 0.01
  
  while (( i<=100) & (found == F)){
    
    exceedances = losses[ losses>sort(losses)[max(floor(thr*length(losses)),1)]] - sort(losses)[max(floor(thr*length(losses)),1)] 
    out = gpd.test(x = exceedances,
                   J = 1000)
    pvalue = out$boot.test$p.value
    print(out)
    if (pvalue > 0.01){
      found =T
      ustar = thr
    }
    
    i = i+1
    thr = thr+0.01
    print(thr)
    
  }
  
  threshold[threshold$Year == window[length(window)], "Threshold"] = thr
  threshold[threshold$Year == window[length(window)], "Value"] = sort(losses)[max(floor(thr*length(losses)),1)] 
  
}

write.csv(x = threshold,
          file = paste0(output,"Threshold_PRC.csv"),
          row.names = F)

## CREATE COVARIATES FOR TAIL

# Find threhsold over the entire period:
losses = df.severity$Total.Records

i=1
found = F
thr = 0.01


while (( i<=100) & (found == F)){
  
  exceedances = losses[ losses>sort(losses)[max(floor(thr*length(losses)),1)]] - sort(losses)[max(floor(thr*length(losses)),1)] 
  out = gpd.test(x = exceedances,
                 J = 1000)
  pvalue = out$boot.test$p.value
  print(out)
  if (pvalue > 0.01){
    found =T
    ustar = thr
  }
  
  i = i+1
  thr = thr+0.01
  print(thr)
  
}

u = thr
df.exceedances = subset(df.severity, df.severity$Total.Records > sort(df.severity$Total.Records)[floor(thr * length(df.severity$Total.Records))])

df.exceedances$Type.of.breach = factor(df.exceedances$Type.of.breach)
df.exceedances$Type.of.organization = factor(df.exceedances$Type.of.organization)


drops = c("X", "X.1","X.2", "Date.Made.Public", "City",
          "State", "Description.of.incident", "Information.Source" , "Source.URL" ,"Latitude","Longitude")

df.exceedances = df.exceedances[, !names(df.exceedances) %in% drops]


model0 = gamlss(Total.Records ~ Year.of.Breach +Type.of.breach + Type.of.organization,
                sigma.formula = ~Year.of.Breach +Type.of.breach + Type.of.organization,
                data = df.exceedances,
                family=GP(mu.link = "log", sigma.link="log"),
                control = gamlss.control(c.crit = 0.001,
                                         n.cyc = 100,
                                         mu.step = 1,
                                         sigma.step = 1,
                                         nu.step = 1, 
                                         tau.step = 1,
                                         gd.tol = Inf,
                                         iter = 0, 
                                         trace = TRUE,
                                         autostep = TRUE, 
                                         save = TRUE))

while (model0$converged==F){
  model0=refit(model0)
}


RiskTypes = unique(df.exceedances$Type.of.breach)


test_matrix.ks = matrix(0,
                        nrow = length(unique(RiskTypes)),
                        ncol = length(unique(RiskTypes)))

df.exceedances$Residual= model0$residuals
for( i in 1:length(RiskTypes)){
  for (j in 1:length(RiskTypes)){
    temp1 = subset(df.exceedances, df.exceedances$Type.of.breach == RiskTypes[i])
    res1 = temp1$Residual
    temp2 = subset(df.exceedances, df.exceedances$Type.of.breach == RiskTypes[j])
    res2 = temp2$Residual
    
    # KS Test 
    pval = ks.test(as.numeric(res1), as.numeric(res2))$p.value
    if (pval>0.1){
      test_matrix.ks[i,j]=1
    }
    
  }
  
}

df.merge = data.frame(test_matrix.ks)
rownames(df.merge) = RiskTypes
colnames(df.merge) = RiskTypes
write.csv(x = df.merge,
          file = paste0(output,"Merging_Table_PRC.csv"))


## CREATE BODY CLASSIFICATION

#df.body = subset(df.severity, df.severity$Total.Records <= sort(df.severity$Total.Records)[floor(thr * length(df.severity$Total.Records))])


df.body = df.severity

drops = c("X", "X.1","X.2", "Date.Made.Public", "City",
          "State", "Description.of.incident", "Information.Source" , "Source.URL" ,"Latitude","Longitude")

df.body = df.body[, !names(df.body) %in% drops]



model0_logno = gamlss(Total.Records ~ Year.of.Breach +Type.of.breach + Type.of.organization,
                      sigma.formula = ~Year.of.Breach +Type.of.breach + Type.of.organization,
                      data = df.body,
                      family=LOGNO(mu.link = "identity", sigma.link = "log"),
                      control = gamlss.control(c.crit = 0.001,
                                               n.cyc = 100,
                                               mu.step = 1,
                                               sigma.step = 1,
                                               nu.step = 1, 
                                               tau.step = 1,
                                               gd.tol = Inf,
                                               iter = 0, 
                                               trace = TRUE,
                                               autostep = TRUE, 
                                               save = TRUE))

while (model0_logno$converged==F){
  model0_logno=refit(model0)
}

RiskTypes = c("CARD",
              "DISC",
              "HACK",
              "INSD",
              "PHYS",
              "PORT",
              "STAT",
              "UNKN")

test_matrix.ks_logno = matrix(0,
                              nrow = length(unique(RiskTypes)),
                              ncol = length(unique(RiskTypes)))

df.body$Residual = model0_logno$residuals

for( i in 1:length(RiskTypes)){
  for (j in 1:length(RiskTypes)){
    temp1 = subset(df.body, df.body$Type.of.breach == RiskTypes[i])
    res1 = temp1$Residual
    temp2 = subset(df.body, df.body$Type.of.breach == RiskTypes[j])
    res2 = temp2$Residual
    
    # KS Test 
    pval = ks.test(as.numeric(res1), as.numeric(res2))$p.value
    if (pval>0.1){
      test_matrix.ks_logno[i,j]=1
    }
    
  }
  
}

df.merge = data.frame(test_matrix.ks_logno)
rownames(df.merge) = RiskTypes
colnames(df.merge) = RiskTypes
write.csv(x = df.merge,
          file = paste0(output,"Merging_Table_PRC_Body.csv"))



### MAP TYPE INTO CLASSIFICATIONS

# TAIL CLASSIFICATION
Type_1_Tail = c("PORT","CARD", "INSD", "STAT","UNKN")
Type_2_Tail = c("DISC","PHYS")
Type_3_Tail = c("HACK")


# BODY CLASSIFICATION

Type_1_Body = c("CARD", "DISC")

Type_2_Body = c("HACK", "INSD")

Type_3_Body = c("PHYS",
                "UNKN")

Type_4_Body = c("PORT", "STAT")



df.severity$Case_Type_Residual_Tail = ""
df.severity$Case_Type_Residual_Body = ""

RiskTypes = unique(df.severity$Type.of.breach)

for (type in RiskTypes){
  
  mask = df.severity$Type.of.breach == type
  
  # Empirical Tail
  if (type %in% Type_1_Tail ){
    df.severity$Case_Type_Residual_Tail[mask] = "Type1Tail"
  } else if (type %in% Type_2_Tail){
    df.severity$Case_Type_Residual_Tail[mask] = "Type2Tail"
  } else if (type %in% Type_3_Tail) {
    df.severity$Case_Type_Residual_Tail[mask] = "Type3Tail"
    
  }
  
  # Empirical Body
  
  if (type %in% Type_1_Body ){
    df.severity$Case_Type_Residual_Body[mask] = "Type1Body"
  } else if (type %in% Type_2_Body){
    df.severity$Case_Type_Residual_Body[mask] = "Type2Body"
  } else if (type %in% Type_3_Body) {
    df.severity$Case_Type_Residual_Body[mask] = "Type3Body"
    
  }else if (type %in% Type_4_Body){
    df.severity$Case_Type_Residual_Body[mask] = "Type4Body"
  }
  
  
}



### Map Frequency Severity Classification



df.box = df.severity

df.severity = create_matrix_frequency_severity_classification_PRC(df.severity)


#### Map Type Importance Classification

df.severity$Case_Type_Matrix_TypeImportance = ""


low_level_scanning = c("DISC",
                       "PHYS",
                       "UNKN")

exfiltration_malware = c("CARD",
                         "PORT",
                         "HACK")

disruption = c("INSD",
               "STAT")

sectors = unique(df.severity$Type.of.organization)


df.box2 = df.severity

df.severity = create_matrix_type_importance_classification_PRC(df.losses = df.severity, 
                                                 disruption = disruption,
                                                 exfiltration_malware = exfiltration_malware,
                                                 low_level_scanning = low_level_scanning,
                                                 other = "other",
                                                 sectors = sector)




unique(df.severity$Case_Type_Residual_Tail)
unique(df.severity$Case_Type_Residual_Body)
unique(df.severity$Case_Type_Matrix_FrequencySeverity)
unique(df.severity$Case_Type_Matrix_TypeImportance)


## ALSO DROP RISK TYPE NA
drops = c("X", "X.1","X.2")

df.severity = df.severity[, !names(df.severity) %in% drops]

write.csv(x = df.severity,
          file = paste0(data_dir,"Covariates_Severity_PRC.csv"),
          row.names = F)



#### CREATE FREQUENCY COVARIATES ####
df.severity = read.csv(paste0(data_dir,"Covariates_Severity_PRC.csv"))

companies = unique(df.severity$Company)

years= 2008:max(df.severity$Year.of.Breach)

# PRC RISK TYPES
RiskTypes = unique(df.severity$Type.of.breach)
RiskTypes = RiskTypes[RiskTypes != "#N/A"]

df.frequency = data.frame(N = rep(0, length(companies) * length(RiskTypes) * length(years)),
                          Year = rep( sort(rep(years, length(RiskTypes))), length(companies)),
                          Company_ID = sort(rep( rep(companies,length(RiskTypes)), length(years))),
                          RiskType = unlist(replicate(length(companies) *length(years) ,sort(unlist(replicate(1, RiskTypes, simplify = F))), simplify = F)),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_PRC.csv"),
          row.names = F)



# TAIL RISK TYPES
RiskTypes = unique(df.severity$Case_Type_Residual_Tail)
RiskTypes = RiskTypes[RiskTypes != ""]

df.frequency = data.frame(N = rep(0, length(companies) * length(RiskTypes) * length(years)),
                          Year = rep( sort(rep(years, length(RiskTypes))), length(companies)),
                          Company_ID = sort(rep( rep(companies,length(RiskTypes)), length(years))),
                          RiskType = unlist(replicate(length(companies) *length(years) ,sort(unlist(replicate(1, RiskTypes, simplify = F))), simplify = F)),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_Tail.csv"),
          row.names = F)




# BODY RISK TYPES

RiskTypes = unique(df.severity$Case_Type_Residual_Body)
RiskTypes = RiskTypes[RiskTypes != ""]

df.frequency = data.frame(N = rep(0, length(companies) * length(RiskTypes) * length(years)),
                          Year = rep( sort(rep(years, length(RiskTypes))), length(companies)),
                          Company_ID = sort(rep( rep(companies,length(RiskTypes)), length(years))),
                          RiskType = unlist(replicate(length(companies) *length(years) ,sort(unlist(replicate(1, RiskTypes, simplify = F))), simplify = F)),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_Body.csv"),
          row.names = F)



# FRE/SEV RISK TYPES

RiskTypes = unique(df.severity$Case_Type_Matrix_FrequencySeverity)
RiskTypes = c("LikelyLow",
              "LikelyHigh",
              "LikelyMedium",
              "UnlikelyLow",
              "UnlikelyMedium",
              "UnlikelyHigh",
              "RareHigh",
              "RareLow",
              "RareMedium")
RiskTypes = RiskTypes[RiskTypes != ""]

df.frequency = data.frame(N = rep(0, length(companies) * length(RiskTypes) * length(years)),
                          Year = rep( sort(rep(years, length(RiskTypes))), length(companies)),
                          Company_ID = sort(rep( rep(companies,length(RiskTypes)), length(years))),
                          RiskType = unlist(replicate(length(companies) *length(years) ,sort(unlist(replicate(1, RiskTypes, simplify = F))), simplify = F)),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_FRESEV.csv"),
          row.names = F)


## TYPE IMPORTANCE

RiskTypes = unique(df.severity$Case_Type_Matrix_TypeImportance)
RiskTypes = RiskTypes[RiskTypes != ""]

RiskTypes=c("ExfiltrationLow",
            "ExfiltrationMedium",
            "ExfiltrationHigh",
            "LowLevelLow",
            "LowLevelMedium",
            "LowLevelHigh",
            "DisruptionLow",
            "DisruptionMedium",
            "DisruptionHigh"
            )

df.frequency = data.frame(N = rep(0, length(companies) * length(RiskTypes) * length(years)),
                          Year = rep( sort(rep(years, length(RiskTypes))), length(companies)),
                          Company_ID = sort(rep( rep(companies,length(RiskTypes)), length(years))),
                          RiskType = unlist(replicate(length(companies) *length(years) ,sort(unlist(replicate(1, RiskTypes, simplify = F))), simplify = F)),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_TYPEIMP.csv"),
          row.names = F)

## Random Classification

RiskType = c("Random1",
             "Random2",
             "Random3",
             "Random4")
set.seed(1)
df.severity$Case_Type_Random = sample(RiskType,
                                      size = nrow(df.severity), replace = T)



RiskTypes = unique(df.severity$Case_Type_Random)
RiskTypes = RiskTypes[RiskTypes != ""]

df.frequency = data.frame(N = rep(0, length(companies) * length(RiskTypes) * length(years)),
                          Year = rep( sort(rep(years, length(RiskTypes))), length(companies)),
                          Company_ID = sort(rep( rep(companies,length(RiskTypes)), length(years))),
                          RiskType = unlist(replicate(length(companies) *length(years) ,sort(unlist(replicate(1, RiskTypes, simplify = F))), simplify = F)),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_Random.csv"),
          row.names = F)

## None


df.frequency = data.frame(N = rep(0, length(companies) * length(years)),
                          Year = rep( years, length(companies)),
                          Company_ID = sort(rep( companies, length(years))),
                          Sector = "")


for( com in companies){
  
  maskFreq = df.frequency$Company_ID == com
  maskSev = df.severity$Company == com
  
  df.frequency$Sector[maskFreq] = rep(df.severity$Type.of.organization[maskSev][1],
                                      length(df.frequency$Sector[maskFreq]))
  
}


write.csv(x = df.frequency,
          file = paste0(data_dir,"Covariates_Frequency_PRC_None.csv"),
          row.names = F)

