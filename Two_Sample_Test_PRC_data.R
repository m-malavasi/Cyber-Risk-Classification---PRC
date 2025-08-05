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
setwd("/Users/z3540935/Cyber Project/Cyber Data")

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



forecast_matrix_frequency_severity_classification_PRC = function(df.losses, df.out_of_sample){
  
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
  
  df.out_of_sample$Case_Type_Matrix_FrequencySeverity = ""
  for (t in RiskType){
    mask = df.out_of_sample$Type.of.breach == t
    df.out_of_sample$Case_Type_Matrix_FrequencySeverity[mask] = paste0(df[t,'Likelihood'],df[t,'Severity'])
    
  }
  return(df.out_of_sample)
  
  
}

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


#df.losses = severity_insample
#df.out_of_sample = severity_outsample
forecast_matrix_type_importance_classification_PRC = function(df.losses, df.out_of_sample, disruption, exfiltration_malware,low_level_scanning, other,sectors ){
  
  RiskType =c(disruption, exfiltration_malware,low_level_scanning)
  
  df.sector = data.frame(Sector = unique(df.losses$Type.of.organization),
                         Median = 0)
  rownames(df.sector) = unique(df.losses$Type.of.organization)
  for( sector in unique(df.losses$Type.of.organization)){
    mask = df.losses$Type.of.organization == sector
    df.sector[sector,"Median"] = median(df.losses$Total.Records[mask])
    
  }
  
  quantile_sectors = quantile(df.sector$Median,prob = c(0.33,0.66))
  df.out_of_sample$Case_Type_Matrix_TypeImportance = ""
  
  for(t in RiskType){
    for(sector in unique(df.out_of_sample$Type.of.organization)){
      
      mask = df.out_of_sample$Type.of.breach == t & df.out_of_sample$Type.of.organization == sector
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

#######



#### START ########
setwd("/Users/z3540935/Cyber Project/Cyber Data")
data_dir = "./PRC/" 
output = "./PRC_output/"

df.severity = read.csv(paste0(data_dir,"Covariates_Severity_PRC.csv"))
df.severity = df.severity[df.severity$Type.of.breach!= "#N/A",]
classifications = c("Body",
                    "Tail",
                    'PRC',
                    "FRESEV",
                    "TYPEIMP")

years = sort(unique(df.severity$Year.of.Breach))
years = years[years>=2008]
years_out_of_sample = c(2013:2019)


RiskType = c("Random1",
             "Random2",
             "Random3",
             "Random4")
set.seed(1)
df.severity$Case_Type_Random = sample(RiskType,
                                      size = nrow(df.severity), replace = T)



# in sampel
Table_Insample_overall = data.frame(matrix(0, nrow = length(classifications), ncol = 2))
rownames(Table_Insample_overall) = classifications
colnames(Table_Insample_overall) = c("None", "Random")



# By years

Table_InSample_perYear_none = data.frame( matrix(0,
                                                 ncol = length(years_out_of_sample),
                                                 nrow = length(classifications)))

rownames(Table_InSample_perYear_none) = classifications
colnames(Table_InSample_perYear_none) =years_out_of_sample

Table_InSample_perYear_random = data.frame( matrix(0,
                                                   ncol = length(years_out_of_sample),
                                                   nrow = length(classifications)))

rownames(Table_InSample_perYear_random) = classifications
colnames(Table_InSample_perYear_random) =years_out_of_sample


# out sample
Table_Outsample_overall = data.frame(matrix(0, nrow = length(classifications), ncol = 2))
rownames(Table_Outsample_overall) = classifications
colnames(Table_Outsample_overall) = c("None", "Random")


# By years
Table_OutSample_perYear_none = data.frame( matrix(0,
                                                  ncol = length(years_out_of_sample),
                                                  nrow = length(classifications)))

rownames(Table_OutSample_perYear_none) = classifications
colnames(Table_OutSample_perYear_none) =years_out_of_sample

Table_OutSample_perYear_random = data.frame( matrix(0,
                                                    ncol = length(years_out_of_sample),
                                                    nrow = length(classifications)))

rownames(Table_OutSample_perYear_random) = classifications
colnames(Table_OutSample_perYear_random) =years_out_of_sample


low_level_scanning = c("DISC",
                       "PHYS",
                       "UNKN")

exfiltration_malware = c("CARD",
                         "PORT",
                         "HACK")

disruption = c("INSD",
               "STAT")

#### PRC ##########

classification = "PRC"
df.frequency = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_",classification,".csv"))
df.none =read.csv(paste0(data_dir,"Covariates_Frequency_PRC_None.csv")) 
df.random = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_Random.csv")) 

freq_insample_overall=vector()
none_insample_overall = vector()
random_insample_overall = vector()

freq_outsample_overall=vector()
none_outsample_overall = vector()
random_outsample_overall = vector()

for(w in 1:length(years_out_of_sample)){
  print(paste0("Year: ",years_out_of_sample[w]))
  severity_insample = subset(df.severity, df.severity$Year.of.Breach<=years_out_of_sample[w]-1  & df.severity$Year.of.Breach>=years_out_of_sample[w]-1 -4)
  severity_outsample = subset(df.severity, df.severity$Year.of.Breach== years_out_of_sample[w])
  
  frequency_insample = subset(df.frequency, df.frequency$Year<=years_out_of_sample[w]-1  & df.frequency$Year>=years_out_of_sample[w]-1 -4)
  frequency_outsample = subset(df.frequency, df.frequency$Year==years_out_of_sample[w])
  
  none_insample = subset(df.none, df.none$Year<=years_out_of_sample[w]-1  & df.none$Year>=years_out_of_sample[w]-1 -4)
  none_outsample = subset(df.none, df.none$Year==years_out_of_sample[w])
  
  random_insample = subset(df.random, df.random$Year<=years_out_of_sample[w]-1  & df.random$Year>=years_out_of_sample[w]-1 -4)
  random_outsample = subset(df.random, df.random$Year==years_out_of_sample[w])
  print(paste0("before Loop1: ", max(frequency_insample$N)))
  print(paste0("before Loop2: ", max(none_insample$N)))
  
  # INSAMPLE
  loop.vector = 1:nrow(severity_insample)
  for( i in loop.vector){
    id = severity_insample$Company[i]
    type = severity_insample$Type.of.breach[i]
    type.random = severity_insample$Case_Type_Random[i]
    date = severity_insample$Year.of.Breach[i]    
    
    mask.df.frequecy= frequency_insample$Company_ID==id & frequency_insample$Year==date & frequency_insample$RiskType== type 
    mask.none =  none_insample$Company_ID==id & none_insample$Year==date
    mask.random = random_insample$Company_ID==id & random_insample$Year==date & random_insample$RiskType== type.random 
    
    
    frequency_insample$N[mask.df.frequecy]=frequency_insample$N[mask.df.frequecy]+1
    none_insample$N[mask.none]=none_insample$N[mask.none]+1
    random_insample$N[mask.random]=random_insample$N[mask.random]+1
    
    
  }
  
  print(paste0("sum after Loop1: ", sum(frequency_insample$N)))
  print(paste0("observation Loop2: ", nrow(severity_insample)))
  
  freq_insample_overall= c(freq_insample_overall,frequency_insample$N)
  none_insample_overall = c(none_insample_overall,none_insample$N )
  random_insample_overall = c(random_insample_overall,random_insample$N)
  
  
  max_val = max(unique(frequency_insample$N),unique(none_insample$N),unique(random_insample$N))
  

  Table_InSample_perYear_none["PRC", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                            table(factor(none_insample$N, levels = 0:max_val)))$p.value
  
  Table_InSample_perYear_random["PRC", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                              table(factor(random_insample$N, levels = 0:max_val)))$p.value
  
  
  #OUT OF SAMPLE
  loop.vector = 1:nrow(severity_outsample)
  for( i in loop.vector){
    id = severity_outsample$Company[i]
    type = severity_outsample$Type.of.breach[i]
    type.random = severity_outsample$Case_Type_Random[i]
    date = severity_outsample$Year.of.Breach[i]    
   
    
    mask.df.frequecy= frequency_outsample$Company_ID==id & frequency_outsample$Year==date & frequency_outsample$RiskType== type 
    mask.none =  none_outsample$Company_ID==id & none_outsample$Year==date 
    mask.random = random_outsample$Company_ID==id & random_outsample$Year==date & random_outsample$RiskType== type.random 
    
    frequency_outsample$N[mask.df.frequecy]=frequency_outsample$N[mask.df.frequecy]+1
    none_outsample$N[mask.none]=none_outsample$N[mask.none]+1
    random_outsample$N[mask.random]=random_outsample$N[mask.random]+1
  }
  
  freq_outsample_overall = c(freq_outsample_overall,frequency_outsample$N)
  none_outsample_overall = c(none_outsample_overall,none_outsample$N)
  random_outsample_overall = c(random_outsample_overall,random_outsample$N)
  
  
  max_val = max(unique(frequency_outsample$N),unique(none_outsample$N),unique(random_outsample$N))
  
  Table_OutSample_perYear_none["PRC", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                             table(factor(none_outsample$N, levels = 0:max_val)))$p.value
  
  Table_OutSample_perYear_random["PRC", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                               table(factor(random_outsample$N, levels = 0:max_val)))$p.value
  
  
  
}

max_val = max(unique(freq_outsample_overall),unique(none_outsample_overall),unique(random_outsample_overall))

Table_Insample_overall["PRC", "None"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                       table(factor(none_insample_overall, levels = 0:max_val)))$p.value

Table_Insample_overall["PRC", "Random"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                         table(factor(random_insample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["PRC", "None"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                        table(factor(none_outsample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["PRC", "Random"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                          table(factor(random_outsample_overall, levels = 0:max_val)))$p.value




#### TAIL ####

classification = "Tail"
df.frequency = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_",classification,".csv"))
df.none =read.csv(paste0(data_dir,"Covariates_Frequency_PRC_None.csv")) 
df.random = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_Random.csv")) 

freq_insample_overall=vector()
none_insample_overall = vector()
random_insample_overall = vector()

freq_outsample_overall=vector()
none_outsample_overall = vector()
random_outsample_overall = vector()

for(w in 1:length(years_out_of_sample)){
  print(paste0("Year: ",years_out_of_sample[w]))
  severity_insample = subset(df.severity, df.severity$Year.of.Breach<=years_out_of_sample[w]-1  & df.severity$Year.of.Breach>=years_out_of_sample[w]-1 -4)
  severity_outsample = subset(df.severity, df.severity$Year.of.Breach== years_out_of_sample[w])
  
  frequency_insample = subset(df.frequency, df.frequency$Year<=years_out_of_sample[w]-1  & df.frequency$Year>=years_out_of_sample[w]-1 -4)
  frequency_outsample = subset(df.frequency, df.frequency$Year==years_out_of_sample[w])
  
  none_insample = subset(df.none, df.none$Year<=years_out_of_sample[w]-1  & df.none$Year>=years_out_of_sample[w]-1 -4)
  none_outsample = subset(df.none, df.none$Year==years_out_of_sample[w])
  
  random_insample = subset(df.random, df.random$Year<=years_out_of_sample[w]-1  & df.random$Year>=years_out_of_sample[w]-1 -4)
  random_outsample = subset(df.random, df.random$Year==years_out_of_sample[w])
  print(paste0("before Loop1: ", max(frequency_insample$N)))
  print(paste0("before Loop2: ", max(none_insample$N)))
  
  # INSAMPLE
  loop.vector = 1:nrow(severity_insample)
  for( i in loop.vector){
    id = severity_insample$Company[i]
    type = severity_insample$Case_Type_Residual_Tail[i]
    type.random = severity_insample$Case_Type_Random[i]
    date = severity_insample$Year.of.Breach[i]    
    
    mask.df.frequecy= frequency_insample$Company_ID==id & frequency_insample$Year==date & frequency_insample$RiskType== type 
    mask.none =  none_insample$Company_ID==id & none_insample$Year==date
    mask.random = random_insample$Company_ID==id & random_insample$Year==date & random_insample$RiskType== type.random 
    
    
    frequency_insample$N[mask.df.frequecy]=frequency_insample$N[mask.df.frequecy]+1
    none_insample$N[mask.none]=none_insample$N[mask.none]+1
    random_insample$N[mask.random]=random_insample$N[mask.random]+1
    
    
  }
  
  print(paste0("sum after Loop1: ", sum(frequency_insample$N)))
  print(paste0("observation Loop2: ", nrow(severity_insample)))
  
  freq_insample_overall= c(freq_insample_overall,frequency_insample$N)
  none_insample_overall = c(none_insample_overall,none_insample$N )
  random_insample_overall = c(random_insample_overall,random_insample$N)
  
  
  max_val = max(unique(frequency_insample$N),unique(none_insample$N),unique(random_insample$N))
  
  Table_InSample_perYear_none["Tail", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                        table(factor(none_insample$N, levels = 0:max_val)))$p.value
  
  Table_InSample_perYear_random["Tail", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                          table(factor(random_insample$N, levels = 0:max_val)))$p.value
  
  
  #OUT OF SAMPLE
  loop.vector = 1:nrow(severity_outsample)
  for( i in loop.vector){
    id = severity_outsample$Company[i]
    type = severity_outsample$Case_Type_Residual_Tail[i]
    type.random = severity_outsample$Case_Type_Random[i]
    date = severity_outsample$Year.of.Breach[i]    
    
    
    mask.df.frequecy= frequency_outsample$Company_ID==id & frequency_outsample$Year==date & frequency_outsample$RiskType== type 
    mask.none =  none_outsample$Company_ID==id & none_outsample$Year==date 
    mask.random = random_outsample$Company_ID==id & random_outsample$Year==date & random_outsample$RiskType== type.random 
    
    frequency_outsample$N[mask.df.frequecy]=frequency_outsample$N[mask.df.frequecy]+1
    none_outsample$N[mask.none]=none_outsample$N[mask.none]+1
    random_outsample$N[mask.random]=random_outsample$N[mask.random]+1
  }
  
  freq_outsample_overall = c(freq_outsample_overall,frequency_outsample$N)
  none_outsample_overall = c(none_outsample_overall,none_outsample$N)
  random_outsample_overall = c(random_outsample_overall,random_outsample$N)
  
  
  max_val = max(unique(frequency_outsample$N),unique(none_outsample$N),unique(random_outsample$N))
  
  Table_OutSample_perYear_none["Tail", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                         table(factor(none_outsample$N, levels = 0:max_val)))$p.value
  
  Table_OutSample_perYear_random["Tail", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                           table(factor(random_outsample$N, levels = 0:max_val)))$p.value
  
  
  
}

max_val = max(unique(freq_outsample_overall),unique(none_outsample_overall),unique(random_outsample_overall))

Table_Insample_overall["Tail", "None"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                   table(factor(none_insample_overall, levels = 0:max_val)))$p.value

Table_Insample_overall["Tail", "Random"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                     table(factor(random_insample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["Tail", "None"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                    table(factor(none_outsample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["Tail", "Random"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                      table(factor(random_outsample_overall, levels = 0:max_val)))$p.value



#### BODY ####

classification = "Body"
df.frequency = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_",classification,".csv"))
df.none =read.csv(paste0(data_dir,"Covariates_Frequency_PRC_None.csv")) 
df.random = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_Random.csv")) 

freq_insample_overall=vector()
none_insample_overall = vector()
random_insample_overall = vector()

freq_outsample_overall=vector()
none_outsample_overall = vector()
random_outsample_overall = vector()

for(w in 1:length(years_out_of_sample)){
  print(paste0("Year: ",years_out_of_sample[w]))
  severity_insample = subset(df.severity, df.severity$Year.of.Breach<=years_out_of_sample[w]-1  & df.severity$Year.of.Breach>=years_out_of_sample[w]-1 -4)
  severity_outsample = subset(df.severity, df.severity$Year.of.Breach== years_out_of_sample[w])
  
  frequency_insample = subset(df.frequency, df.frequency$Year<=years_out_of_sample[w]-1  & df.frequency$Year>=years_out_of_sample[w]-1 -4)
  frequency_outsample = subset(df.frequency, df.frequency$Year==years_out_of_sample[w])
  
  none_insample = subset(df.none, df.none$Year<=years_out_of_sample[w]-1  & df.none$Year>=years_out_of_sample[w]-1 -4)
  none_outsample = subset(df.none, df.none$Year==years_out_of_sample[w])
  
  random_insample = subset(df.random, df.random$Year<=years_out_of_sample[w]-1  & df.random$Year>=years_out_of_sample[w]-1 -4)
  random_outsample = subset(df.random, df.random$Year==years_out_of_sample[w])
  print(paste0("before Loop1: ", max(frequency_insample$N)))
  print(paste0("before Loop2: ", max(none_insample$N)))
  
  # INSAMPLE
  loop.vector = 1:nrow(severity_insample)
  for( i in loop.vector){
    id = severity_insample$Company[i]
    type = severity_insample$Case_Type_Residual_Body[i]
    type.random = severity_insample$Case_Type_Random[i]
    date = severity_insample$Year.of.Breach[i]    
    
    mask.df.frequecy= frequency_insample$Company_ID==id & frequency_insample$Year==date & frequency_insample$RiskType== type 
    mask.none =  none_insample$Company_ID==id & none_insample$Year==date
    mask.random = random_insample$Company_ID==id & random_insample$Year==date & random_insample$RiskType== type.random 
    
    
    frequency_insample$N[mask.df.frequecy]=frequency_insample$N[mask.df.frequecy]+1
    none_insample$N[mask.none]=none_insample$N[mask.none]+1
    random_insample$N[mask.random]=random_insample$N[mask.random]+1
    
    
  }
  
  print(paste0("sum after Loop1: ", sum(frequency_insample$N)))
  print(paste0("observation Loop2: ", nrow(severity_insample)))
  
  freq_insample_overall= c(freq_insample_overall,frequency_insample$N)
  none_insample_overall = c(none_insample_overall,none_insample$N )
  random_insample_overall = c(random_insample_overall,random_insample$N)
  
  
  max_val = max(unique(frequency_insample$N),unique(none_insample$N),unique(random_insample$N))
  
  Table_InSample_perYear_none["Body", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                         table(factor(none_insample$N, levels = 0:max_val)))$p.value
  
  Table_InSample_perYear_random["Body", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                           table(factor(random_insample$N, levels = 0:max_val)))$p.value
  
  
  #OUT OF SAMPLE
  loop.vector = 1:nrow(severity_outsample)
  for( i in loop.vector){
    id = severity_outsample$Company[i]
    type = severity_outsample$Case_Type_Residual_Body[i]
    type.random = severity_outsample$Case_Type_Random[i]
    date = severity_outsample$Year.of.Breach[i]    
    
    
    mask.df.frequecy= frequency_outsample$Company_ID==id & frequency_outsample$Year==date & frequency_outsample$RiskType== type 
    mask.none =  none_outsample$Company_ID==id & none_outsample$Year==date 
    mask.random = random_outsample$Company_ID==id & random_outsample$Year==date & random_outsample$RiskType== type.random 
    
    frequency_outsample$N[mask.df.frequecy]=frequency_outsample$N[mask.df.frequecy]+1
    none_outsample$N[mask.none]=none_outsample$N[mask.none]+1
    random_outsample$N[mask.random]=random_outsample$N[mask.random]+1
  }
  
  freq_outsample_overall = c(freq_outsample_overall,frequency_outsample$N)
  none_outsample_overall = c(none_outsample_overall,none_outsample$N)
  random_outsample_overall = c(random_outsample_overall,random_outsample$N)
  
  
  max_val = max(unique(frequency_outsample$N),unique(none_outsample$N),unique(random_outsample$N))
  
  Table_OutSample_perYear_none["Body", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                          table(factor(none_outsample$N, levels = 0:max_val)))$p.value
  
  Table_OutSample_perYear_random["Body", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                            table(factor(random_outsample$N, levels = 0:max_val)))$p.value
  
  
  
}

max_val = max(unique(freq_outsample_overall),unique(none_outsample_overall),unique(random_outsample_overall))

Table_Insample_overall["Body", "None"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                    table(factor(none_insample_overall, levels = 0:max_val)))$p.value

Table_Insample_overall["Body", "Random"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                      table(factor(random_insample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["Body", "None"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                     table(factor(none_outsample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["Body", "Random"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                       table(factor(random_outsample_overall, levels = 0:max_val)))$p.value

#### FRESEV ####

classification = "FRESEV"
df.frequency = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_",classification,".csv"))
df.none =read.csv(paste0(data_dir,"Covariates_Frequency_PRC_None.csv")) 
df.random = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_Random.csv")) 

freq_insample_overall=vector()
none_insample_overall = vector()
random_insample_overall = vector()

freq_outsample_overall=vector()
none_outsample_overall = vector()
random_outsample_overall = vector()

for(w in 1:length(years_out_of_sample)){
  print(paste0("Year: ",years_out_of_sample[w]))
  severity_insample = subset(df.severity, df.severity$Year.of.Breach<=years_out_of_sample[w]-1  & df.severity$Year.of.Breach>=years_out_of_sample[w]-1 -4)
  
  severity_insample = create_matrix_frequency_severity_classification_PRC(df.losses = severity_insample)
  
  severity_outsample = subset(df.severity, df.severity$Year.of.Breach== years_out_of_sample[w])
  
  severity_outsample = forecast_matrix_frequency_severity_classification_PRC(df.losses = severity_insample,
                                                                         df.out_of_sample =severity_outsample)
  
  
  frequency_insample = subset(df.frequency, df.frequency$Year<=years_out_of_sample[w]-1  & df.frequency$Year>=years_out_of_sample[w]-1 -4)
  frequency_outsample = subset(df.frequency, df.frequency$Year==years_out_of_sample[w])
  
  none_insample = subset(df.none, df.none$Year<=years_out_of_sample[w]-1  & df.none$Year>=years_out_of_sample[w]-1 -4)
  none_outsample = subset(df.none, df.none$Year==years_out_of_sample[w])
  
  random_insample = subset(df.random, df.random$Year<=years_out_of_sample[w]-1  & df.random$Year>=years_out_of_sample[w]-1 -4)
  random_outsample = subset(df.random, df.random$Year==years_out_of_sample[w])
  print(paste0("before Loop1: ", max(frequency_insample$N)))
  print(paste0("before Loop2: ", max(none_insample$N)))
  
  # INSAMPLE
  loop.vector = 1:nrow(severity_insample)
  for( i in loop.vector){
    id = severity_insample$Company[i]
    type = severity_insample$Case_Type_Matrix_FrequencySeverity[i]
    type.random = severity_insample$Case_Type_Random[i]
    date = severity_insample$Year.of.Breach[i]    
    
    mask.df.frequecy= frequency_insample$Company_ID==id & frequency_insample$Year==date & frequency_insample$RiskType== type 
    mask.none =  none_insample$Company_ID==id & none_insample$Year==date
    mask.random = random_insample$Company_ID==id & random_insample$Year==date & random_insample$RiskType== type.random 
    
    
    frequency_insample$N[mask.df.frequecy]=frequency_insample$N[mask.df.frequecy]+1
    none_insample$N[mask.none]=none_insample$N[mask.none]+1
    random_insample$N[mask.random]=random_insample$N[mask.random]+1
    
    
  }
  
  print(paste0("sum after Loop1: ", sum(frequency_insample$N)))
  print(paste0("observation Loop2: ", nrow(severity_insample)))
  
  freq_insample_overall= c(freq_insample_overall,frequency_insample$N)
  none_insample_overall = c(none_insample_overall,none_insample$N )
  random_insample_overall = c(random_insample_overall,random_insample$N)
  
  
  max_val = max(unique(frequency_insample$N),unique(none_insample$N),unique(random_insample$N))
  
  Table_InSample_perYear_none["FRESEV", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                         table(factor(none_insample$N, levels = 0:max_val)))$p.value
  
  Table_InSample_perYear_random["FRESEV", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                           table(factor(random_insample$N, levels = 0:max_val)))$p.value
  
  
  #OUT OF SAMPLE
  loop.vector = 1:nrow(severity_outsample)
  for( i in loop.vector){
    id = severity_outsample$Company[i]
    type = severity_outsample$Case_Type_Matrix_FrequencySeverity[i]
    type.random = severity_outsample$Case_Type_Random[i]
    date = severity_outsample$Year.of.Breach[i]    
    
    
    mask.df.frequecy= frequency_outsample$Company_ID==id & frequency_outsample$Year==date & frequency_outsample$RiskType== type 
    mask.none =  none_outsample$Company_ID==id & none_outsample$Year==date 
    mask.random = random_outsample$Company_ID==id & random_outsample$Year==date & random_outsample$RiskType== type.random 
    
    frequency_outsample$N[mask.df.frequecy]=frequency_outsample$N[mask.df.frequecy]+1
    none_outsample$N[mask.none]=none_outsample$N[mask.none]+1
    random_outsample$N[mask.random]=random_outsample$N[mask.random]+1
  }
  
  freq_outsample_overall = c(freq_outsample_overall,frequency_outsample$N)
  none_outsample_overall = c(none_outsample_overall,none_outsample$N)
  random_outsample_overall = c(random_outsample_overall,random_outsample$N)
  
  
  max_val = max(unique(frequency_outsample$N),unique(none_outsample$N),unique(random_outsample$N))
  
  Table_OutSample_perYear_none["FRESEV", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                          table(factor(none_outsample$N, levels = 0:max_val)))$p.value
  
  Table_OutSample_perYear_random["FRESEV", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                            table(factor(random_outsample$N, levels = 0:max_val)))$p.value
  
  
  
}

max_val = max(unique(freq_outsample_overall),unique(none_outsample_overall),unique(random_outsample_overall))

Table_Insample_overall["FRESEV", "None"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                    table(factor(none_insample_overall, levels = 0:max_val)))$p.value

Table_Insample_overall["FRESEV", "Random"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                      table(factor(random_insample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["FRESEV", "None"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                     table(factor(none_outsample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["FRESEV", "Random"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                       table(factor(random_outsample_overall, levels = 0:max_val)))$p.value


#### TYPE IMPORTANCE ####

classification = "TYPEIMP"
df.frequency = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_",classification,".csv"))
df.none =read.csv(paste0(data_dir,"Covariates_Frequency_PRC_None.csv")) 
df.random = read.csv(paste0(data_dir,"Covariates_Frequency_PRC_Random.csv")) 

freq_insample_overall=vector()
none_insample_overall = vector()
random_insample_overall = vector()

freq_outsample_overall=vector()
none_outsample_overall = vector()
random_outsample_overall = vector()

for(w in 1:length(years_out_of_sample)){
  print(paste0("Year: ",years_out_of_sample[w]))
  severity_insample = subset(df.severity, df.severity$Year.of.Breach<=years_out_of_sample[w]-1  & df.severity$Year.of.Breach>=years_out_of_sample[w]-1 -4)
  
  severity_insample = create_matrix_type_importance_classification_PRC (df.losses = severity_insample,
                                                                        disruption = disruption,
                                                                        exfiltration_malware=exfiltration_malware,
                                                                        low_level_scanning=low_level_scanning,
                                                                        other="other", 
                                                                        sectors  = unique(severity_insample$Type.of.organization))
  
  
  severity_outsample = subset(df.severity, df.severity$Year.of.Breach== years_out_of_sample[w])
  
  severity_outsample = forecast_matrix_type_importance_classification_PRC(df.losses =severity_insample ,
                                                                          df.out_of_sample =severity_outsample ,
                                                                          disruption =disruption,
                                                                          exfiltration_malware = exfiltration_malware,
                                                                          low_level_scanning = low_level_scanning,
                                                                          other = "other",
                                                                          sectors = unique(severity_insample$Type.of.organization) )
  
  
  frequency_insample = subset(df.frequency, df.frequency$Year<=years_out_of_sample[w]-1  & df.frequency$Year>=years_out_of_sample[w]-1 -4)
  frequency_outsample = subset(df.frequency, df.frequency$Year==years_out_of_sample[w])
  
  none_insample = subset(df.none, df.none$Year<=years_out_of_sample[w]-1  & df.none$Year>=years_out_of_sample[w]-1 -4)
  none_outsample = subset(df.none, df.none$Year==years_out_of_sample[w])
  
  random_insample = subset(df.random, df.random$Year<=years_out_of_sample[w]-1  & df.random$Year>=years_out_of_sample[w]-1 -4)
  random_outsample = subset(df.random, df.random$Year==years_out_of_sample[w])
  print(paste0("before Loop1: ", max(frequency_insample$N)))
  print(paste0("before Loop2: ", max(none_insample$N)))
  
 
  
  # INSAMPLE
  loop.vector = 1:nrow(severity_insample)
  for( i in loop.vector){
    id = severity_insample$Company[i]
    type = severity_insample$Case_Type_Matrix_TypeImportance[i]
    type.random = severity_insample$Case_Type_Random[i]
    date = severity_insample$Year.of.Breach[i]    
    
    mask.df.frequecy= frequency_insample$Company_ID==id & frequency_insample$Year==date & frequency_insample$RiskType== type 
    mask.none =  none_insample$Company_ID==id & none_insample$Year==date
    mask.random = random_insample$Company_ID==id & random_insample$Year==date & random_insample$RiskType== type.random 
    
    
    frequency_insample$N[mask.df.frequecy]=frequency_insample$N[mask.df.frequecy]+1
    none_insample$N[mask.none]=none_insample$N[mask.none]+1
    random_insample$N[mask.random]=random_insample$N[mask.random]+1
    
    
  }
  
  print(paste0("sum after Loop1: ", sum(frequency_insample$N)))
  print(paste0("observation Loop2: ", nrow(severity_insample)))
  
  freq_insample_overall= c(freq_insample_overall,frequency_insample$N)
  none_insample_overall = c(none_insample_overall,none_insample$N )
  random_insample_overall = c(random_insample_overall,random_insample$N)
  
  
  max_val = max(unique(frequency_insample$N),unique(none_insample$N),unique(random_insample$N))
  
  Table_InSample_perYear_none["TYPEIMP", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                           table(factor(none_insample$N, levels = 0:max_val)))$p.value
  
  Table_InSample_perYear_random["TYPEIMP", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_insample$N, levels = 0:max_val)),
                                                                                             table(factor(random_insample$N, levels = 0:max_val)))$p.value
  
  
  #OUT OF SAMPLE
  loop.vector = 1:nrow(severity_outsample)
  for( i in loop.vector){
    id = severity_outsample$Company[i]
    type = severity_outsample$Case_Type_Matrix_TypeImportance[i]
    type.random = severity_outsample$Case_Type_Random[i]
    date = severity_outsample$Year.of.Breach[i]    
    
    
    mask.df.frequecy= frequency_outsample$Company_ID==id & frequency_outsample$Year==date & frequency_outsample$RiskType== type 
    mask.none =  none_outsample$Company_ID==id & none_outsample$Year==date 
    mask.random = random_outsample$Company_ID==id & random_outsample$Year==date & random_outsample$RiskType== type.random 
    
    frequency_outsample$N[mask.df.frequecy]=frequency_outsample$N[mask.df.frequecy]+1
    none_outsample$N[mask.none]=none_outsample$N[mask.none]+1
    random_outsample$N[mask.random]=random_outsample$N[mask.random]+1
  }
  
  freq_outsample_overall = c(freq_outsample_overall,frequency_outsample$N)
  none_outsample_overall = c(none_outsample_overall,none_outsample$N)
  random_outsample_overall = c(random_outsample_overall,random_outsample$N)
  
  
  max_val = max(unique(frequency_outsample$N),unique(none_outsample$N),unique(random_outsample$N))
  
  Table_OutSample_perYear_none["TYPEIMP", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                            table(factor(none_outsample$N, levels = 0:max_val)))$p.value
  
  Table_OutSample_perYear_random["TYPEIMP", as.character(years_out_of_sample[w])] = chisq.test(table(factor(frequency_outsample$N, levels = 0:max_val)),
                                                                                              table(factor(random_outsample$N, levels = 0:max_val)))$p.value
  
  
  
}

max_val = max(unique(freq_outsample_overall),unique(none_outsample_overall),unique(random_outsample_overall))

Table_Insample_overall["TYPEIMP", "None"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                      table(factor(none_insample_overall, levels = 0:max_val)))$p.value

Table_Insample_overall["TYPEIMP", "Random"] = chisq.test(table(factor(freq_insample_overall, levels = 0:max_val)),
                                                        table(factor(random_insample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["TYPEIMP", "None"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                       table(factor(none_outsample_overall, levels = 0:max_val)))$p.value

Table_Outsample_overall["TYPEIMP", "Random"] = chisq.test(table(factor(freq_outsample_overall, levels = 0:max_val)),
                                                         table(factor(random_outsample_overall, levels = 0:max_val)))$p.value

##SAVE TABLES ####
write.csv(Table_Outsample_overall,
          paste0(output,"Table_Outsample_overall_PRC.csv"))


write.csv(Table_Insample_overall,
          paste0(output,"Table_Insample_overall_PRC.csv"))



write.csv(Table_OutSample_perYear_random,
          paste0(output,"Table_OutSample_perYear_random_PRC.csv"))

write.csv(Table_InSample_perYear_random,
          paste0(output,"Table_InSample_perYear_random_PRC.csv"))


write.csv(Table_OutSample_perYear_none,
          paste0(output,"Table_OutSample_perYear_none_PRC.csv"))

write.csv(Table_InSample_perYear_none,
          paste0(output,"Table_InSample_perYear_none_PRC.csv"))

