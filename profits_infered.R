profits_infered <- function(df_mod,
                            datevar,
                            month_ini,
                            month_end,
                            mod, 
                            exposure,
                            exposureatdefault,
                            fees,
                            target,
                            perc){
  
  library(dplyr)
  library(scorecardModelUtils)
  
  #datevar: Name of the date variable in format "yyyy-mm-dd" or "yyyy-mm"
  #month_ini: Initial month (Maximum a monthly basis is considered). Format "yyyy-mm"
  #month_end: Final month. Format "yyyy-mm"
  #mod: Name of the model (column with score or pd) without NAs
  #exposure: Name of the column with the initial exposure (price without commisions or interests)
  #exposureatdefault: Name of the column with the exposure when the customer defaults (at €)
  #fees: Column which collects commissions and other charges in euros (not in rates)
  #target: Name of the target. It must be a binary variable
  #perc: Porcentaje de rechazo
  
  df_select <- df_mod
    
  df_select$datevarc <- as.vector(df_select[, datevar])[[1]]
  df_select$exposurec <- as.vector(df_select[, exposure])[[1]]
  df_select$modc <- as.vector(df_select[, mod])[[1]]
  df_select$exposureatdefaultc <- as.vector(df_select[, exposureatdefault])[[1]]
  df_select$feesc <- as.vector(df_select[, fees])[[1]]
  df_select$targetc <- as.vector(df_select[, target])[[1]]
    
  df_select <- df_select %>%
    filter(substr(datevarc, 1, 7) >= month_ini,
           substr(datevarc, 1, 7) <= month_end)
  
  df_model_prev <- gini_table(as.data.frame(df_select),
                              target = target,
                              col_pred = mod,
                              quantile_pt = 100)$gini_tab 
    
  df_model_prev <- df_model_prev[-1,]
  
  gini_val <- gini_table(as.data.frame(df_select),
                         target = target,
                         col_pred = mod,
                         quantile_pt = 100)$gini_value 
    
  #Infered table
    
  cuts <- c( -Inf,
             as.numeric(df_model_prev$Upper_closed_bound))
    
  df_select$Grupo_Mod <- cut(as.vector(df_select[,mod])[[1]], breaks = cuts)
    
  df_model_sum <- df_select %>%
    group_by(Grupo_Mod) %>%
    summarise(total_exposure = sum(exposurec),
              exposure_at_default = sum(targetc * exposureatdefaultc),
              fees = sum(feesc)
    )
    
  NewTotal = round(sum(df_model_prev$Total) + 
                     (perc/(1- perc)) * 
                     sum(df_model_prev$Total), 0)
  
  NewPopSegm = c(round((perc/(1- perc)) * 
                         sum(df_model_prev$Total), 0), 
                 df_model_prev$Total)
  
  NewPorSegm = cumsum(NewPopSegm) / NewTotal
    
  #Amount & fees at rejected zone
    
  df0 = data.frame(Grupo_Mod = "Infered", 
                   total_exposure = mean(df_select$exposurec) * NewPopSegm[1],
                   exposure_at_default = NA,
                   fees = mean(df_select$feesc) * NewPopSegm[1]
                   )
    
  df_model_sum <- rbind(df0, df_model_sum)
    
  df_model_sum <- df_model_sum %>%
    mutate(risk = exposure_at_default / total_exposure,
           porc_total_exposure = cumsum(total_exposure) / sum(total_exposure))
  
    
  #Model Data
  explanatory = df_model_sum$porc_total_exposure[-1]
  explanatory2 = df_model_sum$porc_total_exposure[-1]**2
    
  explained = df_model_sum$risk[-1]
    
  model = lm(explained ~ explanatory +
               explanatory2)
    
  rate = as.numeric((model$coefficients[1] + 
                       model$coefficients[2] * perc + 
                       model$coefficients[3] * perc**2))
    
  df_model_sum$risk[1] = rate
  df_model_sum$exposure_at_default[1] = df_model_sum$total_exposure[1] * rate
  
  n_data = nrow(df_select)
  infered = NewPopSegm[1]

  Total_risk_rate = sum(df_model_sum$exposure_at_default) / sum(df_model_sum$total_exposure)
  risk_rate = rate  
  risk_avoided =  sum(df_model_sum$exposure_at_default[-1]) / sum(df_model_sum$total_exposure[-1])
  Total_Profits = sum(df_model_sum$fees) - sum(df_model_sum$exposure_at_default)
  Total_ProfitsMod = sum(df_model_sum$fees[-1]) - sum(df_model_sum$exposure_at_default[-1])

  return(list(n_data, 
              infered, 
              Total_risk_rate, 
              risk_rate, 
              risk_avoided, 
              gini_val, 
              Total_Profits, 
              Total_ProfitsMod)
         )
  
}



profits_infered(df_mod,
                "MONTH",
                "2023-08",
                "2023-08",
                "SCORE_01", 
                "Exposure",
                "EaD",
                "FEE",
                "TARGET",
                0.04)

