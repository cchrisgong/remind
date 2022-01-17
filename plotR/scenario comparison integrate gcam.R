
# ---
# title: "R Notebook"
# output:
#   html_document: default
#   html_notebook: default
#   pdf_document: default
# ---
# 
# 
# # Data analysis and plotting using quitte
# 
# Contact: Chris Gong
# 
# ## Load necessary libraries
# ---
# title: "R Notebook"
# output:
#   html_document: default
#   html_notebook: default
#   pdf_document: default
# ---
# bespoke script for plotting to compare to G-CAM outputs

require(dplyr)
require(tidyr)
require(ggplot2)
require(quitte)
require(mip)
require(purrr)

require(plotly) #install.packages("plotly")
library(magick) #install.packages("magick")
library(magrittr) #install.packages("magrittr")
require(stringr)

############################################################
############################################################


setwd('~/source/REMIND_integrate/remind/output')
run_number = "REMIND3"

data.dir = paste0("./",run_number)

plot.dir = "./plots"

regs = c("World", "CHA", "EUR")
# regs = c("World","CHA","EUR","JPN","USA","OAS")
# regs = c("World","CHA","CAZ","EUR","IND","LAM","MEA","NEU","REF","SSA","JPN","USA","OAS")


plot.period <- seq(2015,2060,5)

## ARIADNE-specific analysis: Electrif vs. H2 vs. Synfuel 

scenarios = c(
  "REMIND_generic_baseline_bIT",
  "REMIND_generic_ndc_bIT",
  "REMIND_generic_2c_bal_bIT_NZ",
  # "REMIND_generic_2c_Elec_bIT",
  "REMIND_generic_wb2c_bal_bIT_NZ",
  # "REMIND_generic_wb2c_Elec_bIT",
  "REMIND_generic_1p5c_bal_bIT_NZ",
  "REMIND_generic_1p5c_Elec_bIT",
  "REMIND_generic_1p5c_H2_bIT",
  "REMIND_generic_1p5c_BECCS_bIT",
  NULL)

scens_short =
  c(
    "Baseline",
    "NDC",
    "2C netzero bal",
    # "2C netzero DirEl",
    "WB2C netzero bal",
    # "WB2C netzero DirEl",
    "1.5C netzero bal",
    "1.5C netzero DirEl",
    "1.5C netzero H2",
    "1.5C netzero BECCS",
    NULL)

data.files = lapply(scenarios, function(x) paste0( data.dir, "/", x, ".mif")) %>% unlist

run_indices = seq(from = 1, to = length(data.files), by = 1)
df.raw = NULL
# trim df to chosen scenarios and regions
for (i in run_indices){
  print(i)
  # REMIND.data = read.quitte(data.files[[9]])
  REMIND.data = read.quitte(data.files[[i]])
  
  df.raw <- rbind(df.raw, REMIND.data)
}

scens = stringr::str_replace(scenarios, "REMIND_generic_", "")

df.allregs =  filter(df.raw, scenario %in% scens, region %in% regs ) %>%
  order.levels(scenario = scens) %>%
  factor.data.frame()

rm(df.raw)

############################################################
############################################################

tmax = 2060

# also read in historical data

df.hist0 = read.quitte(paste0("./",run_number, "/historical.mif"))

df.hist = filter(df.hist0, model =="CEDS", region %in% regs ) %>%
  factor.data.frame() 

rm(df.hist0)
####################################################################################################################################################################################


# rescale price and cost data to 2020 using 2005->2020 deflator of 1.34 (https://stats.oecd.org/Index.aspx?DataSetCode=PRICES_CPI)

indices = grepl("US\\$2005",df.allregs$unit)
df.allregs[indices, "value"] = df.allregs[indices, "value"] * 1.2
levels(df.allregs$unit) = gsub("US\\$2005", "US\\$2015", levels(df.allregs$unit))

df.allregs$scenario <- plyr::mapvalues(df.allregs$scenario, from = scens, to = scens_short)
df<-df.allregs

df <- calc_addVariable(df,
                       "`PE|Biomass|Solids|Traditional`"  =  "`PE|Biomass|Traditional`", 
                       "`PE|Biomass|Solids|Modern`"  =  "`PE|Biomass|Solids` - `PE|Biomass|Traditional`", 
                       units  = "EJ/yr") 


# calculate other fossil (coal + oil) and other renewables categories for electricity generation shown in some plots
df <- df %>% 
  calc_addVariable("`SE|Electricity|Other Renewables`" = "`SE|Electricity|Biomass|w/ CCS`+`SE|Electricity|Biomass|w/o CCS`+`SE|Electricity|Solar|CSP`+`SE|Electricity|Hydro`+`SE|Electricity|Geothermal`",
                   "`SE|Electricity|Other Fossil`" =  "`SE|Electricity|Coal|w/ CCS` + `SE|Electricity|Coal|w/o CCS` + `SE|Electricity|Oil`", 
                   units = "EJ/yr") 


# calculate additional categories of CO2 capture
df <- df %>% 
  calc_addVariable("`Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Other w/ couple prod`" = "`Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Electricity w/ couple prod` + `Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Heat w/ couple prod`+`Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Solids w/ couple prod`+`Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Gases w/ couple prod`", units = "MtCO2/yr")


# oil and gas consumption per sector
vars <- c(
  # liquids
  "FE|Buildings|Liquids|+|Fossil",
  "FE|Industry|Liquids|+|Fossil",
  "FE|Transport|Liquids|+|Fossil",
  # gases
  "FE|Buildings|Gases|+|Fossil",
  "FE|Industry|Gases|+|Fossil",
  "FE|Transport|Gases|+|Fossil",
  "FE|Transport|Gases|+|Fossil"
)

plot.vars.order <- c("Fossil Gases",
                    "Fossil Liquids")

plot.vars.color <- c("Fossil Gases" = "gray90",
                     "Fossil Liquids" = "gray40")

df_test = filter(df, grepl("FE|Buildings|Liquids|+|", variable, fixed=TRUE))

df.FE.sec <- df %>% 
  filter(variable %in% vars, period %in% plot.period) %>%
  # add bar to be able to retrieve for dimensions of variables in the next step
  mutate( variable = paste0(variable,"| "))

# spread variables label across dimensions (to obtain sector dimensions and energy carrier dimension)
df.FE.sec$sector <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$sector <- sapply(df.FE.sec$sector, "[[", 2)

df.FE.sec$encar <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$encar <- sapply(df.FE.sec$encar, "[[", 3)

df.FE.sec$origin <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$origin <- sapply(df.FE.sec$origin, "[[", 4)

df.FE.sec$origin2 <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$origin2 <- sapply(df.FE.sec$origin2, "[[", 5)

df.FE.sec.plot <- df.FE.sec %>% 
  mutate(encar = ifelse( origin == "+",
                         paste(origin2, encar), encar)) %>% 
  mutate( encar = ifelse( encar == "+", origin, encar)) %>% 
  revalue.levels(encar = c("Hydrogen Liquids" = "Synthetic Liquids",
                           "Hydrogen Gases" = "Synthetic Gases")) %>% 
  order.levels(encar = plot.vars.order, scenario = scens_short)

for(reg in regs){
  
  p.FE.sec <- ggplot(df.FE.sec.plot %>% filter(region == reg)) +
    geom_col(aes(period, value, fill=encar), alpha=0.8) +
    scale_fill_manual(values=plot.vars.color) +
    facet_wrap(sector~scenario, ncol = length(scens), scales = 'free_y') +
    scale_x_continuous("Year") +
    scale_y_continuous("Final Energy\n(EJ/yr)") +
    theme_bw()+
    theme(strip.background = element_blank(), text = element_text(size=12),
          axis.text.x = element_text(angle=90, vjust = 0.5)) +
    theme(legend.position = 'right', legend.title=element_blank())
  
  # p.FE.sec 
  
  ggsave(plot = p.FE.sec, filename = paste0(plot.dir, "/", run_number,"_",reg,"_FE_sec_oilgas.png"),width=32, height=14, units = "cm")
}


