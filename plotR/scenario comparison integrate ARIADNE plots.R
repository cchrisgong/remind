
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
run_number = "REMIND9"

data.dir = paste0("./",run_number)

plot.dir = "./plots"

regs = c("World", "CHA", "EUR")
# regs = c("World","CHA","EUR","JPN","USA","OAS")
# regs = c("World","CHA","CAZ","EUR","IND","LAM","MEA","NEU","REF","SSA","JPN","USA","OAS")


plot.period <- seq(2015,2060,5)

## ARIADNE-specific analysis: Electrif vs. H2 vs. Synfuel 

scenarios = c(
  "REMIND_generic_SSP2EU-Base",
  "REMIND_generic_SSP2EU-NDC",
  "REMIND_generic_SSP2EU-NPi",
  "REMIND_generic_SSP2EU-PkBudg1520",
  "REMIND_generic_SSP2EU-PkBudg1020",
  "REMIND_generic_SSP2EU-PkBudg770",
  NULL)

scens_short =
  c(
    "Baseline",
    "NDC",
    "NPi",
    "2C",
    "WB2C",
    "1.5C",
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

df <- df %>% 
  calc_addVariable("`Cap|Electricity|Other Fossil`" =  "`Cap|Electricity|Coal` + `Cap|Electricity|Oil`", 
                   units = "GW")

# 
# #################### emission plot for Gunnar ##############################
# vars = c(     "Emi|CO2|Gross|Energy|Supply|+|Electricity",
#               "Emi|CO2|Gross|Energy|Supply|Non-electric",
#               
#               "Emi|CO2|Energy|Demand|+|Buildings",
#               "Emi|CO2|Energy|Demand|+|Transport",
#               "Emi|CO2|+|Industrial Processes",
#               "Emi|CO2|Energy|Demand|+|Industry",
#               "Emi|CO2|+|Land-Use Change",
#               "Emi|CO2|CDR|BECCS",
#               "Emi|CO2|CDR|DACCS",
#               "Emi|CO2|CDR|EW")
# # define colors and labels
# tt = plotstyle.add(c("Emi|CO2|CDR|EW")   ,c("EW"),  c("sandybrown"), replace = T)
# tt = plotstyle.add(c("Emi|CO2|CDR|BECCS")   ,c("BECCS"),  c("lightgreen"), replace = T)
# tt = plotstyle.add(c("Emi|CO2|CDR|DACCS")   ,c("DAC"),  c("cyan"), replace = T)
# tt = plotstyle.add(c("Emi|CO2|Energy|Demand|+|Industry")   ,c("Industry"),  c( "grey"),replace=T)
# tt = plotstyle.add(c("Emi|CO2|Energy|Demand|+|Transport")   ,c("Transport"),  c("darkblue"), replace = T)
# tt = plotstyle.add(c("Emi|CO2|Energy|Demand|+|Buildings")   ,c("Buildings"),  c( "darkred"), replace = T)
# tt = plotstyle.add(c("Emi|CO2|+|Land-Use Change")   ,c("Land-Use Change"),  c("darkgreen"),replace=T)
# tt = plotstyle.add(c("Emi|CO2|Gross|Energy|Supply|+|Electricity")   ,c("Elec. Supply"),  c("darkgoldenrod"),replace=T)
# tt = plotstyle.add(c("Emi|CO2|Gross|Energy|Supply|Non-electric")   ,c("Non-elec. Supply"),  c("darkorchid"),replace=T)
# tt = plotstyle.add(c("Emi|CO2|+|Industrial Processes"),c("Industrial Processes"),  c("palevioletred"),replace=T)
# 
# plot.period <- seq(2015,2060,5)
# 
# df.emi.grossneg <- df %>% 
#   filter(variable %in% vars, period %in% plot.period, region == "CHA") %>% 
#   order.levels(variable = vars)
# 
# df.emi.tot <- df %>% 
#   filter(variable == "Emi|CO2", period %in% plot.period, region == "CHA") %>% 
#   mutate(variable = "Total")
# 
# p.emi.grossneg <- ggplot() +
#   geom_col(data=df.emi.grossneg, aes(period, value, fill=variable), alpha=0.8) +
#   geom_line(data=df.emi.tot, aes(period, value, linetype=variable), size=1.2, alpha=0.7) +
#   geom_point(data=df.emi.tot, aes(period, value, shape=variable), size=1.5, alpha=0.7) +
#   facet_wrap(~scenario, ncol = length(getScenarios(df))) +
#   scale_y_continuous("CO2 Emissions (Mt CO2/yr)") +
#   scale_fill_manual(values = mip::plotstyle(vars), labels = mip::plotstyle(vars, out = "legend")) +
#   geom_hline(yintercept=0, size=0.8, linetype="dashed", alpha=0.7) +
#   
#   theme_bw() +
#   theme( axis.text.x = element_text(angle=90, vjust = 0.5),
#          legend.position = "bottom",strip.background = element_blank(),
#          legend.title = element_blank(),strip.text.y = element_text(angle = 0, hjust = 0))
# # show historical data for validation
# df.emi.tot.hist <- df.hist %>%
#   filter(variable == "Emi|CO2", period >= 1990, period <= max(plot.period), model == "CEDS", region == "CHA") %>% 
#   select(-scenario) %>% 
#   mutate(model = "historical")
# 
# p.emi.grossneg <- p.emi.grossneg +
#   geom_line(data=df.emi.tot.hist, aes(period, value, color=model), size=1.2, alpha=0.7) 
# 
# ggsave(plot = p.emi.grossneg, filename = paste0(plot.dir, "/Emi_CO2GrossNeg.png"), width=20, height=10, units = "cm")


vars <- c(
  # liquids
  "FE|Buildings|Liquids|+|Fossil",
  "FE|Buildings|Liquids|+|Biomass",
  "FE|Buildings|Liquids|+|Hydrogen",
  "FE|Industry|Liquids|+|Fossil",
  "FE|Industry|Liquids|+|Biomass",
  "FE|Industry|Liquids|+|Hydrogen",
  "FE|Transport|Liquids|+|Fossil",
  "FE|Transport|Liquids|+|Biomass",
  "FE|Transport|Liquids|+|Hydrogen",
  # gases
  "FE|Buildings|Gases|+|Fossil",
  "FE|Buildings|Gases|+|Biomass",
  "FE|Buildings|Gases|+|Hydrogen",
  "FE|Industry|Gases|+|Fossil",
  "FE|Industry|Gases|+|Biomass",
  "FE|Industry|Gases|+|Hydrogen",
  "FE|Transport|Gases|+|Fossil",
  "FE|Transport|Gases|+|Biomass",
  "FE|Transport|Gases|+|Hydrogen",
  # solids
  "FE|Buildings|+|Solids", "FE|Industry|+|Solids",
  # heat
  "FE|Buildings|+|Heat", "FE|Industry|+|Heat",
  # hydrogen
  "FE|Buildings|+|Hydrogen", 
  "FE|Industry|+|Hydrogen",
  "FE|Transport|+|Hydrogen",
  # electricity
  "FE|Buildings|+|Electricity", 
  "FE|Industry|+|Electricity",
  "FE|Transport|+|Electricity"
)

plot.vars.order <- c("Solids","Heat","Synthetic Gases","Biomass Gases","Fossil Gases",
                     "Synthetic Liquids","Biomass Liquids","Fossil Liquids",
                     "Hydrogen" ,"Electricity" )

plot.vars.color <- c("Solids" = "#191919","Heat" = "#cc0000",
                     "Synthetic Gases" = "plum2","Biomass Gases" = "olivedrab2",
                     "Fossil Gases" = "gray90",
                     "Synthetic Liquids" = "mediumpurple4",
                     "Biomass Liquids" = "olivedrab",
                     "Fossil Liquids" = "gray40",
                     "Hydrogen" = "#66cccc","Electricity" =   "#ffb200" )

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
      
      ggsave(plot = p.FE.sec, filename = paste0(plot.dir, "/", run_number,"_",reg,"_FE_sec_encar.png"),width=32, height=14, units = "cm")
}
### Sectoral Electrification

### set plot config
vars <- c("Transport El. Share", "BEV Share", "Buildings El. Share",
          "Industry El. Share")
vars.colors <- c("CO2 Price" = "black", "Transport El. Share" = "chocolate4",
                 "BEV Share" = "darkorange", "Buildings El. Share" = "grey",
                 "Industry El. Share" = "steelblue")

yax.scale <- 100
secax.scale <- 1500
###

# UE|Transport is there twice, remove duplicate
df <- df %>% 
  distinct(region, scenario, period, variable, .keep_all = T)

# calculate electricity shares

df2 <- df %>%
  select(scenario, region,variable, period, value) %>% 
  filter(variable == "Price|Carbon") %>% 
  # rescale to fit to second y axes
  mutate(value = value / secax.scale * yax.scale,
         sec.variable = "CO2 Price") %>% 
  select(scenario,region,period,sec.variable,`Price|Carbon`=value)

df.electrif <- df %>% 
  calc_addVariable("`Transport El. Share`" = "`FE|Transport|+|Electricity`/`FE|++|Transport`*100",
                   # "`BEV Share`" = "`Est EV LDV Stock`/`Est LDV Stock`*100",
                   "`Buildings El. Share`" = "`FE|Buildings|+|Electricity`/`FE|++|Buildings`*100",
                   "`Industry El. Share`" = "`FE|Industry|+|Electricity`/`FE|++|Industry`*100", units = "%") %>% 
  filter(variable %in% vars, period <= tmax) %>% 
  # join with carbon price as a new column
  left_join(df2) 


for(reg in regs){
# plot
p.Electrif <- ggplot(df.electrif%>% filter(region == reg)) +
  geom_line(aes(period, value, color=variable), size=1.2, alpha=0.7) +
  geom_point(aes(period, value, color=variable), size=2) +
  geom_line(aes(period, `Price|Carbon`, color=sec.variable),  size=1.2, alpha=0.7) +
  geom_point(aes(period, `Price|Carbon`, color=sec.variable), size=2) +
  scale_color_manual(values=vars.colors) +
  facet_wrap(~scenario, ncol = 4) +
  scale_x_continuous("Year") +
  scale_y_continuous("Electrification Share (%)", c(0,yax.scale),
                     sec.axis = sec_axis(~ . * secax.scale / yax.scale, 
                                         name = "CO2 Price\n(USD2005/tCO2)"),
                     breaks = seq(0,100,20)) +
  theme_bw()+
  theme(strip.background = element_blank(), text = element_text(size=12)) +
  theme(legend.position = 'bottom', legend.title=element_blank())

# p.Electrif              

ggsave(plot = p.Electrif, 
       filename = paste0(plot.dir, "/", run_number,"_",reg,"_Electrif_Sector.png"),
       width=30, height=12, units = "cm")
}

### Sectoral H2 use

### set plot config
vars <- c("Transport H2 Share", "FCEV Share", "Buildings H2 Share",
          "Industry H2 Share")
vars.colors <- c("CO2 Price" = "black", "Transport H2 Share" = "chocolate4",
                 "FCEV Share" = "darkorange", "Buildings H2 Share" = "grey",
                 "Industry H2 Share" = "steelblue")

yax.scale <- 50
secax.scale <- 1500
###

# UE|Transport is there twice, remove duplicate
df <- df %>% 
  distinct(region, scenario, period, variable, .keep_all = T)

df2 <- df %>%
  select(scenario, region,variable, period, value) %>% 
  filter(variable == "Price|Carbon") %>% 
  # rescale to fit to second y axes
  mutate(value = value / secax.scale * yax.scale,
         sec.variable = "CO2 Price") %>% 
  select(scenario,region,period,sec.variable,`Price|Carbon`=value)

# calculate H2 shares

df.H2Share <- df %>% 
  calc_addVariable("`Transport H2 Share`" = "`FE|Transport|+|Hydrogen`/`FE|++|Transport`*100",
                   # "`FCEV Share`" = "`Est H2 LDV Stock`/`Est LDV Stock`*100",
                   "`Buildings H2 Share`" = "`FE|Buildings|+|Hydrogen`/`FE|++|Buildings`*100",
                   "`Industry H2 Share`" = "`FE|Industry|+|Hydrogen`/`FE|++|Industry`*100", units = "%") %>% 
  filter(variable %in% vars, period <= tmax) %>% 
  # join with carbon price as a new column
  left_join(df2) 

# plot
for(reg in regs){
  if (reg == "CHA"){yax.scale <- 5}
  # df.H2Share2 <- df.H2Share%>% filter(region == reg)
p.H2Share <- ggplot(df.H2Share%>% filter(region == reg)) +
  geom_line(aes(period, value, color=variable), size=1.2, alpha=0.7) +
  geom_point(aes(period, value, color=variable), size=2) +
  geom_line(aes(period, `Price|Carbon`, color=sec.variable),  size=1.2, alpha=0.7) +
  geom_point(aes(period, `Price|Carbon`, color=sec.variable), size=2) +
  scale_color_manual(values=vars.colors) +
  facet_wrap(~scenario, ncol = 4) +
  scale_x_continuous("Year") +
  scale_y_continuous("H2 Share (%)", c(0,yax.scale),
                     sec.axis = sec_axis(~ . * secax.scale / yax.scale, 
                                         name = "CO2 Price\n(USD2005/tCO2)"),
                     breaks = c(seq(0,100,5))) +
  theme_bw()+
  theme(strip.background = element_blank(), text = element_text(size=12)) +
  theme(legend.position = 'bottom', legend.title=element_blank())

p.H2Share              


ggsave(plot = p.H2Share, 
       filename = paste0(plot.dir, "/", run_number,"_",reg,"_H2Share_Sector.png"),
       width=40, height=12, units = "cm")
}

### CO2 Capture Sources and Carbon Management

### plot with positive supply and negative CO2 demand bars
co2sup.focus.vars <- c("Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Other w/ couple prod",
                       "Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Hydrogen w/ couple prod",
                       "Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Liquids w/ couple prod",
                       "Carbon Management|Carbon Sources|+|Fossil|Pe2Se",
                       "Carbon Management|Carbon Sources|+|Industry Energy (Mt CO2/yr)",
                       "Carbon Management|Carbon Sources|+|Industry Process (Mt CO2/yr)",
                       "Carbon Management|Carbon Sources|+|DAC")

co2dem.focus.vars <- c("Carbon Management|Carbon Sinks|+|Storage",
                       "Carbon Management|Carbon Sinks|+|Synthetic Liquids",
                       "Carbon Management|Carbon Sinks|+|Synthetic Gases")

co2.sink.source.label <- c("Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Other w/ couple prod" = "Other Biomass w/ CC",
                           "Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Hydrogen w/ couple prod" = "CC from Biomass to H2 w/ CC",
                           "Carbon Management|Carbon Sources|Biomass|Pe2Se|+|Liquids w/ couple prod" = "CC from Biomass to Liquids w/ CC",
                           "Carbon Management|Carbon Sources|+|Fossil|Pe2Se" = "Pe2Se Fossil CC",
                           "Carbon Management|Carbon Sources|+|Industry Energy" = "industry energy CC",
                           "Carbon Management|Carbon Sources|+|Industry Process" = "industry process CC",
                           "Carbon Management|Carbon Sources|+|DAC" = "DAC",
                           "Carbon Management|Carbon Sinks|+|Storage" = "Underground Storage",
                           "Carbon Management|Carbon Sinks|+|Synthetic Liquids" = "Synthetic Liquids",
                           "Carbon Management|Carbon Sinks|+|Synthetic Gases" = "Synthetic Gases")



co2.sink.source.color <- c("Other Biomass w/ CC" = "seagreen1",
                           "CC from Biomass to H2 w/ CC" = "lightseagreen",
                           "CC from Biomass to Liquids w/ CC" = "seagreen4",
                           "Pe2Se Fossil CC" = "grey20",
                           "industry energy CC" = "red",
                           "industry process CC" = "lightyellow",
                           "DAC" = "cyan",
                           "Underground Storage" = "sandybrown",
                           "Synthetic Liquids" = "mediumpurple4",
                           "Synthetic Gases" = "plum2")

vars.order <- c("Other Biomass w/ CC",
                "CC from Biomass to H2 w/ CC",
                "CC from Biomass to Liquids w/ CC",
                "Pe2Se Fossil CC",
                "industry energy CC",
                "industry process CC",
                "DAC",
                "Underground Storage",
                "Synthetic Liquids",
                "Synthetic Gases")


df.co2.sink.source <- df %>% 
  filter(variable %in% co2sup.focus.vars,
         scenario %in% scens_short) %>% 
  # change sign of DAC variable
  mutate( value = ifelse(as.character(variable) == "Emi|CO2|DAC", 
                         -value, value)) %>% 
  # bind demand variables with negative sign
  rbind( df %>% 
           filter(variable %in% co2dem.focus.vars,
                  scenario %in% scens_short) %>%
           # change sign an remove "SE|Electricity|used for"
           mutate( value = - value)) %>% 
  revalue.levels(variable = co2.sink.source.label) %>% 
  order.levels(variable = vars.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)


for(reg in regs){
p.co2.sink.source <- ggplot() +
  geom_col(data=df.co2.sink.source%>% filter(region == reg), 
           aes(period, value, fill=variable), 
           alpha=.6, width = 3) +
  facet_wrap(~scenario, ncol=length(scens_short)) +
  scale_y_continuous("Captured CO2 (Mt CO2/yr)") +
  scale_fill_manual(values = co2.sink.source.color) +
  theme_bw() +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"))


# p.co2.sink.source


Check.Balance <- df.co2.sink.source %>% 
  filter(period == 2050, scenario == "Balanced")

ggsave(plot = p.co2.sink.source,
       filename =  paste0(plot.dir, "/", run_number,"_",reg,"_CO2_SinkSource.png"),
       width=25, height=10, units="cm")
}

#H2 supply and demand plots

### plot with positive supply and negative demand bars
h2sup.focus.vars <- c("SE|Hydrogen|Biomass|w/ CCS",
                      "SE|Hydrogen|Biomass|w/o CCS",
                      "SE|Hydrogen|Coal",
                      "SE|Hydrogen|Gas",
                      "SE|Hydrogen|Electricity|Standard Electrolysis",
                      "SE|Hydrogen|Electricity|VRE Storage Electrolysis",
                      "SE|Hydrogen|Net Imports")

h2dem.focus.vars <- c("SE|Hydrogen|used for electricity|normal turbines",
                      "SE|Hydrogen|used for electricity|forced VRE turbines",
                      "SE|Hydrogen|used for synthetic fuels|liquids",
                      "SE|Hydrogen|used for synthetic fuels|gases",
                      "FE|Industry|+|Hydrogen",
                      "FE|Buildings|+|Hydrogen",
                      "FE|Transport|+|Hydrogen",
                      "FE|CDR|+|Hydrogen")


h2.sink.source.label <- c(
  "SE|Hydrogen|Biomass|w/ CCS" = "Biomass w/ CC",
  "SE|Hydrogen|Biomass|w/o CCS" = "Biomass w/o CC",
  "SE|Hydrogen|Coal" = "Coal",
  "SE|Hydrogen|Gas" = "Gas",
  "SE|Hydrogen|Electricity|Standard Electrolysis" = "Grey Electrolysis",
  "SE|Hydrogen|Electricity|VRE Storage Electrolysis" = "VRE Storage Electrolysis",
  "SE|Hydrogen|Net Imports" = "Net Imports",
  
  "SE|Hydrogen|used for electricity|normal turbines" = "H2 Turbines",
  "SE|Hydrogen|used for electricity|forced VRE turbines" = "H2 forced VRE Turbines",
  "SE|Hydrogen|used for synthetic fuels|liquids" = "H2 for Synthetic Liquids",
  "SE|Hydrogen|used for synthetic fuels|gases" = "H2 for Synthetic Gases",
  "FE|Industry|+|Hydrogen" = "H2 for Industry",
  "FE|Buildings|+|Hydrogen" = "H2 for Buildings",
  "FE|Transport|+|Hydrogen" = "H2 for Transport",
  "FE|CDR|+|Hydrogen" = "H2 for CDR"
)

h2.sink.source.color <- c(
  "Biomass w/ CC" = "darkgreen",
  "Biomass w/o CC" = "lightgreen",
  "Coal"= "black",
  "Gas"= "grey80",
  "Grey Electrolysis" = "darkred",
  "VRE Storage Electrolysis" = "darkgoldenrod",
  "Net Imports" = "darkslateblue",
  
  "H2 Turbines" = "darkred",
  "H2 forced VRE Turbines" = "coral",
  "H2 for Synthetic Liquids" = "darkblue",
  "H2 for Synthetic Gases" = "lightblue",
  "H2 for Industry" = "steelblue",
  "H2 for Buildings" = "grey80",
  "H2 for Transport" = "green",
  "H2 for CDR" = "cyan"
  
)

vars.order <- c("Coal",
                "Gas",
                "Biomass w/ CC",
                "Biomass w/o CC",
                "Grey Electrolysis",
                "VRE Storage Electrolysis",
                "Net Imports",
                
                "H2 for Synthetic Gases",
                "H2 for Synthetic Liquids",
                "H2 for CDR",
                "H2 for Transport",
                "H2 for Buildings",
                "H2 for Industry",
                "H2 Turbines",
                "H2 forced VRE Turbines"
)

df.h2.sink.source <- df %>% 
  filter(variable %in% h2sup.focus.vars,
         scenario %in% scens_short) %>%   
  # bind demand variables with negative sign
  rbind( df %>% 
           filter(variable %in% h2dem.focus.vars,
                  scenario %in% scens_short) %>%
           # change sign an remove "SE|Electricity|used for"
           mutate( value = - value)) %>% 
  revalue.levels(variable = h2.sink.source.label) %>% 
  order.levels(variable = vars.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)


for(reg in regs){
p.h2.sink.source <- ggplot() +
  geom_col(data=df.h2.sink.source%>% filter(region == reg), 
           aes(period, value, fill=variable), 
           alpha=.6, width = 3) +
  facet_wrap(~scenario, ncol=length(scens_short)) +
  scale_y_continuous("H2 (EJ/yr)") +
  scale_fill_manual(values = h2.sink.source.color) +
  theme_bw() +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=length(scens_short), direction = "horizontal"))


# p.h2.sink.source


ggsave(plot = p.h2.sink.source,
       filename =  paste0(plot.dir, "/", run_number,"_",reg,"_H2_SinkSource.png"),
       width=30, height=10, units="cm")


}

df_test = filter(df, grepl("SE|Electricity|", variable, fixed=TRUE))

# SE Electricity Supply and Demand Mix
### plot with positive supply and negative demand bars
elsup.focus.vars <- c("SE|Electricity|Wind",
                      "SE|Electricity|Solar|PV",
                      "SE|Electricity|Hydrogen",
                      "SE|Electricity|Gas",
                      "SE|Electricity|Other Fossil",
                      "SE|Electricity|Other Renewables",
                      "SE|Electricity|Nuclear",
                      "SE|Electricity|Net Imports")

eldem.focus.vars <- c("SE|Electricity|used for own consumption of energy system",
                      "SE|Electricity|used for CDR",
                      "SE|Electricity|used in Buildings",
                      "SE|Electricity|used in Industry",
                      "SE|Electricity|used in Transport",
                      "SE|Electricity|used for H2|direct FE H2",
                      "SE|Electricity|used for H2|VRE Storage",
                      "SE|Electricity|used for H2|for synthetic fuels|liquids",
                      "SE|Electricity|used for H2|for synthetic fuels|gases")

el.sink.source.label <- c("SE|Electricity|Wind" = "Wind",
                          "SE|Electricity|Solar|PV" = "PV",
                          "SE|Electricity|Hydrogen" = "Hydrogen",
                          "SE|Electricity|Gas" = "Gas",
                          "SE|Electricity|Other Fossil" = "Other Fossil",
                          "SE|Electricity|Other Renewables" = "Other Renewables",
                          "SE|Electricity|Nuclear" = "Nuclear",
                          "SE|Electricity|Net Imports" = "Net Imports",
                          "SE|Electricity|used for own consumption of energy system" = "for own consumption",
                          "SE|Electricity|used for H2|direct FE H2" = "for direct H2",
                          "SE|Electricity|used for H2|VRE Storage" = "for H2 storage",
                          "SE|Electricity|used for CDR" = "for CDR",
                          "SE|Electricity|used in Buildings" = "for Buildings",
                          "SE|Electricity|used in Industry" = "for Industry",
                          "SE|Electricity|used in Transport" = "for Transport",
                          "SE|Electricity|used for H2|for synthetic fuels|liquids" = "for synthetic liquids", 
                          "SE|Electricity|used for H2|for synthetic fuels|gases" = "for synthetic gases")

el.sink.source.color <- c("Wind" = "deepskyblue1",
                          "PV" = "yellow",
                          "Hydrogen" = "cyan",
                          "Gas" = "grey80",
                          "Other Fossil" = "grey20",
                          "Other Renewables" = "darkgreen",
                          "Nuclear" = "darkorchid",
                          "Net Imports" = "darkslateblue",
                          "for own consumption" = "darkgoldenrod",
                          "for direct H2" = "darkcyan",
                          "for H2 storage" = "mediumpurple1",
                          "for CDR" = "lightgreen",
                          "for Buildings" = "red",
                          "for Industry" = "grey50",
                          "for Transport" = "blue",
                          "for synthetic liquids" = "lightyellow3", 
                          "for synthetic gases" = "navajowhite1")

vars.order <- c("PV",
                "Wind",
                "Hydrogen",
                "Nuclear",
                "Other Renewables",
                "Gas",
                "Other Fossil",
                "Net Imports",
                "for synthetic gases",
                "for synthetic liquids",
                "for H2 storage",
                "for direct H2",
                "for CDR",
                "for Buildings",
                "for Industry",
                "for Transport",
                "for own consumption")



df.el.sink.source <- df %>% 
  filter(variable %in% elsup.focus.vars,
         scenario %in% scens_short) %>% 
  # bind demand variables with negative sign
  rbind( df %>% 
           filter(variable %in% eldem.focus.vars,
                  scenario %in% scens_short) %>%
           # change sign an remove "SE|Electricity|used for"
           mutate( value = - value)) %>% 
  revalue.levels(variable = el.sink.source.label) %>% 
  order.levels(variable = vars.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)

for(reg in regs){
scen = "1.5C"

p.el.sink.source <- ggplot() +
  geom_col(data=df.el.sink.source%>% filter(region == reg)
           %>% filter(scenario == scen )
           , 
           aes(period, value, fill=variable), 
           alpha=.6, width = 3) +
  facet_wrap(~scenario, ncol=length(scens_short)) +
  scale_y_continuous("Electricity (EJ/yr)") +
  scale_fill_manual(values = el.sink.source.color) +
  theme_bw() +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"),color = guide_legend(nrow = 4, byrow = TRUE))

ggsave(plot = p.el.sink.source,
       filename =  paste0(plot.dir, "/", run_number,"_",reg, "_", scen, "_El_SinkSource.png"),
       width=12, height=16, units="cm")
}

for(reg in regs){
  scen = "1.5C"
  
  df.el.sink.source2 <-df.el.sink.source%>% mutate(value = value * 277.77)
                              
  p.el.sink.source <- ggplot() +
    geom_col(data=df.el.sink.source%>% filter(region == reg)%>% mutate(value = value * 277.77)
             %>% filter(scenario == scen )
             , 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario, ncol=length(scens_short)) +
    scale_y_continuous("Electricity (TWh)") +
    scale_fill_manual(values = el.sink.source.color) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"),color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.el.sink.source,
         filename =  paste0(plot.dir, "/", run_number,"_",reg, "_", scen, "_El_SinkSource_TWh.png"),
         width=12, height=16, units="cm")
}


df_test = filter(df, grepl("Cap|Electricity|Hydro", variable, fixed=TRUE))

# SE Electricity Supply and Demand Mix
### plot with positive supply and negative demand bars
cap.focus.vars <- c("Cap|Electricity|Wind",
                      "Cap|Electricity|Solar|PV",
                      "Cap|Electricity|Hydrogen",
                      "Cap|Electricity|Gas",
                      "Cap|Electricity|Other Fossil",
                      "Cap|Electricity|Hydro",
                      "Cap|Electricity|Nuclear",
                      "Cap|Electricity|Biomass")

cap.label <- c("Cap|Electricity|Wind" = "Wind",
                          "Cap|Electricity|Solar|PV" = "PV",
                          "Cap|Electricity|Hydrogen" = "Hydrogen",
                          "Cap|Electricity|Gas" = "Gas",
                          "Cap|Electricity|Other Fossil" = "Other Fossil",
                          "Cap|Electricity|Hydro" = "Hydro",
                          "Cap|Electricity|Nuclear" = "Nuclear",
                          "Cap|Electricity|Biomass" = "Biomass")

cap.color <- c("Wind" = "deepskyblue1",
                          "PV" = "yellow",
                          "Hydrogen" = "cyan",
                          "Gas" = "grey80",
                          "Other Fossil" = "grey20",
                          "Biomass" = "darkgreen",
                          "Nuclear" = "darkorchid",
                          "Hydro" = "darkslateblue")

vars.order <- c("PV",
                "Wind",
                "Hydrogen",
                "Nuclear",
                "Hydro",
                "Gas",
                "Other Fossil",
                "Biomass")

df.cap <- df %>% 
  filter(variable %in% cap.focus.vars,
         scenario %in% scens_short) %>% 
  # bind demand variables with negative sign
  rbind( df %>% 
           filter(variable %in% eldem.focus.vars,
                  scenario %in% scens_short) %>%
           # change sign an remove "SE|Electricity|used for"
           mutate( value = - value)) %>% 
  revalue.levels(variable = cap.label) %>% 
  order.levels(variable = vars.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)

for(reg in regs){
  scen = "1.5C"
  
  df.cap2 <- df.cap %>%
    filter(region == "CHA")  %>% 
    filter(scenario == scen) %>% 
    filter(variable == "Hydro")
             
  p.cap <- ggplot() +
    geom_col(data=df.cap%>% filter(region == reg)
             %>% filter(scenario == scen )
             , 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario, ncol=length(scens_short)) +
    scale_y_continuous("Capacity (GW)") +
    scale_fill_manual(values = cap.color) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"),color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.cap,
         filename =  paste0(plot.dir, "/", run_number,"_",reg, "_", scen, "_cap.png"),
         width=16, height=12, units="cm")
}


df_test = filter(df, grepl("Cap|Electricity|Hydro", variable, fixed=TRUE))

# SE Electricity Supply and Demand Mix
### plot with positive supply and negative demand bars
cap.focus.vars <- c("Cap|Electricity|Wind",
                    "Cap|Electricity|Solar|PV",
                    "Cap|Electricity|Hydrogen",
                    "Cap|Electricity|Hydro",
                    "Cap|Electricity|Nuclear",
                    "Cap|Electricity|Biomass")

cap.label <- c("Cap|Electricity|Wind" = "Wind",
               "Cap|Electricity|Solar|PV" = "PV",
               "Cap|Electricity|Hydrogen" = "Hydrogen",
               "Cap|Electricity|Hydro" = "Hydro",
               "Cap|Electricity|Nuclear" = "Nuclear",
               "Cap|Electricity|Biomass" = "Biomass")

cap.color <- c("Wind" = "deepskyblue1",
               "PV" = "yellow",
               "Hydrogen" = "cyan",
               "Biomass" = "darkgreen",
               "Nuclear" = "darkorchid",
               "Hydro" = "darkslateblue")

vars.order <- c("Biomass",
                "PV",
                "Wind",
                "Hydrogen",
                "Hydro",
                "Nuclear")

df.cap <- df %>% 
  filter(variable %in% cap.focus.vars,
         scenario %in% scens_short) %>% 
  # bind demand variables with negative sign
  rbind( df %>% 
           filter(variable %in% eldem.focus.vars,
                  scenario %in% scens_short) %>%
           # change sign an remove "SE|Electricity|used for"
           mutate( value = - value)) %>% 
  revalue.levels(variable = cap.label) %>% 
  order.levels(variable = vars.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)

for(reg in regs){
  scen = "WB2C netzero DirEl"
  
  df.cap2 <- df.cap %>%
    filter(region == "CHA")  %>% 
    filter(scenario == scen) %>% 
    filter(variable == "Hydro")
  
  p.cap <- ggplot() +
    geom_col(data=df.cap%>% filter(region == reg)
             %>% filter(scenario == scen )
             , 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario, ncol=length(scens_short)) +
    scale_y_continuous("Capacity (GW)") +
    scale_fill_manual(values = cap.color) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"),color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.cap,
         filename =  paste0(plot.dir, "/", run_number,"_",reg, "_", scen, "_cap_renew.png"),
         width=16, height=12, units="cm")
}

