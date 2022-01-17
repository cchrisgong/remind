
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

###################### Plot styles######################################
options(warn=-1)
tt = plotstyle.add( entity = "non-LDV"  ,legend = "public", c("#999999"), replace = T)
tt = plotstyle.add( entity = "SE|Liquids|Oil"  ,legend = "Oil", c("#222222"), replace = T)
tt = plotstyle.add( entity = "Oil"  ,legend = "Oil", c("#884422"), replace = T)
tt = plotstyle.add( entity = "Solids|Modern"  ,legend = "Solids", c("#008800"), replace = T)
tt = plotstyle.add("1.5C-Elec", "1.5C-Elec", "#009E73", linestyle = "solid", marker = 19, replace = T)
tt = plotstyle.add("WB2C-Elec", "WB2C-Elec", "#882288", linestyle = "solid", marker = 19,replace = T)
tt = plotstyle.add("1.5C-Conv", "1.5C-Conv", "#009E73", linestyle = "dotted", marker = 1, replace = T)
tt = plotstyle.add("WB2C-Conv", "WB2C-Conv", "#882288", linestyle = "dotted", marker = 1,  replace = T)
tt = plotstyle.add(c("Reference") ,c("Reference"),  c( "#cc0000"),linestyle = "dotted", marker = 1, replace = T)
tt = plotstyle.add("SR15-1.5C", "SR15-1.5C", "#009E73", replace = T)
tt = plotstyle.add("SR15-WB2C", "SR15-WB2C", "#882288", replace = T)

# demand
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|CDR")   ,c("CDR"),  c("#00cc99"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Industry")   ,c("Industry"),  c("#7777ff"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Transport")   ,c("Transport"),  c("#440077"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Buildings")   ,c("Buildings"),  c("#4444bb"), replace = T)
# supply
tt = plotstyle.add(c("Emi|CO2|Energy|Supply|Electricity w/ couple prod")   ,c("Elec. Supply"),  c("#ffaa00"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Gross|Energy|Supply|Electricity")   ,c("Elec. Supply"),  c("#ffaa00"),replace=T)

tt = plotstyle.add(c("Emi|CO2|Energy|Supply|Non-electric")   ,c("Non Elec. Supply"),  c("#aa0000"),replace=T)

#misc
tt = plotstyle.add(c("Emi|CO2|non-BECCS CDR")   ,c("non BECCS CDR"),  c("#116611"),replace=T)

# tt = plotstyle.add(c("Emi|CO2|CDR|BECCS")   ,c("BECCS"),  c("#FF337F"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Land-Use Change")   ,c("Land-use change"),  c("#919100"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Industrial Processes")   ,c("Industrial Processes"),  c("#10bfcc"),replace=T)


tt = plotstyle.add(c("FE|Transport|Electricity")   ,"Transport|Elec" ,  c("#ffb200"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Electricity")   ,"Buildings|Elec",  c("#ffcc55"),replace=T)
tt = plotstyle.add(c("FE|Industry|Electricity")    ,"Industry|Elec",  c("#fa8c02"),replace=T)

tt = plotstyle.add(c("FE|Transport|Hydrogen")   ,"Transport|Hydrogen" ,  c("#46fc02"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Hydrogen")   ,"Buildings|Hydrogen",  c("#94ff6c"),replace=T)
tt = plotstyle.add(c("FE|Industry|Hydrogen")    ,"Industry|Hydrogen",  c("#2b7c0d"),replace=T)

tt = plotstyle.add(c("FE|Buildings|Heat")   ,"Buildings|Heat",  c("#ff7a59"),replace=T)
tt = plotstyle.add(c("FE|Industry|Heat")    ,"Industry|Heat",  c("#b92400"),replace=T)

tt = plotstyle.add(c("FE|Transport|Gases")   ,"Transport|Gases" ,  c("#b200ff"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Gases")   ,"Buildings|Gases",  c("#c875ec"),replace=T)
tt = plotstyle.add(c("FE|Industry|Gases")    ,"Industry|Gases",  c("#610d85"),replace=T)

tt = plotstyle.add(c("FE|Transport|Liquids")   ,"Transport|Liquids" ,  c("#0f30ec"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Liquids")   ,"Buildings|Liquids",  c("#7e8fef"),replace=T)
tt = plotstyle.add(c("FE|Industry|Liquids")    ,"Industry|Liquids",  c("#04167c"),replace=T)

tt = plotstyle.add(c("FE|Buildings|Solids")   ,"Buildings|Solids", c("#a5a5a5"),replace=T)
tt = plotstyle.add(c("FE|Industry|Solids")    ,"Industry|Solids",   c("#1b1b1b"),replace=T)

tt = plotstyle.add(c("Electricity")    ,"Electricity",   c("#990000"),replace=T)
tt = plotstyle.add(c("Heat")    ,"Heat",   c("#dd7700"),replace=T)

tt = plotstyle.add(c("SE|Hydrogen|Electricity")    ,"H2|Electrolysis",   c("#ffdd44"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Biomass")    ,"H2|Bio",   c("#33AACC"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Fossil")    ,"H2|Fossil",   c("#1133EE"),replace=T)
tt = plotstyle.add(c("SE|Heat|Geothermal")    ,"Heat|Heat Pump",   c("#e51900"),replace=T)
tt = plotstyle.add(c("SE|Heat|Biomass")    ,"Heat|Bio",   c("#cc5555"),replace=T)
tt = plotstyle.add(c("SE|Heat|Fossil")    ,"Heat|Fossil",   c("#cc0000"),replace=T)


tt = plotstyle.add(c("SE|Heat|Biomass")    ,"Heat|Bio",   c("#66cc11"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Biomass")    ,"H2|Bio",   c("#55EEEE"),replace=T)
tt = plotstyle.add(c("SE|Gases|Biomass")    ,"Gases|Bio",   c("#33ff00"),replace=T)
tt = plotstyle.add(c("SE|Liquids|Biomass")    ,"Liquids|Bio",   c("#11AA22"),replace=T)
tt = plotstyle.add(c("SE|Solids|Biomass")    ,"Solids|Bio",   c("#005900"),replace=T)


tt = plotstyle.add(c("SE|Heat|Fossil")    ,"Heat|Fossil",   c("#996666"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Fossil")    ,"H2|Fossil",   c("#666699"),replace=T)
tt = plotstyle.add(c("SE|Gases|Fossil")    ,"Gases|Fossil",   c("#444444"),replace=T)
tt = plotstyle.add(c("SE|Liquids|Fossil")    ,"Liquids|Fossil",   c("#553022"),replace=T)
tt = plotstyle.add(c("SE|Solids|Coal")    ,"Solids|Coal",   c("#0c0c0c"),replace=T)



options(warn=0)
###################### Scenario names #####################################


setwd('~/source/REMIND_integrate/remind/output')
run_number = "china_main1"

data.dir = paste0("./",run_number)

plots.path = paste0("./plots/", run_number,"/")
dir.create(plots.path, showWarnings = FALSE)

# regs = c("World", "CHA")
regs_elecshare = c("World","CHA","EUR")
regs = c("World","CHA","CAZ","EUR","IND","LAM","MEA","NEU","REF","SSA","JPN","USA","OAS")
t.barplot <- c(2015, 2020, 2030, 2040, 2050, 2060)

scenarios = c(
  "REMIND_generic_Base_INT",
  "REMIND_generic_Trend_INT",
  "REMIND_generic_Ref-gd40_INT",
  "REMIND_generic_Bal_INT",
  "REMIND_generic_Elec_dom_INT",
  "REMIND_generic_H2_dom_INT",
  NULL)

scens_short =
  c(
    "Baseline",
    "Trend",
    "Ref",
    "Bal",
    "Elec",
    "H2",
    NULL)

default_scen = "Bal"

data.files = lapply(scenarios, function(x) paste0( data.dir, "/", x, "_withoutPlus.mif")) %>% unlist

run_indices = seq(from = 1, to = length(data.files), by = 1)
df.raw = NULL
# trim df to chosen scenarios and regions
for (i in run_indices){
  print(i)
  # REMIND.data = read.quitte(data.files[[9]])
  REMIND.data = read.quitte(data.files[[i]])
  
  df.raw <- rbind(df.raw, REMIND.data)
}

scens0 = stringr::str_replace(scenarios, "_withoutPlus", "")
scens = stringr::str_replace(scens0, "REMIND_generic_", "")

df.allregs =  filter(df.raw, scenario %in% scens, region %in% regs ) %>%
  order.levels(scenario = scens) %>%
  factor.data.frame()

rm(df.raw)

###################### Historical mif ######################################

tmax = 2060

# also read in historical data

df.hist0 = read.quitte(paste0("./",run_number, "/historical.mif"))

df.hist = filter(df.hist0, model =="CEDS", region %in% regs ) %>%
  factor.data.frame() 

df.hist_FE = filter(df.hist0, model =="IEA", region %in% regs ) %>%
  factor.data.frame() 
# %>% 
# filter(period >2000 ) %>% 
# filter(variable == "Emi|CO2|Energy" ) 

# df.hist = df.hist1 %>% 
#   calc_addVariable("`Emi|CO2|Total`" = "`Emi|CO2|Energy` + `Emi|CO2|Land Use|Grassland Burning` +       `Emi|CO2|Land Use|Forest Burning`+ `Emi|CO2|Land Use|Agriculture and Biomass Burning`",
#                    units = "Gt CO2/yr") %>% 
#   factor.data.frame()

rm(df.hist0)
###################### Rescale price ######################


# rescale price and cost data to 2020 using 2005->2020 deflator of 1.34 (https://stats.oecd.org/Index.aspx?DataSetCode=PRICES_CPI)

indices = grepl("US\\$2005",df.allregs$unit)
df.allregs[indices, "value"] = df.allregs[indices, "value"] * 1.2
levels(df.allregs$unit) = gsub("US\\$2005", "US\\$2015", levels(df.allregs$unit))

df.allregs$scenario <- plyr::mapvalues(df.allregs$scenario, from = scens, to = scens_short)

df <- df.allregs%>%
  # remove "|+|" elements in variable names
  mutate (variable = stringr::str_replace(variable, "\\+\\|", "")) %>% 
  mutate (tech = case_when(grepl("Elec", scenario) ~ "Elec", 
                           TRUE ~ "Def"))
# factor.data.frame()


###################### Color ######################################
# reg = "World"
# limy = c(-10,45)
reg = "CHA"
limy = c(-2,13)

# scens = scens_short[c(5)]
scens = scens_short

cbbPalette <- c( "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#F0E442","#E69F00", "#CC79A7", "#FF9999", "#9966FF", "#999900")
# mycolors = c(scens[[1]] = cbbPalette[[1]])
library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(7)[c(1,3,4)]
######################################################################################################
######################################################################################################

###################### emission plot for Gunnar ##############################
vars = c(     "Emi|CO2|Gross|Energy|Supply|Electricity",
              "Emi|CO2|Gross|Energy|Supply|Non-electric",

              "Emi|CO2|Energy|Demand|Buildings",
              "Emi|CO2|Energy|Demand|Transport",
              "Emi|CO2|Industrial Processes",
              "Emi|CO2|Energy|Demand|Industry",
              "Emi|CO2|Land-Use Change",
              "Emi|CO2|CDR|BECCS",
              "Emi|CO2|CDR|DACCS",
              "Emi|CO2|CDR|EW")

# define colors and labels
tt = plotstyle.add(c("Emi|CO2|CDR|EW")   ,c("EW"),  c("sandybrown"), replace = T)
tt = plotstyle.add(c("Emi|CO2|CDR|BECCS")   ,c("BECCS"),  c("lightgreen"), replace = T)
tt = plotstyle.add(c("Emi|CO2|CDR|DACCS")   ,c("DAC"),  c("cyan"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Industry")   ,c("Industry"),  c( "grey"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Transport")   ,c("Transport"),  c("darkblue"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Buildings")   ,c("Buildings"),  c( "darkred"), replace = T)
tt = plotstyle.add(c("Emi|CO2|+|Land-Use Change")   ,c("Land-Use Change"),  c("darkgreen"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Gross|Energy|Supply|Electricity")   ,c("Elec. Supply"),  c("darkgoldenrod"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Gross|Energy|Supply|Non-electric")   ,c("Non-elec. Supply"),  c("darkorchid"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Industrial Processes"),c("Industrial Processes"),  c("palevioletred"),replace=T)

plot.period <- seq(2015,2060,5)

df.emi.grossneg <- df %>%
  filter(variable %in% vars, period %in% plot.period, region == "CHA") %>%
  order.levels(variable = vars)

df.emi.tot <- df %>%
  filter(variable == "Emi|CO2", period %in% plot.period, region == "CHA") %>%
  mutate(variable = "Total")

p.emi.grossneg <- ggplot() +
  geom_col(data=df.emi.grossneg, aes(period, value, fill=variable), alpha=0.8) +
  geom_line(data=df.emi.tot, aes(period, value, linetype=variable), size=1.2, alpha=0.7) +
  geom_point(data=df.emi.tot, aes(period, value, shape=variable), size=1.5, alpha=0.7) +
  facet_wrap(~scenario, ncol = length(getScenarios(df))) +
  scale_y_continuous("CO2 Emissions (Mt CO2/yr)") +
  scale_fill_manual(values = mip::plotstyle(vars), labels = mip::plotstyle(vars, out = "legend")) +
  geom_hline(yintercept=0, size=0.8, linetype="dashed", alpha=0.7) +

  theme_bw() +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5),
         legend.position = "bottom",strip.background = element_blank(),
         legend.title = element_blank(),strip.text.y = element_text(angle = 0, hjust = 0))
# show historical data for validation
  df.emi.tot.hist <- df.hist %>%
    filter(variable == "Emi|CO2", period >= 1990, period <= max(plot.period), model == "CEDS", region == "CHA") %>%
    select(-scenario) %>%
    mutate(model = "historical")

  p.emi.grossneg <- p.emi.grossneg +
    geom_line(data=df.emi.tot.hist, aes(period, value, color=model), size=1.2, alpha=0.7)

ggsave(plot = p.emi.grossneg, filename = paste0(plots.path,  "/", run_number, "Emi_CO2GrossNeg.png"), width=20, height=10, units = "cm")

###################### electricity emission intensity ######################################
# emission intensity of electricity

df.plot = df %>%
  calc_addVariable("`Carbon Intensity|Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`SE|Electricity`)*3.6",
                   units  = "tCO2/MWh", only.new = T)  %>%
  mutate (value = pmax(0, value)) %>%
  filter( period <= tmax)

g1 <- mipLineHistorical(df.plot, ylab = "Carbon Intensity Electricity\n[kgCO2/MWh]", size = 10) +
  xlim(c(2015,2060))+
  theme_bw()
ggsave(filename = paste0(plots.path, "/", run_number, "_CI_Elec_lineplot_regs.png"), width=20, height=14, units = "cm")

g1 <- mipLineHistorical(df.plot %>%  filter(region ==reg), ylab = "Carbon Intensity Electricity\n[kgCO2/MWh]", size = 10) +
  xlim(c(2015,2060))+
  theme_bw()
ggsave(filename = paste0(plots.path, "/", run_number, "_CI_Elec_lineplot", reg, ".png"), width=20, height=14, units = "cm")



df.plot = df %>%
  calc_addVariable("`Carbon Intensity|Non-Elec`" = "(`Emi|CO2|Energy|Demand|Industry` +  `Emi|CO2|Energy|Demand|Buildings` +  `Emi|CO2|Energy|Demand|Transport`)  / (`SE|Solids` + `SE|Liquids` + `SE|Gases`)*3.6",
                   units  = "tCO2/MWh", only.new = T)  %>%
  mutate (value = pmax(0, value)) %>%
  filter( period <= tmax)

mipLineHistorical(df.plot, ylab = "Carbon Intensity Fuels\n[kgCO2/MWh]", size = 10) +
  xlim(c(2015,2060))+
  theme_bw()

ggsave(filename = paste0(plots.path, "/", run_number, "_CI_nonElec_lineplot_regs.png"), width=20, height=14, units = "cm")

g1 <- mipLineHistorical(df.plot %>% filter(region ==reg), ylab = "Carbon Intensity Electricity\n[kgCO2/MWh]", size = 10) +
  xlim(c(2015,2060))+
  theme_bw()

ggsave(filename = paste0(plots.path, "/", run_number, "_CI_nonElec_lineplot_", reg, ".png"), width=20, height=14, units = "cm")


###################### Kaya ################################
kaya_var = c("Population", "GDP|per capita|MER", "Intensity|Final Energy|CO2", "Intensity|GDP|Final Energy")
df.allregs$scenario <- plyr::mapvalues(df.allregs$scenario, from = scens, to = scens_short)

kaya.mapping <- c(coalchp = "Coal (Lig + HC)",
                           igcc = "Coal (Lig + HC)",
                           igccc = "Coal (Lig + HC)",
                           pcc = "Coal (Lig + HC)",
                           pco = "Coal (Lig + HC)",
                           pc = "Coal (Lig + HC)",
                           tnrs = "Nuclear",
                           ngt = "OCGT",
                           ngcc = "CCGT",
                           ngccc = "CCGT",
                           gaschp = "CCGT",
                           biochp = "Biomass",
                           bioigcc = "Biomass",
                           bioigccc = "Biomass",
                           NULL)

# kaya 1: GDP, Energy intensity, carbon intensity
df.plot0 = df %>%
  filter(scenario == default_scen) %>% 
  filter(variable %in% c("GDP|MER", "Intensity|Final Energy|CO2", "Intensity|GDP|Final Energy")) %>% 
  # mutate(value = pmax(0, value)) %>%
  filter(period <= tmax)

df.plot_2000 = df %>%
  filter(scenario == default_scen) %>% 
  filter(variable %in% c("GDP|MER", "Intensity|Final Energy|CO2", "Intensity|GDP|Final Energy")) %>% 
  # mutate(value = pmax(0, value)) %>%
  filter(period == "2005") %>% 
  select(model,scenario,region,variable,histref = value)

df.plot = list(df.plot0,df.plot_2000) %>%
  reduce(full_join) %>% 
  mutate(value = value/histref)
  
g1 <- ggplot() + geom_line(data= df.plot, aes(x=period, y = value, color = variable)) +
  ggtitle(default_scen)+
  # xlim(c(2005,2060))+
  scale_color_manual(name = "Kaya factors", values = cbbPalette)+
  xlab("") + ylab("changes relative to 2005") +
  # scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) + 
  scale_x_discrete(limits=c(2005,2020,2030,2040,2050,2060)) + 
  facet_wrap(~region, nrow = 4, scales = 'free_y')+
  theme_bw()

ggsave(filename = paste0(plots.path, "/", run_number, "_Kaya3factors_regs.png"), width=20, height=14, units = "cm")

g1 <- ggplot() + geom_line(data= df.plot%>%  filter(region ==reg), aes(x=period, y = value, color = variable)) +
  ggtitle(default_scen)+
  # xlim(c(2005,2060))+
  scale_color_manual(name = "Kaya factors", values = cbbPalette)+
  xlab("") + ylab("changes relative to 2005") +
  scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) + 
  scale_x_discrete(limits=c(2005,2020,2030,2040,2050,2060)) + 
  facet_wrap(~region, nrow = 4, scales = 'free_y')+
  theme_bw()

ggsave(filename = paste0(plots.path, "/", run_number, "_Kaya3factors_", reg, ".png"), width=20, height=14, units = "cm")

###################### Kaya 2######################################
# kaya 2: pop, GDP/capita, Energy intensity, carbon intensity
df.plot0 = df %>%
  filter(scenario == default_scen) %>% 
  filter(variable %in% c("Population", "GDP|per capita|MER", "Intensity|Final Energy|CO2", "Intensity|GDP|Final Energy")) %>% 
  # mutate(value = pmax(0, value)) %>%
  filter(period <= tmax)

df.plot_2000 = df %>%
  filter(scenario == default_scen) %>% 
  filter(variable %in% c("Population", "GDP|per capita|MER", "Intensity|Final Energy|CO2", "Intensity|GDP|Final Energy")) %>% 
  # mutate(value = pmax(0, value)) %>%
  filter(period == "2005") %>% 
  select(model,scenario,region,variable,histref = value)

df.plot = list(df.plot0,df.plot_2000) %>%
  reduce(full_join) %>% 
  mutate(value = value/histref)

df.plot_reg <- df.plot  %>%
  filter(region ==reg)

g1 <- ggplot() + geom_line(data= df.plot, aes(x=period, y = value, color = variable)) +
  ggtitle(default_scen)+
  xlim(c(2005,2060))+
  scale_color_manual(name = "Kaya factors", values = cbbPalette)+
  xlab("") + ylab("changes relative to 2005") +
  # scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) + 
  scale_x_discrete(limits=c(2005,2020,2030,2040,2050,2060)) + 
  facet_wrap(~region, nrow = 4, scales = 'free_y')+
  theme_bw()

ggsave(filename = paste0(plots.path, "/", run_number, "_Kaya4factors_regs.png"), width=20, height=14, units = "cm")

g1 <- ggplot() + geom_line(data= df.plot%>%  filter(region ==reg), aes(x=period, y = value, color = variable)) +
  ggtitle("1.5C")+
  xlim(c(2005,2060))+
  scale_color_manual(name = "Kaya factors", values = cbbPalette)+
  xlab("") + ylab("changes relative to 2005") +
  scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9")) + 
  scale_x_discrete(limits=c(2005,2020,2030,2040,2050,2060)) + 
  facet_wrap(~region, nrow = 4, scales = 'free_y')+
  theme_bw()

ggsave(filename = paste0(plots.path, "/", run_number, "_Kaya4factors_", reg, ".png"), width=20, height=14, units = "cm")

# 
# stop()



###################### Emi CO2 sector ######################################
# mycolors <- c( = gg_color_hue(6))

  vars = c(
  # energy|supply:
    "Emi|CO2|Energy|Supply|Non-electric",
    "Emi|CO2|Energy|Supply|Electricity w/ couple prod" ,
    # energy|demand: 
    "Emi|CO2|Energy|Demand|Industry" ,
    "Emi|CO2|Energy|Demand|Buildings",
    "Emi|CO2|Energy|Demand|Transport",
    "Emi|CO2|Energy|Demand|CDR",
    
    "Emi|CO2|Industrial Processes",
    "Emi|CO2|Land-Use Change",
    "Emi|CO2|non-BECCS CDR"
    # ,
    # "Emi|CO2|CDR|BECCS"
    )

vars = c(     "Emi|CO2|Gross|Energy|Supply|Electricity",
              "Emi|CO2|Gross|Energy|Supply|Non-electric",
              
              "Emi|CO2|Energy|Demand|Buildings",
              "Emi|CO2|Energy|Demand|Transport",
              "Emi|CO2|Industrial Processes",
              "Emi|CO2|Energy|Demand|Industry",
              "Emi|CO2|Land-Use Change",
              "Emi|CO2|CDR|BECCS",
              "Emi|CO2|CDR|DACCS",
              "Emi|CO2|CDR|EW")

df.tot.hist = filter(df.hist, variable == "Emi|CO2",model == "CEDS", region == reg) %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.tot.hist.regcheck = filter(df.hist, variable == "Emi|CO2",model == "CEDS", region == "CHA") %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.tot.hist.regs = filter(df.hist, variable == "Emi|CO2",model == "CEDS") %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.tot.histcheck = filter(df.hist, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, model == "CEDS", region == reg) %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.plot = filter(df, variable %in% vars,scenario %in% scens_short) %>%
    order.levels(scenario = scens_short, variable = vars) %>%
  filter(period >= 2010, period <= 2080) %>%
  mutate(value = value/1000) %>%
  factor.data.frame()

df.sector_emi_check <- df.plot %>% 
  filter(scenario == default_scen,region == "CHA")
  
  
df.BECCS.check = filter(df.plot, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, period < 2055, scenario == "1.5C netzero BECCS", region == reg) %>% 
  dplyr::summarise( value = sum(value) )

df.1.5.check = filter(df.plot, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, period < 2065, scenario == default_scen, region == reg)  

df.2.check = filter(df.plot, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, period < 2055, scenario == "2C netzero bal", region == reg)  

df.tot =  filter(df, variable ==  "Emi|CO2", period <= 2080, scenario %in% scens_short) %>%
  mutate(value = value/1000) %>%
  factor.data.frame()

df.tot.plot =  filter(df, variable ==  "Emi|CO2", period <= 2080, scenario %in% scens_short) %>%
  mutate(value = value/1000) %>%
  factor.data.frame()

# peak budget
# between years
df.tot_sum1 <- df.tot %>% 
  filter(period > 2020, period < 2060) %>%
  select(scenario, value, period, region) %>% 
  filter(region == "CHA") %>% 
  dplyr::group_by(scenario, region) %>%
  dplyr::summarise( value = sum(value) *5)%>% 
  dplyr::ungroup()

# end year
df.tot_sum2 <- df.tot %>% 
  filter(period %in% c(2060)) %>% 
  select(scenario, value, period, region) %>% 
  filter(region == "CHA") %>% 
  mutate(value2 = value * 7.5)%>% 
  select(scenario, value2, region)

# start year
df.tot_sum3 <- df.tot %>% 
  filter(period %in% c(2020)) %>% 
  select(scenario, value, period, region) %>% 
  filter(region == "CHA") %>% 
  mutate(value3 = value * 2.5)%>% 
  select(scenario, value3, region)

df.tot_sum = list(df.tot_sum1, df.tot_sum2,df.tot_sum3) %>%
  reduce(full_join) %>% 
  mutate(budget = value + value2 +value3)

df.plot2 = df.plot %>% filter(region == reg, scenario == default_scen, variable == "Emi|CO2|Energy|Demand|CDR")

for (scen in scens){

mip::mipArea(df.plot %>% filter(region == reg, scenario == scen), total = df.tot.plot %>% filter(region == reg, scenario == scen)) +
  scale_fill_manual(name = "",values = as.character(plotstyle(vars)), labels = as.character(plotstyle(vars, out = "legend"))) +
  ylab("CO2 Emissions [GtCO2/yr]") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(2010,2060), ylim = limy)+
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size = 1.5 )+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold")) 

ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", scen, "_", "EmiCO2.png"),width=14, height=10, units = "cm")
}

for (scen in scens){
mip::mipArea(df.plot %>% filter(scenario == scen), total = df.tot.plot %>% filter(scenario == scen)) +
  scale_fill_manual(values = as.character(plotstyle(vars)), labels = as.character(plotstyle(vars, out = "legend"))) +
  ylab("CO2 Emissions [GtCO2/yr]") +
  theme_bw() +
  ggtitle(paste0(scen))+
  theme(strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(2010,2060))+
  geom_line(data= df.tot.hist.regs, aes(x=period, y = value), color = "black", size = 1.5 )+
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold")) +
  facet_wrap(~region, nrow = 4, scales = 'free_y')

ggsave(filename = paste0(plots.path,"/", run_number, "_regs_", scen, "_", "EmiCO2.png"),width=23, height=12, units = "cm")

}



df.linecomparison = df.tot.plot %>% filter(period <= 2080, scenario %in% scens_short, region == reg)

levels(df.linecomparison$scenario) <- scens_short

p<-ggplot() + geom_line(data= df.linecomparison, aes(x=period, y = value, color = scenario)) +
  ylab("CO2 Emissions  [GtCO2/yr]") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(2010,2080),ylim = limy)+
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =1 ) +
  scale_color_manual(values = cbbPalette) +
  labs(linetype = "Peaking Time")

ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", "EmiCO2_lines.png"),width=18, height=10, units = "cm")

if (reg == "CHA"){
  df.tsinhua_co2 <- data.frame(period=seq(from = 2020, to = 2070, by = 5), 
                   value=c(9.7,10.2,10.0,8.2,6.2,4.3,2.6,1.2,0,0,0), scenario = "2060CN") 
  
  df.linecomparison2 = list(df.linecomparison, df.tsinhua_co2) %>%
    reduce(full_join) 
  
  levels(df.linecomparison2$scenario) <- c(scens_short, "2060CN")

  p<-ggplot() + geom_line(data= df.linecomparison2, aes(x=period, y = value, color = scenario)) +
  ylab("CO2 Emissions [GtCO2/yr]") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(2010,2080),ylim = limy)+
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =1 )+
    scale_color_manual(values = cbbPalette)+
    labs(linetype = "peaking Time")

ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", "EmiCO2_lines_TsingHua.png"),width=18, height=10, units = "cm")
}


#####################################################################################################
#####################################################################################################
#####################################################################################################

###################### FE carrier #################################
vars = c(
  "FE|Electricity",                                                              
  "FE|Hydrogen",
  "FE|Heat",
  "FE|Gases",                                                                    
  "FE|Liquids",     
  "FE|Solids",     
  NULL)

df.plot = filter(df, variable %in% vars, period <= tmax, region == reg, scenario %in% scens_short ) %>%
  order.levels(scenario = scens_short, variable = vars) %>%
  mutate(value = value) %>%
  factor.data.frame()

df.tot =  filter(df, variable ==  "FE", period <= tmax, region == reg, scenario %in% scens_short ) %>%
  mutate(value = value) %>%
  factor.data.frame()

df.tot.hist = filter(df.hist, variable == "FE", period >= 1990,period <= 2012,!is.na(value), model == "IEA", region == reg) %>%
  select(-scenario)

mip::mipArea(df.plot %>% filter(period <= 2065), total = df.tot%>% filter(period <= 2065)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "right") +
  # facet_grid(~scenario, nrow = 2) +
  coord_cartesian(xlim = c(2000,2060))+
  ylab("Final Energy  [EJ/yr]") +
  geom_line(data= df.tot.hist, aes(x = period, y = value), color = "black", size =2 )

ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", "FE_energyType.png"),width=18, height=10, units = "cm")

###################### FE sector Tsinghua #############################################
vars = c(
  "FE|CDR",
  "FE|Buildings",
  "FE|Transport",                                                              
  "FE|Industry",
  
  NULL)

df.plot = filter(df, variable %in% vars, period <= tmax , region == reg, scenario %in% scens_short) %>%
  order.levels(scenario = scens_short, variable = vars) %>%
  mutate(value = value) %>%
  select(period,scenario,variable,value) %>% 
  factor.data.frame()

df.tot =  filter(df, variable ==  "FE", period <= tmax, region == reg, scenario %in% scens_short ) %>%
  mutate(value = value) %>%
  factor.data.frame()

df.tot.hist = filter(df.hist, variable == "FE", period >= 1990,period <= 2012,!is.na(value), model == "IEA", region == reg) %>%
  select(-scenario)

# Tsinghua report Figure 2:
df.Tsinghua_2C_i <- NULL
df.Tsinghua_2C_i$period <- c(2020,2025,2030,2035,2040,2045,2050)
df.Tsinghua_2C_i$variable <- "FE|Industry"
df.Tsinghua_2C_i$value<- c(22,25,24,23,22,20,18)*2.9308
df.Tsinghua_2C_i <- as.data.frame(df.Tsinghua_2C_i)

# Tsinghua report Figure 7:
df.Tsinghua_2C_t <- NULL
df.Tsinghua_2C_t$period <- c(2020,2025,2030,2035,2040,2045,2050)
df.Tsinghua_2C_t$variable <- "FE|Transport"
df.Tsinghua_2C_t$value<- c(5,5.2,5.5,5.35,5,4.8,4.5)*2.9308
df.Tsinghua_2C_t <- as.data.frame(df.Tsinghua_2C_t)

# Tsinghua report table 17 (different from Figure 2&7!!):
# df.Tsinghua_2C_i <- NULL
# df.Tsinghua_2C_i$period <- c(2020,2030,2050)
# df.Tsinghua_2C_i$variable <- "FE|Industry"
# df.Tsinghua_2C_i$value<- c(16.1,18.8,6.9)*2.9308
# df.Tsinghua_2C_i <- as.data.frame(df.Tsinghua_2C_i)
# 
df.Tsinghua_2C_b <- NULL
df.Tsinghua_2C_b$period <- c(2020,2030,2050)
df.Tsinghua_2C_b$variable <- "FE|Buildings"
df.Tsinghua_2C_b$value<- c(5.5,5.1,2.6)*2.9308
df.Tsinghua_2C_b <- as.data.frame(df.Tsinghua_2C_b)

# df.Tsinghua_2C_t <- NULL
# df.Tsinghua_2C_t$period <- c(2020,2030,2050)
# df.Tsinghua_2C_t$variable <- "FE|Transport"
# df.Tsinghua_2C_t$value<- c(4.9,5.4,3.0)*2.9308
# df.Tsinghua_2C_t <- as.data.frame(df.Tsinghua_2C_t)


df.Tsinghua_2C_tot = list(df.Tsinghua_2C_i, df.Tsinghua_2C_b,df.Tsinghua_2C_t) %>%
    reduce(full_join) %>%
    filter(period %in% c(2020,2030,2050)) %>% 
    dplyr::group_by(period) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(period) %>% 
    mutate(scenario = "Tsinghua 2C") %>% 
    mutate(variable = "FE")
  
df.Tsinghua_2C = list(df.Tsinghua_2C_i, df.Tsinghua_2C_b,df.Tsinghua_2C_t,df.Tsinghua_2C_tot) %>%
    reduce(full_join) %>%
    mutate(scenario = "Tsinghua 2C")

df.plot2 <- list(df.Tsinghua_2C, df.plot) %>%
  reduce(full_join) %>% 
  filter(period <= 2065) %>% 
  filter(period >=2020)

mip::mipArea(df.plot2 , total = df.tot %>% filter(period <= 2065) %>% filter(period >=2020)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "right") +
  ylab("Final Energy[EJ/yr]") +
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =2 ) +
  facet_wrap(~scenario, nrow = 2)

ggsave(filename = paste0(plots.path,"/", run_number,"_", reg, "_", "FE_sector_Tsinghua.png"),width=18, height=10, units = "cm")


###################### FEEl sector Tsinghua #########################################
vars = c(
  "FE|Buildings|Electricity",
  "FE|Transport|Electricity",                                                              
  "FE|Industry|Electricity",
  NULL)

df.plot = filter(df, variable %in% vars, period <= tmax , region == reg, scenario %in% scens_short) %>%
  order.levels(scenario = scens_short, variable = vars) %>%
  mutate(value = value) %>%
  select(period,scenario,variable,value) %>% 
  factor.data.frame()

df.tot =  filter(df, variable ==  "FE|Electricity", period <= tmax, region == reg, scenario %in% scens_short ) %>%
  mutate(value = value) %>%
  factor.data.frame()

df.tot.hist = filter(df.hist, variable == "FE|Electricity", period >= 1990,period <= 2012,!is.na(value), model == "IEA", region == reg) %>%
  select(-scenario)

# Tsinghua report Table 6:
df.Tsinghua_2C_ie <- NULL
df.Tsinghua_2C_ie$period <- c(2020,2030,2050)
df.Tsinghua_2C_ie$variable <- "FE|Industry|Electricity"
df.Tsinghua_2C_ie$value<- c(4.59,6.06,7.8)*3.6
df.Tsinghua_2C_ie <- as.data.frame(df.Tsinghua_2C_ie)

df.Tsinghua_2C_be <- NULL
df.Tsinghua_2C_be$period <- c(2020,2030,2050)
df.Tsinghua_2C_be$variable <- "FE|Buildings|Electricity"
df.Tsinghua_2C_be$value<- c(1.87,2.51,3.68)*3.6
df.Tsinghua_2C_be <- as.data.frame(df.Tsinghua_2C_be)

df.Tsinghua_2C_te <- NULL
df.Tsinghua_2C_te$period <- c(2020,2030,2050)
df.Tsinghua_2C_te$variable <- "FE|Transport|Electricity"
df.Tsinghua_2C_te$value<- c(0.22,0.42,0.79)*3.6
df.Tsinghua_2C_te <- as.data.frame(df.Tsinghua_2C_te)

df.Tsinghua_2Ce_tot = list(df.Tsinghua_2C_ie, df.Tsinghua_2C_be,df.Tsinghua_2C_te) %>%
  reduce(full_join) %>%
  filter(period %in% c(2020,2030,2050)) %>% 
  dplyr::group_by(period) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period) %>% 
  mutate(scenario = "Tsinghua 2C") %>% 
  mutate(variable = "FE|Electricity")

df.Tsinghua_2Ce = list(df.Tsinghua_2C_ie, df.Tsinghua_2C_be,df.Tsinghua_2C_te,df.Tsinghua_2Ce_tot) %>%
  reduce(full_join) %>%
  mutate(scenario = "Tsinghua 2C")

df.plot2 <- list(df.Tsinghua_2Ce, df.plot) %>%
  reduce(full_join) %>% 
  filter(period <= 2065) %>% 
  filter(period >=2020)

mip::mipArea(df.plot2 , total = df.tot %>% filter(period <= 2065) %>% filter(period >=2020)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "right") +
  ylab("Final Energy[EJ/yr]") +
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =2 ) +
  facet_wrap(~scenario, nrow = 2)

ggsave(filename = paste0(plots.path,"/", run_number,"_", reg, "_", "FE_Electricity_sector_Tsinghua.png"),width=18, height=10, units = "cm")


###################### FE elshare sector Tsinghua #####################################
reg = "CHA"
df.out_elshare <-NULL
sectors = c("", "|Transport", "|Industry", "|Buildings")

for (sec in sectors){
  # sec = "|Industry"
  df.tot = filter(df, variable ==  paste0("FE",sec), period <= 2080, region == reg , scenario %in% scens_short) %>%
    factor.data.frame()
  
  df.FEEl = df %>% 
    filter(period <= 2080, region == reg, scenario %in% scens_short) %>% 
    filter(variable == paste0("FE",sec, "|Electricity")) %>% 
    select(scenario, period, FEEl = value) 
  
  df.linecomparison_elshare = full_join(df.tot, df.FEEl) %>% 
    select(period,scenario,variable,value,FEEl) %>% 
    mutate(value = FEEl/value * 100) %>% 
    select(-variable)
  
  if (sec == ""){df.linecomparison_elshare$sector <- "all sectors"}
  
  if (sec != ""){df.linecomparison_elshare$sector <- substr(sec, 2,nchar(sec))}
  
  df.out_elshare <- rbind(df.out_elshare, df.linecomparison_elshare)
}

df.elshare_Tsinghua = full_join(df.Tsinghua_2C, df.Tsinghua_2Ce) %>% 
  filter(period %in% c(2020,2030,2050))

df.out_elshare_Tsing <-NULL
for (sec in sectors){
    # sec = "|Buildings"
    df.tot_Tsing = filter(df.elshare_Tsinghua, variable ==  paste0("FE",sec)) %>%
      factor.data.frame()
    
    df.FEEl_Tsing = df.elshare_Tsinghua %>% 
      filter(variable == paste0("FE",sec, "|Electricity")) %>% 
      select(scenario, period, FEEl = value) 
    
    df.elshare_Tsing = full_join(df.tot_Tsing, df.FEEl_Tsing) %>% 
      select(period,scenario,variable,value,FEEl) %>% 
      mutate(value = FEEl/value * 100) %>% 
      select(-variable,FEEl) 
    
    if (sec == ""){df.elshare_Tsing$sector <- "all sectors"}
    
    if (sec != ""){df.elshare_Tsing$sector <- substr(sec, 2,nchar(sec))}
    
  df.out_elshare_Tsing <- rbind(df.out_elshare_Tsing, df.elshare_Tsing)
}
df.out_elshare_Tsing$scenario <- "Tsinghua 2C"


p<-ggplot() + geom_line(data= df.out_elshare, aes(x=period, y = value, color = scenario)) +
  geom_line(data= df.out_elshare_Tsing, aes(x=period, y = value, color = scenario)) +
  theme_bw() +
  scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = paste0(c(10,20,30,40,50,60,70,80,90),"%"))+
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  # coord_cartesian(ylim = c(15,80))+
  ylab("Electricity Share of Final Energy  [%]")+
  scale_color_manual(values = cbbPalette) + 
  facet_wrap(~sector, nrow = 2)+
  labs(linetype = "peaking Time", color = "scenario") +
  guides(color=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=2,byrow=TRUE)) + 
  theme(legend.title = element_text(size = 8),                                                                                     legend.text = element_text(size = 8))

ggsave(filename = paste0(plots.path,"/",run_number,"_", reg, "_", "FE_elshare.png"),width=20, height=10, units = "cm")
# }

###################### FE elshare total Tsinghua #####################################
# for (reg in regs_elecshare){
if (reg == "CHA"){
  df.tot =  filter(df, variable ==  "FE", period <= 2080, region == reg ,period <= tmax, scenario %in% scens_short) %>%
    factor.data.frame()
  
  df.FEEl = df %>% 
    filter(period <= 2080, region == reg, scenario %in% scens_short) %>% 
    filter(variable %in% c("FE|Electricity")) %>% 
    select(scenario, period, FEEl = value) 
  
  df.linecomparison_elshare = full_join(df.tot, df.FEEl) %>% 
    mutate(value = FEEl/value * 100) 
  
  levels(df.linecomparison_elshare$scenario) <- scens_short
  
df.tsinhua_elshare <- data.frame(period=seq(from = 2020, to = 2070, by = 5), 
                 value=c(27,30,32,36,41,48,57,66,79,79,80), scenario = "2060CN") 

df.linecomparison_elshare2 = list(df.linecomparison_elshare, df.tsinhua_elshare) %>%
  reduce(full_join) 

levels(df.linecomparison2$scenario) <- c(scens_short, "2060CN")

p<-ggplot() + geom_line(data=df.linecomparison_elshare2, aes(x=period, y = value, color = scenario)) +
  theme_bw() +
  scale_y_continuous(breaks=c(20,30,40,50,60,70,80,90), labels = paste0(c(20,30,40,50,60,70,80,90),"%"))+
  labs(color = "") +
  theme(strip.background = element_blank()) +
  theme(legend.position = "right") +
  coord_cartesian(xlim = c(2010,2080),ylim = c(15,80))+
  ylab("Electricity Share of Final Energy  [%]")+
  scale_color_manual(values = cbbPalette)+
  labs(linetype = "peaking Time", color = "scenario")

ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", "FE_elshare_Tsinghua.png"),width=18, height=10, units = "cm")
}

###################### Sector and carrier resfuel remand #######################
## residual fuel demand

vars = c(
  "FE|Transport|Electricity",
  "FE|Buildings|Electricity",
  "FE|Industry|Electricity",
  "FE|Transport|Hydrogen",
  "FE|Buildings|Hydrogen",
  "FE|Industry|Hydrogen",
  "FE|Industry|Heat",
  "FE|Buildings|Heat",
  "FE|Transport|Gases",                                                                    
  "FE|Buildings|Gases",       
  "FE|Industry|Gases",                                                                    
  "FE|Transport|Liquids",     
  "FE|Buildings|Liquids",     
  "FE|Industry|Liquids",  
  "FE|Industry|Solids",  
  "FE|Buildings|Solids",
  NULL
)

vars_tot = c(
  "FE|Electricity",
  "FE|Hydrogen",
  "FE|Heat",
  "FE|Gases",
  "FE|Liquids",
  "FE|Solids",
  "FE",
  NULL
)

# reg = "CHA"
reg = "World"
tmax = 2100

carriers = 
  c("Electricity", "Hydrogen",  "Heat", "Gases","Liquids", "Solids")

df.plot = filter(df, variable %in% vars, period <= tmax, region == reg ) %>%
  # factor.data.frame() %>% 
  # arrange(variable) %>% 
  mutate(value = value) %>%
  separate(variable,into = c("dummy", "sector", "carrier"), remove = F,sep = "\\|" ) %>% 
  quitte::order.levels(carrier = carriers) 
# %>% 
  # mutate(variable = stringr::str_replace(variable, "FE\\|", ""))
# %>%
  # factor.data.frame()

  df.plot %>% getElement("variable")

df.plot$variable <- factor(df.plot$variable, levels=vars)

df.tot = filter(df, variable %in% vars_tot, period <= tmax, region == reg) %>%
  calc_addVariable(
    "`FE|So`"  =  "`FE|Solids`",
    "`FE|SoLi`"  =  "`FE|Solids` + `FE|Liquids`", 
    "`FE|Carb`"  =  "`FE|Solids` + `FE|Liquids`+`FE|Gases`", 
    #                      "`FE|CarbHe`"  =  "`FE|Solids` + `FE|Liquids`+`FE|Gases` + `FE|Heat`", 
    "`FE|CarbHeH2`"  =  "`FE|Solids` + `FE|Liquids`+`FE|Gases` + `FE|Heat`+ `FE|Hydrogen`",
    "`FE|Tot`"  =  "`FE`",
     only.new = TRUE) %>%  
  mutate(value = value) %>%
  factor.data.frame()

df.tot.hist = filter(df.hist_FE, variable == "FE", period >= 1980,period <= 2013, model == "IEA", region == reg) %>%
  mutate(value = .95*value) %>%
  select(-scenario)

geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =2 )

df.plot_check = filter(df.plot,  grepl("|Solids", variable, fixed=TRUE))
df.plot_elec = filter(df.plot,  grepl("|Electricity", variable, fixed=TRUE))

mip::mipArea(df.plot, total = F) +
  facet_grid(~scenario) +
  ylab("Final Energy Demand[EJ/yr]") +
  xlab("") +
  scale_fill_manual(values = as.character(plotstyle(vars)), labels = as.character(plotstyle(vars, out = "legend"))) +
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =2 ) +
  theme_bw() +
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom') +
  facet_wrap(~scenario, nrow = 2)
# +
 # guides(fill = guide_legend(reverse=T))

ggsave(filename = paste0(plots.path,"/", run_number,"_", reg, "_ResFuels.png"),width=20, height=15, units = "cm")

mip::mipArea(df.plot_elec, total = F) +
  facet_grid(~scenario) +
  ylab("Final Energy Demand[EJ/yr]") +
  xlab("") +
  scale_fill_manual(values = as.character(plotstyle(vars)), labels = as.character(plotstyle(vars, out = "legend"))) +
  geom_line(data= df.tot.hist, aes(x=period, y = value), color = "black", size =2 ) +
  theme_bw() +
  theme(strip.background = element_blank()) + 
  theme(legend.position = 'bottom') +
  facet_wrap(~scenario, nrow = 2)
# +
# guides(fill = guide_legend(reverse=T))

ggsave(filename = paste0(plots.path,"/", run_number,"_", reg, "_Elec_ResFuels.png"),width=20, height=15, units = "cm")


for (scen in scens){

mip::mipArea(df.plot %>% filter(scenario == scen), total = F) +
  # facet_grid(~scenario) +
  ylab("Final Energy Demand [EJ/yr]") +
  xlab("") +
  scale_fill_manual(values = as.character(plotstyle(vars)), labels = as.character(plotstyle(vars, out = "legend"))) +
  geom_line(data= df.tot %>% filter(scenario == scen), 
            aes(x=period, y = value, group = variable), color = "black", alpha = 0.8, size =0.4 ) +
  theme_bw() +
  theme(legend.position = 'right') + 
  theme(strip.background = element_blank())

ggsave(filename = paste0(plots.path, "/", run_number,"_", reg, "_", scen,"_ResFuels.png"),width=17, height=13, units = "cm")
}

vars = c(
  "FE|+|Electricity",
  "FE|+|Heat",
  
  "FE|Transport|Hydrogen",
  "FE|Buildings|Hydrogen",
  "FE|Industry|Hydrogen",
  "FE|CDR|Hydrogen",
  
  "FE|Transport|Gases", 
  "FE|Buildings|Gases",                                                           
  "FE|Industry|Gases",                                                                  
  "FE|Transport|Liquids",  
  "FE|Buildings|Liquids",     
  "FE|Industry|Liquids", 
  
  "FE|Buildings|Solids",    
  "FE|Industry|Solids"
)

df.plot = filter(df, variable %in% vars, period <= tmax, region == "CHA", scenario %in% scens_short) %>%
  order.levels(scenario = scens_short, variable = vars) %>%
  mutate(value = value) %>% 
  factor.data.frame()

mipBarYearData(df.plot %>% filter(period %in% t.barplot) ) +
  ylab("Final Energy Demand [EJ/yr]") + 
  theme_bw() +
  theme(strip.background = element_blank())

ggsave(filename = paste0(plots.path, "/", run_number,"_",reg, "_ResFuels_bar0.png"), width=25, height=14, units = "cm")


############################################################
###################### Carbon Price ###############################
reg= "regs"
# reg= "EUR"
cprice = "Price|Carbon"
# tmax = 2100
tmax = 2060

df.linecomparison_cprice = filter(df, variable == cprice, period <= tmax) %>%
  mutate(value = as.numeric(value)) %>%
  select(scenario, region, period, value) 


p <- ggplot() + geom_line(data= df.linecomparison_cprice, aes(x=period, y = value, color = scenario)) +
  theme_bw() +
  labs(color = "") +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  scale_y_continuous(trans='log10') +
  ylab("Carbon Price (US$2015/t CO2)")+
  labs(linetype = "peaking Time", color = "scenario") +
  guides(color=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.title = element_text(size = 8),                                                                                     legend.text = element_text(size = 8))+
  facet_wrap(~region, nrow = 2)

ggsave(filename = paste0(plots.path,"/",run_number,"_", reg,"_CO2price_log.png"),width=20, height=10, units = "cm")

p <- ggplot() + geom_line(data= df.linecomparison_cprice, aes(x=period, y = value, color = scenario)) +
  theme_bw() +
  labs(color = "") +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  # scale_y_continuous(trans='log10') +
  ylab("Carbon Price (US$2015/t CO2)")+
  labs(linetype = "peaking Time", color = "scenario") +
  guides(color=guide_legend(nrow=3,byrow=TRUE), linetype=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.title = element_text(size = 6),                                                                                     legend.text = element_text(size = 6))+
  facet_wrap(~region, nrow = 3, scales = 'free_y')

ggsave(filename = paste0(plots.path,"/",run_number,"_", reg,"_CO2price_lin.png"),width=17, height=10, units = "cm")

###############################################################################
###############################################################################
###################### Transport EDGE ################################################
df_test = filter(df, grepl("FE|Transport|Pass|Road", variable, fixed=TRUE), period > 2005, period < 2055, scenario == "Baseline", region == "CHA")  

df.trans = df %>%
  calc_addVariable("`FE|Transport|Pass|Rail+Bus|Hydrocarbons`" = "`FE|Transport|Pass|Rail|Liquids` +
                                       `FE|Transport|Pass|Road|Bus|Liquids` + `FE|Transport|Pass|Road|Bus|Gases`" ,
                   "`FE|Transport|Pass|Rail+Bus|Electricity`" = "`FE|Transport|Pass|Rail|Electricity` 
                   # +
                                       # `FE|Transport|Pass|Road|Bus|Electricity`",
                   "`FE|Transport|Freight|Road|Hydrocarbons`" = "`FE|Transport|Freight|Road|Liquids` +
                                       `FE|Transport|Freight|Road|Gases`",
                   "`FE|Transport|Pass|Road|LDV|Hydrocarbons`" =  "`FE|Transport|Pass|Road|LDV|Liquids` +
                                       `FE|Transport|Pass|Road|LDV|Gases`",
                   "`FE|Transport|Pass|Aviation|Hydrocarbons`" =  "`FE|Transport|Pass|Aviation|Domestic|Liquids` +
                                       `FE|Transport|Pass|Aviation|International|Liquids`",
                   "`FE|Transport|Freight|Shipping|Hydrocarbons`" =  "`FE|Transport|Freight|International Shipping|Liquids` +
                                       `FE|Transport|Freight|Navigation|Liquids`",
                   units = "EJ/yr") %>% factor.data.frame()

# FIXME: Account for electric two-wheelers
df.trans = df.trans %>%
  calc_addVariable("`ES|Transport|Pass|Rail+Bus|Hydrocarbons`" = "`ES|Transport|Pass|Rail|Liquids` + `FE|Transport|Pass|Road|Bus|Liquids` + `FE|Transport|Pass|Road|Bus|Gases`",
                   "`ES|Transport|Pass|Rail+Bus|Electricity`" = "`ES|Transport|Pass|Rail|Electric` 
                   # + `ES|Transport|Pass|Road|Bus|Electric`",
                   "`ES|Transport|Pass|Road|LDV|Hydrocarbons`" = "`ES|Transport|Pass|Road|LDV|Liquids` + `ES|Transport|Pass|Road|LDV|Gases` - 	`ES|Transport|Pass|Road|LDV|Two-Wheelers|Liquids` ",
                   units = "bn pkm/yr") %>%
  factor.data.frame()

## add the colors associated

## FE electricity
tt = plotstyle.add(entity = "Pass|Road|LDV|Electricity", legend = "Pass|Road|LDV|Electricity", c("#00b159"), replace = T)
tt = plotstyle.add(entity = "Pass|Rail+Bus|Electricity", legend = "Pass|Rail+Bus|Electricity", c("#7cfc00"), replace = T)
tt = plotstyle.add(entity = "Freight|Road|Electricity", legend = "Freight|Road|Electricity", c("#68c6a4"), replace = T)
## FE hydrogen
tt = plotstyle.add(entity = "Pass|Road|LDV|Hydrogen", legend = "Pass|Road|LDV|Hydrogen", c("#00aedb"), replace = T)
tt = plotstyle.add(entity = "Pass|Bus|Hydrogen", legend = "Pass|Bus|Hydrogen", c("#00bfff"), replace = T)
tt = plotstyle.add(entity = "Freight|Road|Hydrogen", legend = "Freight|Road|Hydrogen", c("#035aa6"), replace = T)
## FE hydrocarbons
tt = plotstyle.add(entity = "Pass|Road|LDV|Hydrocarbons", legend = "Pass|Road|LDV|Hydrocarbons", c("#8c8c8c"), replace = T)
tt = plotstyle.add(entity = "Pass|Rail+Bus|Hydrocarbons", legend = "Pass|Bus+Rail|Hydrocarbons", c("#b2b2b2"), replace = T)
tt = plotstyle.add(entity = "Freight|Road|Hydrocarbons", legend = "Freight|Road|Hydrocarbons", c("#787f77"), replace = T)
tt = plotstyle.add(entity = "Pass|Aviation|Hydrocarbons", legend = "Pass|Aviation|Hydrocarbons", c("#474847"), replace = T)
tt = plotstyle.add(entity = "Freight|Shipping|Hydrocarbons", legend = "Freight|Shipping|Hydrocarbons", c("#53515b"), replace = T)


tt = plotstyle.add(entity = "Hydrocarbons", legend = "ICE", c("#8c8c8c"), replace = T)
tt = plotstyle.add(entity = "BEV", legend = "BEV", c("#00b159"), replace = T)
tt = plotstyle.add(entity = "Hybrid Liquids", legend = "Full/Mild hybrids", c("#ffc425"), replace = T)
tt = plotstyle.add(entity = "Hybrid Electric", legend = "PHEV", c("#f37735"), replace = T)
tt = plotstyle.add(entity = "FCEV", legend = "FCEV", c("#00aedb"), replace = T)
## plot all transport modes, divided into h2, electricity, and hydrocarbon groups

vars2plot = c(
  ## FE Transport electricity
  "FE|Transport|Pass|Road|LDV|Electricity", 
  "FE|Transport|Pass|Rail+Bus|Electricity",
  "FE|Transport|Freight|Road|Electricity",
  
  ## FE Transport hydrogen
  "FE|Transport|Pass|Road|LDV|Hydrogen", 
  "FE|Transport|Pass|Bus|Hydrogen",
  "FE|Transport|Freight|Road|Hydrogen", 
  
  ## FE Transport hydrocarbons
  "FE|Transport|Freight|Road|Hydrocarbons",
  "FE|Transport|Freight|Shipping|Hydrocarbons",
  "FE|Transport|Pass|Road|LDV|Hydrocarbons", 
  "FE|Transport|Pass|Rail+Bus|Hydrocarbons",
  "FE|Transport|Pass|Aviation|Hydrocarbons")


reg = "CHA"
# reg = "World"

#vars2plot = rev(vars2plot)

df.plot = filter(df.trans, variable %in% vars2plot, period <= tmax , scenario %in% scens)  %>% 
  order.levels(scenario = scens, variable = vars2plot) 

levels(df.plot$scenario) <- scens

ciccio=
  mipArea(filter(df.plot, region == reg)) +
  ylab("FE transport\n[EJ/yr]") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "right") +
  facet_wrap(~scenario, nrow = 2)


ggsave(filename = paste0(plots.path, "/", run_number,"_",reg, "_FEdetailTransp_mr.png"), width=20, height=10, units = "cm")


############################################################
###################### Industry subsector #########################
# Energy service industry
reg = "EUR"
# reg = "World"

vars = c("Production|Industry|Cement", 
         "Production|Industry|Steel|Primary", 
         "Production|Industry|Steel|Secondary", 
         "Value Added|Industry|Chemicals")
newvars = c("Production|Industry|Cement (Mt/yr)", 
            "Production|Industry|Steel|Primary (Mt/yr)", 
            "Production|Industry|Steel|Secondary (Mt/yr)", 
            "Value Added|Industry|Chemicals (billion US$2005/yr)")

df.linecomparison_ES_ind = filter(df, variable %in% vars, period <= tmax) %>% 
  order.levels(scenario = scens_short, variable = vars) %>%
  mutate(value = value) %>%
  factor.data.frame()

df.linecomparison_ES_ind$variable <- plyr::mapvalues(df.linecomparison_ES_ind$variable, from = vars, to = newvars)

for (reg in regs){
  # reg = "EUR"
p<-ggplot() + geom_line(data= df.linecomparison_ES_ind %>% filter(region==reg), aes(x=period, y = value, color = scenario)) +
  theme_bw() +
  labs(color = "") +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  ylab("Energy Service") +
  labs(scolor = "scenario") +
  guides(color=guide_legend(nrow=2, byrow=TRUE), linetype=guide_legend(nrow=2, byrow=TRUE))+ 
  theme(legend.title = element_text(size = 8),                                                                                     legend.text = element_text(size = 8)) +
  facet_wrap(~variable, nrow = 2)

ggsave(filename = paste0(plots.path,"/",run_number,"_", reg,"_EnergyService_Industry.png"), width=17, height=10, units = "cm")
}

############################################################
# ---- prepare comparison data ---
comp_FE <- tribble(
  ~source,         ~region,   ~subsector,      ~period,   ~fety,     ~value,
  'IEA ETP RTS',   'World',   'Cement',        2015,      'Total',   10.633,
  'IEA ETP RTS',   'World',   'Cement',        2025,      'Total',   11.990,
  'IEA ETP RTS',   'World',   'Cement',        2030,      'Total',   11.986,
  'IEA ETP RTS',   'World',   'Cement',        2035,      'Total',   11.862,
  'IEA ETP RTS',   'World',   'Cement',        2040,      'Total',   12.146,
  'IEA ETP RTS',   'World',   'Cement',        2045,      'Total',   12.166,
  'IEA ETP RTS',   'World',   'Cement',        2050,      'Total',   12.089,
  'IEA ETP RTS',   'World',   'Cement',        2055,      'Total',   11.912,
  'IEA ETP RTS',   'World',   'Cement',        2060,      'Total',   11.897,
  
  'IEA ETP RTS',   'World',   'Chemicals',     2015,      'Total',   42.478,
  'IEA ETP RTS',   'World',   'Chemicals',     2025,      'Total',   63.799,
  'IEA ETP RTS',   'World',   'Chemicals',     2030,      'Total',   67.630,
  'IEA ETP RTS',   'World',   'Chemicals',     2035,      'Total',   70.335,
  'IEA ETP RTS',   'World',   'Chemicals',     2040,      'Total',   72.085,
  'IEA ETP RTS',   'World',   'Chemicals',     2045,      'Total',   75.230,
  'IEA ETP RTS',   'World',   'Chemicals',     2050,      'Total',   78.217,
  'IEA ETP RTS',   'World',   'Chemicals',     2055,      'Total',   82.724,
  'IEA ETP RTS',   'World',   'Chemicals',     2060,      'Total',   88.769,
  
  'IEA ETP RTS',   'World',   'Steel',         2015,      'Total',   35.615,
  'IEA ETP RTS',   'World',   'Steel',         2025,      'Total',   38.990,
  'IEA ETP RTS',   'World',   'Steel',         2030,      'Total',   40.551,
  'IEA ETP RTS',   'World',   'Steel',         2035,      'Total',   43.200,
  'IEA ETP RTS',   'World',   'Steel',         2040,      'Total',   47.544,
  'IEA ETP RTS',   'World',   'Steel',         2045,      'Total',   47.894,
  'IEA ETP RTS',   'World',   'Steel',         2050,      'Total',   48.137,
  'IEA ETP RTS',   'World',   'Steel',         2055,      'Total',   48.082,
  'IEA ETP RTS',   'World',   'Steel',         2060,      'Total',   47.475,
  
  'IEA ETP RTS',   'World',   'Total',         2015,      'Total',   153.653,
  'IEA ETP RTS',   'World',   'Total',         2025,      'Total',   188.735,
  'IEA ETP RTS',   'World',   'Total',         2030,      'Total',   199.316,
  'IEA ETP RTS',   'World',   'Total',         2035,      'Total',   210.044,
  'IEA ETP RTS',   'World',   'Total',         2040,      'Total',   220.492,
  'IEA ETP RTS',   'World',   'Total',         2045,      'Total',   229.172,
  'IEA ETP RTS',   'World',   'Total',         2050,      'Total',   237.257,
  'IEA ETP RTS',   'World',   'Total',         2055,      'Total',   244.937,
  'IEA ETP RTS',   'World',   'Total',         2060,      'Total',   253.300
) %>% 
  pivot_wider(names_from = subsector) %>% 
  mutate(other = Total - Cement - Chemicals - Steel) %>% 
  pivot_longer(!(1:4), names_to = 'subsector')

# ---- setup plot scales ----
colours_fety <- c('Solids'            = '#191919',
                  'Liquids'           = '#0000cc',
                  'Gases'             = '#999966',
                  'Hydrogen'          = '#66cccc',
                  'Heat'              = '#cc0000',
                  'Electricity (HTH)' = '#ff7f00',
                  'Electricity'       = '#ffb200')

shapes_comp <- c('IEA ETP RTS'                = 'circle filled',
                 'Steel Statistical Yearbook' = 'cross')

# ---- prepare data ---- 

d_FE <- df %>% 
  filter(region == reg) %>% 
  filter(2060 >= period,
         grepl(paste0('^FE\\|Industry\\|',
                      '(Cement|Chemicals|Other Industry|Steel(\\|(Primary|Secondary)?))',
                      '(\\|Solids|Liquids|Gases|Hydrogen|Heat|Electricity)',
                      '(\\|(High|Mechanical work and low)-temperature Heat)?$'
                      ),
               variable)) 
  mutate(
    subsector = sub(paste0('.*(Cement|Chemicals|Other Industry|Steel(\\|(Primary|',
                           'Secondary))?).*'), '\\1', variable),
    subsector = stringr::str_trim(sub('^(.*)\\|(.*)$', '\\2 \\1', subsector)),
    extra = sub('.*((High|Mechanical work and low)-temperature Heat)$', '\\1',
                variable),
    extra = ifelse(variable == extra, '', extra),
    fety = sub('^FE\\|Industry\\|([^\\|]*).*', '\\1', variable)
  ) 
  select(-variable) %>% 
  group_by(scenario, region, period, subsector, fety) %>% 
  filter(!(  '' == extra 
             & 'Electricity' == fety 
             & 'High-Temperature Heat' %in% extra), scenario %in% scens_short) %>% 
  ungroup() %>% 
  mutate(fety = ifelse('High-Temperature Heat' == extra, 'Electricity (HTH)',
                       fety)) %>% 
  select(-extra) %>% 
  sum_total(subsector) %>% 
  order.levels(
    fety = rev(names(colours_fety)),
    subsector = c('Cement', 'Chemicals', 'Steel', 'Primary Steel', 
                  'Secondary Steel', 'Other Industry', 'Total')
  )
#######################################################################################
#######################################################################################
reg = "CHA"
# reg = "EUR"
# reg = "World"
# ---- plot absolute values ----
cat('\n\n### Absolute Values\n')
p <- ggplot_bar_remind_vts(data = d_FE %>% 
                             filter(reg == region),
                           mapping = aes(x = period, y = value, fill = fety),
                           gaps = 0) +
  scale_fill_manual(values = colours_fety, name = NULL) +
  # geom_point(data = comp_FE %>% 
  # 				  	filter('World' == region) %>% 
  # 		   	add_timesteps_columns(remind_timesteps),
  # 		   mapping = aes(x = xpos, y = value, shape = source),
  # 		   colour = 'red',
  # 		   fill = 'white') +
  # 	scale_shape_manual(values = shapes_comp, name = NULL) +
  scale_x_continuous(breaks = c(2020, 2050,  2100),
                     minor_breaks = unique(remind_timesteps$period)) +
  facet_wrap(subsector ~ scenario, scales = 'free_y') +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = 'EJ/yr') +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "right")


# plot(p)

ggsave(filename = paste0(plots.path, "/", run_number,"_",reg, "_FEdetailIndustryAll_allscens.png"),width=24, height=20, units = "cm")

# ---- plot shares ----
cat('\n\n### Shares\n')
p <- ggplot_bar_remind_vts(data = d_FE %>% 
                             filter('World' == region,
                                    'Secondary Steel' != subsector),
                           mapping = aes(x = period, y = value, fill = fety),
                           gaps = 0,
                           position_fill = TRUE) +
  scale_fill_manual(values = colours_fety, name = NULL) +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100),
                     minor_breaks = unique(remind_timesteps$period)) +
  facet_grid(subsector ~ scenario, scales = 'free_y') +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  ttheme_minimal()
# plot(p)

# ---- plot absolute values ----
cat('\n\n### Absolute Values\n')
p <- ggplot_bar_remind_vts(data = d_FE %>% 
                             filter(reg == region, 
                                    # scenario == default_scen,
                                    subsector %in% c("Steel", "Chemicals", "Total")),
                           mapping = aes(x = period, y = value, fill = fety),
                           gaps = 0) +
  scale_fill_manual(values = colours_fety, name = NULL) +
  # geom_point(data = comp_FE %>% 
  # 				  	filter('World' == region) %>% 
  # 		   	add_timesteps_columns(remind_timesteps),
  # 		   mapping = aes(x = xpos, y = value, shape = source),
  # 		   colour = 'red',
  # 		   fill = 'white') +
  scale_shape_manual(values = shapes_comp, name = NULL) +
  scale_x_continuous(breaks = c(2020, 2050,  2100),
                     minor_breaks = unique(remind_timesteps$period)) +
  facet_wrap(~ subsector , scales = 'free', nrow = 2) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = 'EJ/yr') +
  theme_bw() +
  theme(strip.background = element_blank())
# plot(p)

ggsave(filename = paste0(plots.path, "/", run_number,"_",reg, "_FEdetailIndustry.png"),width=16, height=10, units = "cm")

