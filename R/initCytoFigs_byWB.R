# initCytoFigs_byWB.R ####
# produce some initial figs by WB of selected cyto data parameters

## load packages ####
ld_pkgs <- c("tidyverse", "tictoc","sf","maps","nngeo", "stringr","ggforce")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

tictoc::tic.clearlog()

#Load data####
tic("Load data")
source("R/data2WB.R")

df0 <- df

### remove data outside of WB border
df %>% filter(., distance_to_nearest <2000) -> df
toc(log=TRUE)

# data formatting ####
tic("data formatting")
### steps:
# = remove unnecessary columns
# = lengthen data
# = produce facet plots of interesting cols

cnt <- c("Counts_pml_picoRed","Counts_pml_nanoCrypto1",
         "Counts_pml_nanoCrypto1_HS", "Counts_pml_nanoCrypto2_HS",
         "Counts_pml_nanoRed_HS",
         "Counts_pml_microCrypto1","Counts_pml_microCrypto1_HS",
         "Counts_pml_microCrypto2","Counts_pml_microCrypto2_HS",
         "Counts_pml_microRed","Counts_pml_microRed_HS",
         "Counts_pml_NanoRed_HS_nw","Counts_pml_NanoRed_nw","Counts_pml_WrSi")
fws <- c("SUM_Total_FWS_pml_microCrypto1","SUM_Total_FWS_pml_microCrypto1_HS",
         "SUM_Total_FWS_pml_microCrypto2","SUM_Total_FWS_pml_microCrypto2_HS",
         "SUM_Total_FWS_pml_microRed","SUM_Total_FWS_pml_microRed_HS",
         "SUM_Total_FWS_pml_NanoRed_HS_nw","SUM_Total_FWS_pml_NanoRed_nw",
         "SUM_Total_FWS_pml_WrSi","SUM_Total_FWS_pml_picoRed",
         "SUM_Total_FWS_pml_nanoCrypto1","SUM_Total_FWS_pml_nanoCrypto1_HS",
         "SUM_Total_FWS_pml_nanoCrypto2_HS","SUM_Total_FWS_pml_nanoRed_HS")
sws <- c("SUM_Total_SWS_HS_pml_picoRed","SUM_Total_SWS_HS_pml_nanoCrypto1",
         "SUM_Total_SWS_HS_pml_nanoCrypto1_HS",
         "SUM_Total_SWS_HS_pml_nanoCrypto2_HS","SUM_Total_SWS_HS_pml_nanoRed_HS",
         "SUM_Total_SWS_HS_pml_microCrypto1",
         "SUM_Total_SWS_HS_pml_microCrypto1_HS","SUM_Total_SWS_HS_pml_microCrypto2",
         "SUM_Total_SWS_HS_pml_microCrypto2_HS","SUM_Total_SWS_HS_pml_microRed",
         "SUM_Total_SWS_HS_pml_microRed_HS","SUM_Total_SWS_HS_pml_NanoRed_HS_nw",
         "SUM_Total_SWS_HS_pml_NanoRed_nw","SUM_Total_SWS_HS_pml_WrSi")

mt <- c("Date","WB_Name","WB_Type","RBD") ##row info columns

# extract OSGB & write file ####
east <- sf::st_coordinates(df$geometry)[,1]
north <- sf::st_coordinates(df$geometry)[,2]

df %>% dplyr::select(all_of(c(mt,cnt, fws, sws))) %>% 
  st_drop_geometry() %>% ## remove geometry column
  as_tibble() %>% #names()# convert to tibble
  ### tidy column names
  #modify count names
  rename_with(
    .cols = 5:18,
    #.fn = ~str_c(str_sub(.,12),"_ml")) %>% names()
    .fn = ~ str_c(
      str_remove_all(str_sub(., 12), "_"),
      "_ml")) %>% #names()
  #modify FWS names
  rename_with(
    .cols = 19:32, 
    # .fn = ~str_c(str_sub(.,19),"_FWS")) %>% #names()
    .fn = ~ str_c(
      str_remove_all(str_sub(., 19), "_"),
      "_FWS")) %>% #names()
  #modify SWS names
  rename_with(
    .cols = 33:46, 
    # .fn = ~str_c(str_sub(.,22),"_SWS")) %>% #names()
    .fn = ~ str_c(
      str_remove_all(str_sub(., 22), "_"),
      "_SWS")) %>% #names()
  ### append eastings and northings
  mutate("OSBG_Northings"=north) %>% relocate(OSBG_Northings, .after = RBD) %>%
  mutate("OSBG_Eastings"=east) %>% relocate(OSBG_Eastings, .after = RBD) %>% #View()
  ### lengthen data
  pivot_longer(.,
               cols = -c("Date","WB_Name","WB_Type","RBD","OSBG_Eastings","OSBG_Northings"),
               names_to = "variable",
               values_to = "value"
  ) %>% 
  ### remove zero values
  filter(.,value != 0) %>% 
  mutate(., variable = paste0(tolower(substr(variable, 1, 1)),
                              substr(variable,
                                     2,
                                     nchar(variable)))) -> df_trim_l
write.csv(df_trim_l, file="data_out/EA_Cytodata_2024.csv",row.names = FALSE)
toc(log = TRUE)

# export data ####

tic("generate plots")
# generate plots ####
## count data ####
df_trim_l %>% 
  filter(.,str_ends(variable, "_ml")) %>% 
  filter(., variable != "wrSi_ml") %>% 
  filter(.,!WB_Name %in% c("BURE & WAVENEY & YARE & LOTHING","TEES","HUMBER LOWER",
                           "GREAT OUSE","Suffolk","TWEED","WASH INNER")) %>% 
  ggplot(., aes(x= variable,
                #col=WB_Name,
                y= log(value+1)
  ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(height = 0, width = 0.25, alpha=.25)+
  facet_wrap(.~WB_Name)+
  labs(
    title = "Log (n+1) counts per ml"
  ) +
  theme(
    axis.text.x = element_text(angle=90*3, hjust = 0, vjust=0),
    axis.title = element_blank(),
    strip.text = element_text(face=2)
  ) -> pl

ggsave(plot=pl, file="figs/initPlot_box_counts_WBs.pdf",width = 14, height = 8)
rm(pl)

## FWS data ####
df_trim_l %>% 
  filter(.,str_ends(variable, "_FWS")) %>% 
  filter(., variable != "wrSi_FWS") %>% 
  filter(.,!WB_Name %in% c("BURE & WAVENEY & YARE & LOTHING","TEES","HUMBER LOWER",
                           "GREAT OUSE","Suffolk","TWEED","WASH INNER")) %>% 
  ggplot(., aes(x= variable,
                #col=WB_Name,
                y= log(value+1)
  ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(height = 0, width = 0.25, alpha=.25)+
  facet_wrap(.~WB_Name)+
  labs(
    title = "Log (n+1) total FWS values per ml"
  ) +
  theme(
    axis.text.x = element_text(angle=90*3, hjust = 0, vjust=0),
    axis.title = element_blank(),
    strip.text = element_text(face=2)
  ) -> pl

ggsave(plot=pl, file="figs/initPlot_box_FWS_WBs.pdf",width = 14, height = 8)
rm(pl)

## SWS data ####
df_trim_l %>% 
  filter(.,str_ends(variable, "_SWS")) %>% 
  filter(., variable != "wrSi_SWS") %>% 
  filter(.,!WB_Name %in% c("BURE & WAVENEY & YARE & LOTHING","TEES","HUMBER LOWER",
                           "GREAT OUSE","Suffolk","TWEED","WASH INNER")) %>% 
  ggplot(., aes(x= variable,
                #col=WB_Name,
                y= log(value+1)
  ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(height = 0, width = 0.25, alpha=.25)+
  facet_wrap(.~WB_Name)+
  labs(
    title = "Log (n+1) total SWS values per ml"
  ) +
  theme(
    axis.text.x = element_text(angle=90*3, hjust = 0, vjust=0),
    axis.title = element_blank(),
    strip.text = element_text(face=2)
  ) -> pl

ggsave(plot=pl, file="figs/initPlot_box_SWS_WBs.pdf",width = 14, height = 8)
rm(pl)

toc(log=TRUE)

# Time series ####

toc(log=TRUE)

unlist(tictoc::tic.log())

# Plots by lifeforms ####
tic("Plots by lifeforms")
unique_vars <- unique(df_trim_l$variable)
unique_vars <- unique_vars[!grepl("^wrSi", unique_vars)]

# Loop through each variable
## log abundance
for (var in unique_vars) {
  # Filter the dataset for the current variable
  filtered_data <- df_trim_l %>% filter(variable == var)
  
  # Create the plot
  filtered_data %>% 
    filter(!WB_Name %in% c("BURE & WAVENEY & YARE & LOTHING","TEES","HUMBER LOWER",
                           "GREAT OUSE","Suffolk","TWEED","WASH INNER")) %>% 
    ggplot(., aes(x = Date, y = log(value + 1))) +
    geom_point() +
    geom_smooth(method = "loess",se = FALSE,span=.999) +
    ylim(0, NA) +
    labs(
      title = var,
      subtitle = "Log (n+1) total abundance values per ml",
      caption="Blue line indicates loess smooth term (span = 0.999)"
    ) +
    facet_wrap(. ~ WB_Name) +
    theme(
      axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0),
      axis.title = element_blank(),
      strip.text = element_text(face = 2)
    ) -> pl
  
  # Save the plot
  ggsave(
    filename = file.path(paste0("figs/log_",var,"_by_lifeform_by_WB", ".pdf")),
    plot = pl,
    device = "pdf",
    height = 8,
    width = 14
  )
}

## non-log abundance
for (var in unique_vars) {
  # Filter the dataset for the current variable
  filtered_data <- df_trim_l %>% filter(variable == var)
  
  # Create the plot
  filtered_data %>% 
    filter(!WB_Name %in% c("BURE & WAVENEY & YARE & LOTHING","TEES","HUMBER LOWER",
                           "GREAT OUSE","Suffolk","TWEED","WASH INNER")) %>% 
    ggplot(., aes(x = Date, y = value)) +
    geom_point() +
    geom_smooth(method = "loess",se = FALSE,span=.999) +
    ylim(0, NA) +
    labs(
      title = var,
      subtitle = "Total abundance values per ml",
      caption="Blue line indicates loess smooth term (span = 0.999)"
    ) +
    facet_wrap(. ~ WB_Name) +
    theme(
      axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0),
      axis.title = element_blank(),
      strip.text = element_text(face = 2)
    ) -> pl
  
  # Save the plot
  ggsave(
    filename = file.path(paste0("figs/",var,"_by_lifeform_by_WB", ".pdf")),
    plot = pl,
    device = "pdf",
    height = 8,
    width = 14
  )
}


rm(unique_vars, filtered_data, pl)
toc(log = TRUE)

unlist(tictoc::tic.log())

##tidy up
rm(list = ls(pattern = "^(df|cb)"))
rm(cnt, facet,fws,GISfol,i,mt,n_pages,nit,perms,ppi,sws,var,east,north)

detach("package:ggforce", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:nngeo", unload=TRUE)
detach("package:maps", unload=TRUE)
detach("package:sf", unload=TRUE)
detach("package:tictoc", unload=TRUE)
detach("package:tidyverse", unload=TRUE)
