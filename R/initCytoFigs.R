# initCytoFigs.R ####
# produce some initial figs by WB of selected cyto data parameters

## load packages ####
ld_pkgs <- c("tidyverse", "tictoc","sf","maps","nngeo", "stringr")
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

mt <- c("WB_Name","WB_Type","RBD") ##row info columns

df %>% dplyr::select(all_of(c(mt,cnt, fws, sws))) %>% 
  st_drop_geometry() %>% ## remove geometry column
  as_tibble() %>% #names()# convert to tibble
  ### tidy column names
  #modify count names
  rename_with(
    .cols = 4:17,
    #.fn = ~str_c(str_sub(.,12),"_ml")) %>% names()
    .fn = ~ str_c(
      str_remove_all(str_sub(., 12), "_"),
      "_ml")) %>% #names()
  #modify FWS names
  rename_with(
    .cols = 18:31, 
    # .fn = ~str_c(str_sub(.,19),"_FWS")) %>% #names()
    .fn = ~ str_c(
      str_remove_all(str_sub(., 19), "_"),
      "_FWS")) %>% #names()
  #modify SWS names
  rename_with(
    .cols = 32:45, 
    # .fn = ~str_c(str_sub(.,22),"_SWS")) %>% #names()
    .fn = ~ str_c(
      str_remove_all(str_sub(., 22), "_"),
      "_SWS")) %>% #names()
  ### lengthen data
  pivot_longer(.,
               cols = -c("WB_Name","WB_Type","RBD"),
               names_to = "variable",
               values_to = "value"
               ) %>% 
  mutate(., variable = paste0(tolower(substr(variable, 1, 1)),
                               substr(variable,
                                      2,
                                      nchar(variable)))) -> df_trim_l
toc(log = TRUE)

tic("generate plots")
# generate plots ####
# count data ####
df_trim_l %>% 
  filter(.,str_ends(variable, "_ml")) %>% 
  ggplot(., aes(x= variable,
                #col=WB_Name,
                y= log(value+1)
                ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(height = 0, width = 0.25, alpha=.25)+
  facet_wrap(.~RBD)+
  labs(
    title = "Log (n+1) counts per ml"
    ) +
  theme(
    axis.text.x = element_text(angle=90*3, hjust = 0, vjust=0),
    axis.title = element_blank(),
    strip.text = element_text(face=2)
  ) -> pl
  
ggsave(plot=pl, file="figs/initPlot_box_counts.pdf",width = 14, height = 8)
rm(pl)

#FWS data ####
df_trim_l %>% 
  filter(.,str_ends(variable, "_FWS")) %>% 
  ggplot(., aes(x= variable,
                #col=WB_Name,
                y= log(value+1)
  ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(height = 0, width = 0.25, alpha=.25)+
  facet_wrap(.~RBD)+
  labs(
    title = "Log (n+1) total FWS values per ml"
  ) +
  theme(
    axis.text.x = element_text(angle=90*3, hjust = 0, vjust=0),
    axis.title = element_blank(),
    strip.text = element_text(face=2)
  ) -> pl

ggsave(plot=pl, file="figs/initPlot_box_FWS.pdf",width = 14, height = 8)
rm(pl)

#SWS data ####
df_trim_l %>% 
  filter(.,str_ends(variable, "_SWS")) %>% 
  ggplot(., aes(x= variable,
                #col=WB_Name,
                y= log(value+1)
  ))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(height = 0, width = 0.25, alpha=.25)+
  facet_wrap(.~RBD)+
  labs(
    title = "Log (n+1) total SWS values per ml"
  ) +
  theme(
    axis.text.x = element_text(angle=90*3, hjust = 0, vjust=0),
    axis.title = element_blank(),
    strip.text = element_text(face=2)
  ) -> pl

ggsave(plot=pl, file="figs/initPlot_box_SWS.pdf",width = 14, height = 8)
rm(pl)

toc(log=TRUE)

unlist(tictoc::tic.log())
