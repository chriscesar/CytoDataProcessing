# initPCA.R ####
# Principal components analysis of raw cyz export

# load packages ####
ld_pkgs <- c("tidyverse", "tictoc","janitor","FactoMineR", "ggfortify")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)
theme_set(ggthemes::theme_few())

# load data ####
df0 <- as_tibble(read.csv("data_in/s Measurement 2024-06-06 12h12_Listmode.csv",
                header = TRUE))

### first PCA
df0 %>% 
  select(., -c(Particle.ID, Sample.Length, Arrival.Time)) -> df

## normalise data
dfnorm <- scale(df)

pca01 <- princomp(na.omit(dfnorm))
summary(pca01)
pca01$loadings

plot(pca01)
# factoextra::fviz_pca(pca01)

pca02 <- prcomp(na.omit(dfnorm), scale. = TRUE)

# autoplot(pca02,
#          loadings=TRUE,
#          loadings.label = TRUE,
#          loadings.colour = 'blue',
#          loadings.label.size = 3)

# Getting proportion of variance explained by PC1 and PC2
prop_var <- pca02$sdev^2 / sum(pca02$sdev^2)

scores <- as.data.frame(pca02$x)

loadings <- as.data.frame(pca02$rotation)
loadings$var <- rownames(loadings)

# Create biplot
biplot <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_point(#aes(color = var),
    size = 2, shape = 19) +
  geom_segment(
    data = loadings, aes(
      x = 0, y = 0,
      xend = PC1 , yend = PC2
    ),
    arrow = arrow(length = unit(0.3, "cm"), type = "open", angle = 25),
    linewidth = 1, color = "darkblue"
  )
biplot

scale <- 300
# Create biplot
ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, linetype="dashed", col="grey")+
  geom_vline(xintercept = 0, linetype="dashed", col="grey")+
  geom_point(#aes(color = Species),
    size = 2, shape = 19) +
  geom_segment(
    data = loadings, aes(
      x = 0, y = 0,
      xend = PC1 * scale, yend = PC2 * scale
    ),
    arrow = arrow(length = unit(0.3, "cm"), type = "open", angle = 25),
    linewidth = 1, color = "darkblue"
  )+
  scale_x_continuous(
    #limits = c(-3, 3),
    name = paste0("PC1 (",round(prop_var[1]*100, digits = 2), " %)"),
    sec.axis = sec_axis(~ . / scale, name = "Loadings on PC1")
  ) +
  scale_y_continuous(
    #limits = c(-3, 3),
    name = paste0("PC2 (",round(prop_var[2]*100, digits = 2), " %)"),
    sec.axis = sec_axis(~ . / scale, name = "Loadings on PC2")
  ) +
  ggrepel::geom_label_repel(
    data = loadings,
    aes(
      label = rownames(loadings),
      x = PC1 * scale,
      y = PC2 * scale
    ),
    box.padding = 0.2,
    point.padding = 0.3,
    size = 3, # Change the font size of the text here
    color = "black", # Change the color of the text here
    arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 25),
    force = 4
  ) 
