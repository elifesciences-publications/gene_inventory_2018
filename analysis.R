# ---
# title: "Analyses"
# author: "Casey Dunn, Zack Lewis"
# date: "6/13/2018"
# output: html_document
# ---

# Data are not directly comparable. Both Fairclough2013 and Richter2018 rely on OrthoMCL, so they are the most comparable. Suga2013 rely on Pfam domains, so their numbers are much lower than other studies.

# dependencies

library(tidyverse)
library(reshape2)

D = read_csv("inventory_data.csv")

#remove King2008 data, because intron gain/loss not directly comparable
D = D %>% filter(!study %in% c("King2008"))

#rename nodes, if desired
D$node_renamed[D$node_renamed=="Opisthokonts"] <- "Opisthokonta"
D$node_renamed[D$node_renamed=="Choanoflagellates"] <- "Choanoflagellata"

#rename columns
D = dplyr::rename(D, 'gene_groups' = 'genes_total')

# gather
Dg = D %>% gather(`gene_groups`, `gained`, `lost`, key = "attribute", value = "n")

# remove NA
Filtered = Dg %>% filter(!is.na(node_renamed))

#rearranging ordering by nodes

order <- c("Opisthokonta", "Holozoa", "Choanozoa","Choanoflagellata", "Metazoa")

FilteredReorder = Filtered %>%
  mutate(node_renamed = factor(node_renamed, levels = order)) %>%
  arrange(node_renamed)

FilteredReorder = FilteredReorder %>% filter(!is.na(node_renamed))

countPlot <- FilteredReorder %>%
  ggplot() +
    #geom_point(aes(x=node_renamed, y = n, col =attribute, pch=study), size=2,stroke = 2) +
    geom_line(aes(group=interaction(study, attribute),  x = node_renamed, y = n, col = attribute)) +
    theme_classic() +
    guides(colour = guide_legend(title="Attribute")) +
    guides(pch = guide_legend(title="Study")) +
    labs(x = "Node", y = "N") +
    theme(legend.position = "right") +
    scale_shape_manual(values=c(0,1,2,5)) 

#layering points
countPlot <- countPlot +  geom_point(aes(x=node_renamed, y = n,  pch=study), col ="grey90", size=0,stroke = 3) +
  geom_point(aes(x=node_renamed, y = n, col =attribute, pch=study), size=3,stroke = 1)

# changing color order
countPlot <- countPlot + scale_color_manual(values=c("#00BA38","#619CFF", "#F8766D"))

#removing legend and changing angle
countPlotNoLegend<-countPlot + theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, size=10))
countPlotNoLegend

#save output
ggsave( "fig1B.pdf", countPlot, device=pdf, width=5, height=4 )
ggsave( "fig1B_noLegend.pdf", countPlotNoLegend, device=pdf, width=5, height=4 )


# scatterplot gains vs. losses

#remove NAs
D.noNA = D %>% filter(!is.na(node_renamed))


### with color and pch swapped

gainLossPlot <- D.noNA %>%
	ggplot() +
		#geom_point(aes(x=lost, y=gained, col=node_renamed, pch=study), na.rm = TRUE, size = 3,stroke=2) +
    scale_shape_manual(values=c(0,1,2,5)) +
    guides(colour = guide_legend(title="Node")) +
    guides(pch = guide_legend(title="Study")) +
    labs(x = "Lost", y = "Gained") +
    theme_classic() +
    geom_abline(intercept = 0, slope = 1, color="grey90", linetype="dashed", size=1, alpha=0.7) #+
    #scale_x_continuous(limits = c(0, 3700)) + scale_y_continuous(limits = c(0, 3700))

#layering points
gainLossPlot <- gainLossPlotNoScaleSwap + geom_point(aes(x=lost, y=gained, pch=study), col="grey90", na.rm = TRUE, size = 0,stroke=3) +
  geom_point(aes(x=lost, y=gained, col=D.noNA$node_renamed, pch=study), na.rm = TRUE, size = 3,stroke=1) 

gainLossPlot


ggsave( "fig1D_swap.pdf", gainLossPlotNoScaleSwap, device=pdf, width=5, height=4 )

gainLossPlotnoLegend<-gainLossPlotNoScaleSwap + theme(legend.position="none")
gainLossPlotnoLegend

ggsave( "fig1D_noLegend_swap.pdf", gainLossPlotnoLegend, device=pdf, width=5, height=4 )







