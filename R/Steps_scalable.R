library(raster)
library(rgdal)
library(maps)
library(ggplot2)

## Loading Layers

LatAM <- readOGR(dsn = "./data/GIS/", layer = "continent")
MORR <- readOGR(dsn = "./data/GIS/", layer = "Lowenberg_Neto_2014") # Morrone's 2014 Neotropical Region
A_1dgDCA0 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA0_AF-grid")
A_1dgDCA1 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA1_AM-grid")
A_1dgDCA2 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA2_AF-grid")
A_1dgDCA3 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA3_AF-grid")
A_1dgDCA4 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA4_AM-grid")
A_1dgDCA5 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA5_AF-grid")
A_1dgDCA8 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA8_AF-grid")
A_1dgDCA6 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA6_DD-grid")
A_1dgDCA7 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA7_AM-grid")
A_1dgDCA9 <- readOGR(dsn = "./output/GIS/40perc/", layer = "1dgD_CA9_AF-grid")

## Ambiguity group 1: Eastern South America

A_2dgDCA0 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA0_AF-grid")
A_2dgDCA5 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA5_DD-grid")
A_2dgDCA19 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA19_DD-grid")
A_2dgDCA8 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA8_AF-grid")
A_2dgDCA15 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA15_AF-grid")
A_2dgDCA9 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA9_AF-grid")
A_2dgDCA6 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA6_AFDD-grid")
A_2dgDCA13 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA13_AF-grid")
A_2dgDCA1 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA1_AF-grid")
A_2dgDCA2 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA2_DD-grid")
A_2dgDCA21 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA21_AF-grid")

## Ambiguity group 2: Northern South America
A_2dgDCA17 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA17_NWCol-grid")
A_2dgDCA22 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA22_NWCol2-grid")
A_2dgDCA14 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA14_NWColVen-grid")
A_2dgDCA26 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA26_NWCol3-grid")

## Ambiguity group 3: Amazonia
A_2dgDCA7 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA7_AM-grid")
A_2dgDCA18 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA18_AM-grid")

## Without Ambiguity

A_2dgDCA3 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA3_AM-grid")
A_2dgDCA4 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA4_AM-grid")
A_2dgDCA10 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA10_AM-grid")
A_2dgDCA11 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA11_THDD-grid")
A_2dgDCA12 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA12_NWYuc-grid")
A_2dgDCA16 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA16_AM-grid")
A_2dgDCA20 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA20_AM-grid")
A_2dgDCA23 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA23_AM-grid")
A_2dgDCA24 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA24_AF-grid")
A_2dgDCA25 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA25_AM-grid")
A_2dgDCA27 <- readOGR(dsn = "./output/GIS/40perc/", layer = "2dgD_CA27_DDb-grid")

# Ambiguity group 1: Eastern South America & Dry Diagonal

A_3dgDCA2 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA2_THDD-grid")
A_3dgDCA5 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA5_SESouth-grid")
A_3dgDCA10 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA10_AF-grid")
A_3dgDCA19 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA19_AF-grid")
A_3dgDCA23 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA23_AF-grid")
A_3dgDCA16 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA16_DD-grid")
A_3dgDCA4 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA4_DD-grid")

# Ambiguity group 2: Northern Soutn America

A_3dgDCA8 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA8_NWPac-grid")
A_3dgDCA13 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA13_NWCol-grid")
A_3dgDCA29 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA29_NWVen-grid")
A_3dgDCA6 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA6_AM-grid")
A_3dgDCA11 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA11_AM-grid")

# Ambiguity group 3: Amazonia

A_3dgDCA3 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA3_AM-grid")
A_3dgDCA0 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA0_AM-grid")
A_3dgDCA12 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA12_AM-grid")
A_3dgDCA30 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA30_AM-grid")
A_3dgDCA14 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA14_AM-grid")
A_3dgDCA15 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA15_AM-grid")
A_3dgDCA17 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA17_AM-grid")
A_3dgDCA24 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA24_AM-grid")
A_3dgDCA7 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA7_AM-grid")
A_3dgDCA20 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA20_AM-grid")
A_3dgDCA32 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA32_AM-grid")
A_3dgDCA28 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA28_AM-grid")
A_3dgDCA9 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA9_AM-grid")

# Ambiguity group 4: Mesoamerica and Central America

A_3dgDCA25 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA25_NWMeso-grid")
A_3dgDCA33 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA33_NWMeso3-grid")
A_3dgDCA31 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA31_NWMeso2-grid")

## Without Ambiguity

A_3dgDCA18 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA27_THDD-grid")
A_3dgDCA26 <- readOGR(dsn = "./output/GIS/40perc/", layer = "3dgD_CA26_AM-grid")



## Scalable patterns
pdf("./figs/NEWMANFIG/Appendix_ScaleRelations_TOTAL.pdf", height = 10, width = 10, paper = "a4")
par(mfrow = c(4, 3), mar=c(2,2,2,2), new = TRUE)

# Showin spatial relationships between areas at differen t scales
##___________________________________
# Scalable 1 (52): D2CA12, D3CA25

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA25, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA12, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "1. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA12", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA25", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 2 (53): D2CA12, D3CA31

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA31, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA12, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "2. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA12", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA31", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 3 (54): D2CA14, D3CA8

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA8, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA14, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "3. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA14", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA8", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 4 (55): D2CA17 D3CA8

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA8, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA17, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "4. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA14", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA8", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 5 (56): D2CA22 D3CA8

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA8, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA22, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "5. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA22", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA8", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 6 (57): D2CA22 D3CA13

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA13, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA22, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "6. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA22", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA13", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 7 (58): D2CA26, D3CA8

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA8, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA26, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "7. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA26", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA8", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 8 (32): D1CA1, D2CA25, D3CA11

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA11, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA25, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA1, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "8. Complete nested", cex.main = 1)
legend(x = -70, 40, legend = "D1CA1", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA25", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA11", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 9 (33): D1CA4, D2CA10, D3CA30

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA30, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA10, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA4, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "9. Complete nested", cex.main = 1)
legend(x = -70, 40, legend = "D1CA4", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA10", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA30", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 10 (35): D2CA3, D3CA6

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA6, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA3, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "10. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA3", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA6", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 11 (36): D2CA3, D3CA11

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA11, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA3, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "11. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA3", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA11", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 12 (39): D2CA10, D3CA6

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA6, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA10, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "12. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA10", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA6", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 13 (40): D2CA10, D3CA11

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA11, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA10, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "13. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA10", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA11", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 14 (41): D2CA10, D3CA30

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA30, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA10, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "14. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA10", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA30", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 15 (34): D1CA7, D2CA23, D3CA14

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA14, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA23, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA7, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "15. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA7", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA23", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA314", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 16 (37): D2CA4, D3CA26

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA26, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA4, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "16. Identical", cex.main = 1)
legend(x = -70, 35, legend = "D2CA4", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA26", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 17 (38): D2CA7, D3CA0

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA0, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA7, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "17. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA7", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA0", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

#___________________________________
# Scalable 18 (42): D2CA16 D3CA28

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA28, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA16, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "18. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA16", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA28", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 19 (43): D2CA16, D3CA32

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA32, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA16, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "19. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA16", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA32", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 20 (44): D2CA18, D3CA0

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA0, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA18, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "20. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA18", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA30", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 21 (45): D2CA18, D3CA3

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA3, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA18, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "21. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA18", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA3", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 22 (46): D2CA20, D3CA20

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA20, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA20, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "22. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA20", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA20", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 23 (47): D2CA20, D3CA32

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA32, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA20, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "23. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA20", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA32", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 24 (48): D2CA23, D3CA14

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA14, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA23, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "24. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA23", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA14", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 25 (49): D2CA25, D3CA11

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA11, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA25, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "25. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA25", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA11", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 26 (50): D2CA25, D3CA29

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA29, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA25, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "26. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA25", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA29", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 27 (51): D2CA27, D3CA18 !!!!

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA18, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA27, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "27. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA27", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA18", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 28 (1): D1CA0, D2CA0, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA0, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "28. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA0", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 29 (2): D1CA0, D2CA1, D3CA5
plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA1, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA0, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "29. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA0", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA1", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 30 (3): D1CA2, D2CA0, D3CA5
plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA2, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "30. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA2", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 31 (4): D1CA3, D2CA0, D3CA5
plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA3, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "31. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA3", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)


##___________________________________
# Scalable 32 (5): D1CA5, D2CA0, D3CA5
plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA5, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "32. Incomplete nested: lost", cex.main = 1)
legend(x = -70, 40, legend = "D1CA5", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)


##___________________________________
# Scalable 33 (6): D1CA5, D2CA1, D3CA5
plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA1, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA5, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "33. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA5", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA1", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 34 (7): D1CA6, D2CA5, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA5, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA6, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "34. Incomplete nested: lost", cex.main = 1)
legend(x = -70, 40, legend = "D1CA6", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA5", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 35 (8): D1CA8, D2CA0, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA8, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "35. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA8", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 36 (9): D1CA8, D2CA1, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA1, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA8, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "36. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA8", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA1", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 37 (10): D1CA9, D2CA0, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA9, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "37. Incomplete nested: lost", cex.main = 1)
legend(x = -70, 40, legend = "D1CA9", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 38 (11): D1CA9, D2CA8, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA8, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_1dgDCA9, add = TRUE,  col = alpha("#3604ff", 0.4), lty = 0)
map.axes()
title(main = "38. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 40, legend = "D1CA9", fill = alpha("#3604ff", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 35, legend = "D2CA8", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 39 (12): D2CA0, D3CA4

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_3dgDCA4, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
map.axes()
title(main = "39. Reverse nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA4", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 40 (13): D2CA0, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "40. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 41 (14): D2CA0, D3CA10

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA10, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "41. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA10", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)


##___________________________________
# Scalable 42 (15): D2CA0, D3CA16

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_2dgDCA0, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
plot(A_3dgDCA16, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
map.axes()
title(main = "42. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA0", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA16", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 43 (16): D2CA1, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA1, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "43. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA1", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 44 (17): D2CA1, D3CA10

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA10, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA1, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "44. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA1", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA10", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 45 (18): D2CA2, D3CA16

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA16, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA2, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "45. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA2", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA16", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 46 (19): D2CA5, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA5, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "46. Incomplete nested: lost", cex.main = 1)
legend(x = -70, 35, legend = "D2CA5", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 47 (20): D2CA6, D3CA4

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA4, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA6, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "47. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA6", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA4", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 48 (21): D2CA6, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA6, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "48. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA6", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 49 (22): D2CA8, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA8, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "49. Incomplete nested: lost", cex.main = 1)
legend(x = -70, 35, legend = "D2CA8", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 50 (23): D2CA9, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA9, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "50. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA9", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 51 (24): D2CA13, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA13, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "51. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA13", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 52 (25): D2CA15, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA15, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "52. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA15", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 53 (26): D2CA15, D3CA23

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA23, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA15, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "53. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA15", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA23", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 54 (27): D2CA19, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA19, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "54. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA19", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 55 (28): D2CA19, D3CA16

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA16, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA19, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "55. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA19", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA16", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 56 (29): D2CA21, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA21, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "56. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA21", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 57 (30): D2CA24, D3CA5

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA5, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA24, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "57. Incomplete nested: combination", cex.main = 1)
legend(x = -70, 35, legend = "D2CA24", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA5", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 58 (31): D2CA24, D3CA19

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA19, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA24, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "58. Complete nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA24", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA19", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

##___________________________________
# Scalable 59: D2CA11, D3CA2

plot(LatAM, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.3)
plot(MORR, add = TRUE, xlim = c(-117, -34), ylim = c(-37, 39), lwd = 0.1)
plot(A_3dgDCA2, add = TRUE, col = alpha("#4fff04", 0.4), lty = 0)
plot(A_2dgDCA11, add = TRUE, col = alpha("#ff04cd", 0.4), lty = 0)
map.axes()
title(main = "59. Reverse Nested", cex.main = 1)
legend(x = -70, 35, legend = "D2CA11", fill = alpha("#ff04cd", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)
legend(x = -70, 30, legend = "D3CA2", fill = alpha("#4fff04", 0.4), 
       border = "white", horiz = T, bty = "n", x.intersp = 0.5)

dev.off()
