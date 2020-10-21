# Checking the sets of ambiguous species for D and S.



### 1. Checking species ####
ca_df <- read_excel("./data/ndm-outputs/Loose40/CA_Loose40_data.xlsx")

spp_D <- ca_df %>%
  filter(Spatial_Scale == 3, Analysis == "D") %>%
  dplyr::select(Species) %>%
  pull()

(dssp <- unique(spp_D[duplicated(spp_D)]))

spp_D[spp_D == "Bignonia costata"]

spp_S <- ca_df %>%
  filter(Spatial_Scale == 3, Analysis == "S") %>%
  dplyr::select(Species) %>%
  pull()

(sssp <- unique(spp_S[duplicated(spp_S)]))

sum(sssp %in% dssp)
sum(sssp %in% spp_D)
sum(dssp %in% spp_S)

sssp[!sssp %in% dssp] %in% dssp[!dssp %in% spp_S]
sssp[!sssp %in% dssp] %in% spp_D
sssp[!sssp %in% dssp] %in% dssp
  
# D1: "Adenocalymma sessile"  "Adenocalymma bullatum" "Bignonia costata" #3

# S1: #0 

# D2:  [1] "Adenocalymma bullatum"     "Fridericia formosa"        "Lundia obliqua"            "Tynanthus fasciculatus"    "Adenocalymma subspicatum" 
# [6] "Lundia helicocalyx"        "Adenocalymma ackermannii"  "Adenocalymma dichilum"     "Adenocalymma hypostictum"  "Lundia gardneri"          
# [11] "Xylophragma harleyi"       "Adenocalymma divaricatum"  "Fridericia erubescens"     "Pleonotoma stichadenia"    "Martinella insignis"      
# [16] "Anemopaegma citrinum"      "Cuspidaria lasiantha"      "Amphilophium bracteatum"   "Amphilophium dolichoides"  "Dolichandra unguiculata"  
# [21] "Adenocalymma magdalenense" "Tanaecium bilabiatum"      "Amphilophium blanchetii"   "Mansoa hirsuta"            "Adenocalymma dusenii"     
# [26] "Adenocalymma hatschbachii" "Tanaecium exitiosum"       "Bignonia neouliginosa"     "Pachyptera erythraea" #29

# S2:  [1] "Tanaecium neobrasiliense"  "Adenocalymma bullatum"     "Amphilophium frutescens"   "Fridericia subincana"      "Tanaecium bilabiatum"     
# [6] "Adenocalymma apetiolatum"  "Adenocalymma hirtum"       "Adenocalymma lineare"      "Adenocalymma grandifolium" "Anemopaegma heringeri"    
# [11] "Fridericia elegans" #11

# In S2 only 2/11 ambiguous species are also in D2 ambiguous species. 
# 11 ambiguous species from S2 are in the total set of species in D2.
# 9 new ambiguous species appeared in S2 that were not ambiguous in D2.
# 20 ambiguous species from D2 are in the total set of species of S2.
# This means 29 - 9 = 20 ambiguities were resolved from D to S, 2 remained, and 9 new cases of ambiguity emerged. 

# D3:  [1] "Adenocalymma schomburgkii"    "Amphilophium granulosum"      "Anemopaegma paraense"         "Fridericia nigrescens"       
# [5] "Fridericia prancei"           "Pleonotoma clematis"          "Adenocalymma divaricatum"     "Adenocalymma scabriusculum"  
# [9] "Fridericia dispar"            "Fridericia samydoides"        "Dolichandra steyermarkii"     "Cuspidaria pulchella"        
# [13] "Lundia virginalis"            "Pleonotoma tetraquetra"       "Xylophragma myrianthum"       "Amphilophium porphyrotrichum"
# [17] "Anemopaegma robustum"         "Bignonia microcalyx"          "Fridericia oligantha"         "Pleonotoma echitidea"        
# [21] "Pleonotoma jasminifolia"      "Adenocalymma dugandii"        "Amphilophium chocoense"       "Anemopaegma santaritense"    
# [25] "Anemopaegma ionanthum"        "Anemopaegma jucundum"         "Cuspidaria monophylla"        "Cuspidaria subincana"        
# [29] "Adenocalymma adenophorum"     "Amphilophium lohmanniae"      "Anemopaegma foetidum"         "Anemopaegma mirabile"        
# [33] "Cuspidaria argentea"          "Cuspidaria cratensis"         "Fridericia limae"             "Mansoa hirsuta"              
# [37] "Pleonotoma orientalis"        "Tanaecium paradoxum"          "Adenocalymma impressum"       "Fridericia tuberculata"      
# [41] "Adenocalymma paulistarum"     "Cuspidaria convoluta"         "Tynanthus micranthus"         "Fridericia arthrerion"       
# [45] "Pleonotoma pavettiflora"      "Tanaecium xanthophyllum"      "Mansoa glaziovii"             "Tynanthus cognatus"          
# [49] "Adenocalymma cidii"           "Amphilophium laeve"           "Amphilophium steyermarkii"    "Anemopaegma villosum"        
# [53] "Fridericia carichanensis"     "Amphilophium parkeri"         "Fridericia floribunda"        "Bignonia bracteomana"        
# [57] "Fridericia pearcei"           "Lundia spruceana"             "Tanaecium affine"             "Bignonia potosina"           
# [61] "Fridericia viscida" #61

# S3:  [1] "Adenocalymma cymbalum"     "Anemopaegma album"         "Cuspidaria lasiantha"      "Lundia gardneri"           "Pleonotoma stichadenia"   
# [6] "Tanaecium neobrasiliense"  "Xylophragma harleyi"       "Cuspidaria octoptera"      "Lundia virginalis"         "Pleonotoma tetraquetra"   
# [11] "Cuspidaria pulchella"      "Dolichandra unguiculata"   "Fridericia speciosa"       "Lundia obliqua"            "Mansoa glaziovii"         
# [16] "Amphilophium bracteatum"   "Adenocalymma coriaceum"    "Anemopaegma citrinum"      "Xylophragma corchoroides"  "Adenocalymma bullatum"    
# [21] "Fridericia formosa"        "Amphilophium dolichoides"  "Cuspidaria argentea"       "Cuspidaria cratensis"      "Pleonotoma variabilis"    
# [26] "Adenocalymma hirtum"       "Amphilophium frutescens"   "Fridericia tynanthoides"   "Fridericia pearcei"        "Lundia spruceana"         
# [31] "Adenocalymma paulistarum"  "Tynanthus micranthus"      "Adenocalymma magdalenense" "Bignonia pterocalyx"       "Pachyptera erythraea"     
# [36] "Tanaecium exitiosum"       "Adenocalymma bracteatum"   "Bignonia uleana"           "Adenocalymma dusenii"      "Adenocalymma hatschbachii"
# [41] "Fridericia pliciflora" #41

# In S3 only 
# 41/41 ambiguous species from S3 are in the total set of species in D3.
# Therefore, 31 ambiguous species from S3 are part of the non-ambiguous species of D3.
# This means 31 ambiguities were resolved from D to S, but 10 remained. 

# In S3 only 10/41 ambiguous species are also in D3 ambiguous species set.  
# 41/41 ambiguous species from S3 are in the total set of species in D2.
# 61 - 41 = 20 fewer ambiguous species from D to S. But with only 10 shared species, 31 ambiguos spp in S and 51 ambiguous spp in D are different!
# 33 ambiguous species from D2 are in the total set of species of S3.
# This means 61 - 33 = 28 ambiguities were resolved from D to S, 10 remained, and 31 new cases of ambiguity emerged. 


### 2. Drawing VennDiagram ####
library(VennDiagram)

ca_df <- read_excel("./data/ndm-outputs/Loose40/CA_Loose40_data.xlsx")

spp_D <- ca_df %>%
  filter(Spatial_Scale == 3, Analysis == "D") %>%
  dplyr::select(Species) %>%
  pull()

(dssp <- unique(spp_D[duplicated(spp_D)]))

spp_D[spp_D == "Bignonia costata"]

spp_S <- ca_df %>%
  filter(Spatial_Scale == 3, Analysis == "S") %>%
  dplyr::select(Species) %>%
  pull()

(sssp <- unique(spp_S[duplicated(spp_S)]))


venDplot <- venn.diagram(x = list(dssp, sssp),
             category.names = c("D" , "S"),
             col = viridis::viridis(option = "E", end = 0.7, n = 2),
             filename = "./figs/NEWMANFIG/AmbiguousSPP_vennD_3dg.png",
             main = "3°",
             main.cex = 7,
             output = TRUE ,
             imagetype="png" ,
             height = 800 , 
             width = 800 , 
             resolution = 120,
             compression = "lzw",
              lwd = 5,
            #  
            #  #fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
             cex = 3,
             # fontfamily = "sans",
             cat.cex = 4
            #  # cat.fontfamily = "sans",
            # #  cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
            #  rotation = 1
)

### 3. Looking at the effect of S in Ambiguity ####

Effect_ambDS <- data.frame(Spatial_scale = c(1, 2, 3),
                           Total_D = c(39, 166, 281),
                           Total_S = c(23, 103, 203),
                           AmbsppD = c(3, 29, 61), 
                           AmbsppS = c(0, 11, 41),
                           Resolved_spp = c(3, 20, 28)) %>%
  mutate(Total_Dperc = (Total_D / Total_D) * 100,
         LostDS = Total_D - Total_S,
         Amb_tot = (AmbsppD / Total_D) * 100,
         Amb_solved = (Resolved_spp / Total_D) * 100,
         Amb_notsolved = Amb_tot - Amb_solved,
         CostinD = (LostDS / Total_D) * 100,
         Amb_remain = (AmbsppS / Total_S) * 100)

Effect_ambDS <- format(Effect_ambDS, digits = 4)

write.csv(Effect_ambDS , "./output/NEW_MANOUT/Ambiguityperc.csv")


Effect_ambDS %>%
  pivot_longer(cols = c(Total_D, Total_S, AmbsppD, AmbsppS, Resolved_spp, LostDS), 
               names_to = "Class", values_to = "Spp_number") %>%
  ggplot(aes(x = Class, y = Spp_number)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Spatial_scale) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#### 4. Ambiguity across scales in D ####

spp_D1 <- ca_df %>%
  filter(Spatial_Scale == 1, Analysis == "D") %>%
  dplyr::select(Species) %>%
  pull()

(dssp1 <- unique(spp_D1[duplicated(spp_D1)]))

spp_D2 <- ca_df %>%
  filter(Spatial_Scale == 2, Analysis == "D") %>%
  dplyr::select(Species) %>%
  pull()

(dssp2 <- unique(spp_D2[duplicated(spp_D2)]))

spp_D3 <- ca_df %>%
  filter(Spatial_Scale == 3, Analysis == "D") %>%
  dplyr::select(Species) %>%
  pull()

(dssp3 <- unique(spp_D3[duplicated(spp_D3)]))

length(unique(c(dssp1, dssp2, dssp3)))
