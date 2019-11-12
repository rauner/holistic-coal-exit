#disaggregate the LCA to iso country and get dalys and cost from AP table results

# disaggregate the ap dalys to sectors with LCA ap ratio

#read lca results, read talbe ap results

# to-dos: 1. add aufl?sung auf category level

library(dplyr)
library(tidyr)
library(magpie)
library(moinput)

wd_dir <- "C:/Users/rauner/Documents/PIK/fasstr_sebastian"
setwd(wd_dir)

dir_root           <- paste0(getwd()    ,'/mnt/FASST_IDL_PACK/')              # main folder
dir_ancil  <- paste0(dir_root,  'ANCILLARY')                       # ancillary data
dir_DALYS  <- paste0(dir_ancil, '/DALYS')                          # DALYS
#dir_outtab <- paste0(dir_root,  'OUTPUT/TABLES/enavi/')           # output (tables)
dir_outtab <- paste0(dir_root,  'OUTPUT/TABLES/')           # output (tables)
dir_lca   <- paste0('C:/Users/rauner/Documents/PIK/holi_coal_exit/rmnd-lca/results/')            # main folder
dir_plot_out       <- paste0(getwd(),  '/plots/')                              # output


#scenarios
scenario_files <- list.files(path = dir_outtab, pattern = 'results_dalys_SSP2')
lca_files      <- list.files(path = dir_lca, pattern = 'lca_REMIND_monetization_coal_exit_end_points')

end_category <- c('ED_H', 'HH_H', 'RA_H')
end_category_clear <- c('Ecosystem Damages [species.yr]',
                        'Human Health [DALY]',
                        'Ressource Depletion [US$2005]')


category <- c('CC',
              'OD',
              'TA',
              'FE',
              'HT',
              'POF',
              'PMF',
              'TET',
              'FET',
              'MET',
              'IR',
              'ALO',
              'ULO',
              'NLT',
              'FD',
              'MD',
              'ME',
              'WD')

category_clear <- c('Climate change',
                    'Ozone depletion',
                    'Terrestrial acidification',
                    'Freshwater eutrophication',
                    'Human toxicity',
                    'Photochemical oxidant formation',
                    'Air Pollution',
                    'Terrestrial ecotoxicity',
                    'Freshwater ecotoxicity',
                    'Marine ecotoxicity',
                    'Ionising radiation',
                    'Agricultural land occupation',
                    'Urban land occupation',
                    'Natural land transformation',
                    'Fossil depletion',
                    'Metal depletion',
                    'Marine eutrophication',
                    'Water depletion')

# read and rbind tab results
ap <- do.call(rbind,
        lapply(paste0(dir_outtab,scenario_files), read.csv,sep=';'))

#read the lca results
lca <- read.csv(paste0(dir_lca,lca_files[1]), sep=';')

#add a total cost column
#lca$cost_total_vsl <- lca$ED_H_cost_total_vsl + lca$RA_H_cost_total_vsl  + lca$HH_H_cost_total_vsl

#filter the LCA impact
lca <- lca %>% select(c('scenario','period','region','category','end_category','tech','lca_impact')) 
#year
year <- unique(ap$year)
lca <- lca %>% filter(period %in% year)
lca <- as.magpie(lca )






#region mapping
region_mapping <- read.csv('regionmappingREMIND.csv', sep =';')

#pop as weights
pop                           <- calcOutput("Population",     years=year, aggregate = F ) 
pop=pop[,,"SSP2",pmatch=TRUE]
pop=pop[,2100,,invert=TRUE]
lca = speed_aggregate(lca, rel =  region_mapping, weight = pop )

lca = quitte::as.quitte(lca)

# does that really make sense here? its more a loss of ssectoral information.

#filter the AP endpoint of the LCA and make a relation of the techs out of it
# distribute the AP total dalys to the techs with this relation
lca_pmf <- lca %>% filter(category == 'PMF')

#add a total row
lca_pmf_total= lca_pmf %>% 
               group_by(scenario, region, variable, unit,  period) %>% 
               summarise(total = sum(value))
# join lca_pmf and total
lca_pmf <- dplyr::left_join(lca_pmf, lca_pmf_total)

lca_pmf$value <- lca_pmf$value / lca_pmf$total

#add the ap total dalys instead of PMF and multiply with ratio of tech to total
# join by scenario, year, iso
lca_pmf <- dplyr::left_join(lca_pmf, lca_pmf_total)

#filter only for dalys column of ap
ap_dalys <- ap %>% select('scenario','year','iso.iso.y', 'daly_total')

#match lca_pmf and ap scenario names
#multiply the ap value with the lca_pmf value(ratio sector to total)
# there should be a bug
lca_pmf <- dplyr::left_join(lca_pmf, ap_dalys, by =c('scenario'='scenario',
                                               'period'  ='year',
                                               'region'  ='iso.iso.y'))
lca_pmf$value = lca_pmf$value * lca_pmf$daly_total
lca_pmf <- lca_pmf %>% select(-daly_total, -total)


# add this to the lca insted of particulate matter formation
# filter lca for non pmf and rbind lca and lca_pmf (adjusted with our ap results)

lca <- lca %>% filter(category != 'PMF' )

lca <- rbind(lca, lca_pmf)

#make some REMIND region plots out of that
lca_remind <- dplyr::left_join(lca, region_mapping, by =c('region'='CountryCode'))

# add end_category and end_category_tech totals
# add category totals

#this total does not include the AP
lca_remind <- lca_remind %>% filter(category != 'total')

lca_remind <- lca_remind %>% select(-variable, -unit, -model)
lca_remind[lca_remind == NA] <- 0

lca_remind$category     <- plyr::mapvalues(lca_remind$category, from = category,
                                           to = category_clear)

lca_remind$end_category <- plyr::mapvalues(lca_remind$end_category, from = end_category,
                                           to = end_category_clear)

write.csv(lca_remind,paste0(dir_outtab,'environmental_indicators.csv'))
#add some different totals
lca_remind_end_category_tech_total <- na.omit(lca_remind) %>% 
                                      group_by(scenario, region, period,tech, end_category) %>% 
                                      summarise(value = sum(value))

lca_remind_end_category_total <- na.omit(lca_remind) %>% 
  group_by(scenario, region, period, end_category) %>% 
  summarise(value = sum(value))

lca_remind_end_category_category_total <- na.omit(lca_remind) %>% 
  group_by(scenario, region, period, category, end_category) %>% 
  summarise(value = sum(value))

#loop over end categories since they have different scales
#loop over sectors?

tech = unique(lca_remind$tech)
end_category = unique(lca_remind$end_category)
category = unique(lca_remind$category)
# add a sector total
#aggregate( df[,11:200], df[,1:10], FUN = sum )

#add an end_category total
x<-1
#sectoral category
for(cat in category){
  lca_plot <- lca_remind %>% 
    filter(category == cat)
  
  p <- ggplot(data=lca_plot, aes(x=period, y=value, group = scenario, fill = scenario)) +
    labs(title = paste(cat))+
    geom_bar(stat='identity',position = "dodge")  +
    facet_wrap(~tech) +
    theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(filename=paste0(dir_plot_out,"lca_sectoral_category_",cat,".pdf"),p,scale=1,width=25,height=25,unit="cm")
  x<-x+1
}



x<-1

# sector midpoints
for(sector in tech){
lca_plot <- lca_remind %>%
  #filter(period == as.integer('2050')) %>%
  filter(tech == sector)%>%
  filter(category != 'total')

p <- ggplot(data=lca_plot, aes(x=period, y=value, group = scenario, fill = scenario)) +
     labs(title = paste(sector))+
     geom_bar(stat="identity", position = "dodge")  +
     facet_wrap(~category, scales = "free_y") +
     theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste0(dir_plot_out,"lca_sectoral_",x,".pdf"),p,scale=1,width=25,height=25,unit="cm")
x<-x+1
}

x<-1

#sector endpoints
for(sector in tech){
  lca_plot <- lca_remind_end_category_tech_total %>% 
    #filter(period == as.integer('2050')) %>%
    filter(tech == sector)
  
  p <- ggplot(data=lca_plot, aes(x=period, y=value, group = scenario, fill = scenario)) +
    labs(title = paste(sector))+
    geom_bar(stat="identity", position = "dodge")  +
    facet_wrap(~end_category, scales = "free_y") +
    theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(filename=paste0(dir_plot_out,"lca_end_category_total_",x,".pdf"),p,scale=1,width=25,height=25,unit="cm")
  x<-x+1
}
x<-1
#total category
for(end in end_category){
  lca_plot <- lca_remind_end_category_category_total %>% 
    filter(end_category == end)
  
  p <- ggplot(data=lca_plot, aes(x=period, y=value, group = scenario, fill = scenario)) +
    labs(title = paste(end))+
    geom_bar(stat="identity", position = "dodge")  +
    facet_wrap(~category, scales = "free_y") +
    theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(filename=paste0(dir_plot_out,"lca_end_category_category_",x,".pdf"),p,scale=1,width=25,height=25,unit="cm")
  x<-x+1
}

#total category

  lca_plot <- lca_remind_end_category_total
  
  p <- ggplot(data=lca_plot, aes(x=period, y=value, group = scenario, fill = scenario)) +
    geom_bar(stat="identity", position = "dodge")  +
    facet_wrap(~end_category, scales = "free_y") +
    theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(filename=paste0(dir_plot_out,"lca_remind_end_category_total.pdf"),p,scale=1,width=25,height=25,unit="cm")

#same script for cost!


