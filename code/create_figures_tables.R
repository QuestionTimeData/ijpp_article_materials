
# Packages ----------------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(showtext)
library(ggthemes)
library(colorspace)
library(patchwork)
library(gt)
library(Cairo)
library(openxlsx)


# Load fonts from google.fonts.com
font_add_google("Arimo", "arimo")
showtext_auto()




# Setup -------------------------------------------------------------------


# Create function to calculate group percentages for each values
group_pct <- function(data){
  
  data |> 
    mutate(pct = round((n / sum(n) * 100), 0),
           pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
           guests = paste0('\n(N = ', sum(n), ')')) 
}



# Custom plot theme
custom_theme <-   ggthemes::theme_tufte(base_family = 'arimo') +
  theme(plot.title   = element_text(vjust= 0, hjust = .5),
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 0.5, angle = 0),
        legend.position = 'top',
        legend.justification = 'center',
        legend.direction = "horizontal",
        legend.key.size = unit(.9, "lines"),    
        legend.box.spacing = unit(0, "cm"),
        legend.box.margin = margin(0, 0, 0, 0, "cm"), 
        axis.ticks.length.x = unit(.25, "cm"),
        axis.ticks   = element_line(colour = 'grey60'),
        plot.margin  = margin(0,0.5,0,0, "cm"),
        text         = element_text(size = 14),
        axis.text    = element_text(size = 14),
        legend.text  = element_text(size = 14))


# Plot output text size
text_size <- 28


# Set plot colour palettes
sex_pal     <- c('#66B8EB', '#F04932')
eth_grp_pal <- c('grey85',            '#66B8EB',           '#F04932')
pri_pal     <- c('grey85',            '#66B8EB', '#9952CC')
oxb_pal     <- c('grey85', '#FFBE23', '#66B8EB', '#9952CC')

iraq_pal <- c('#45CB85', '#B95F89',  'grey80')
lr_pal   <- c('goldenrod1', '#443A83FF', '#35B779FF', 'grey70')
cat_pal  <- c('goldenrod1', '#5DC863FF', '#21908CFF', '#3B528BFF', '#440154FF', 'grey70')






# Import Data -------------------------------------------------------------


# All BBCQT appearances between 2001 and 2019
qt_0119 <- readRDS(here('data', 'wd_bbcqt_0119_v01_2024-09-22.rds'))
         






# 01.Figure 1 - Demographics ------------------------------------------------------------



## Prepare data for plotting

# Calculate % for each group by year

qt_0119_sex_all <- qt_0119 |> 
  group_by(year) |> 
  summarise(year_apps = n(),
            Male = sum(sex == 0)/n(),
            Female = sum(sex == 1)/n()) |> 
  pivot_longer(c(Male, Female), names_to = 'sex', values_to = 'pct') |> 
  mutate(sex = factor(sex, levels = c('Male', 'Female'))) 


qt_0119_eth_grp_all <- qt_0119 |> 
  group_by(year) |> 
  summarise(year_apps = n(),
             White = sum(eth_grp == 'White')/n(),
            `Ethnic minority` = sum(eth_grp == 'Ethnic minority')/n(),
            `No information` = sum(eth_grp == 'No information')/n())|> 
  pivot_longer(c(White, `Ethnic minority`, `No information`), names_to = 'eth_grp', values_to = 'pct')


qt_0119_ed_priv_all <- qt_0119 |> 
  group_by(year) |> 
  summarise(year_apps = n(),
            `State educated`     = sum(ed_priv_lab == 'State educated')/n(),
            `Privately educated` = sum(ed_priv_lab == 'Privately educated')/n(),
            `No information`     = sum(ed_priv_lab == 'No information')/n())|> 
  pivot_longer(c(`State educated`, `Privately educated`, `No information`), names_to = 'ed_priv', values_to = 'pct')


qt_0119_ed_oxb_all <- qt_0119 |> 
  group_by(year) |> 
  summarise(year_apps = n(),
            `Oxford/Cambridge` = sum(ed_oxb_lab == 'Oxford/Cambridge')/n(),
            `Other university` = sum(ed_oxb_lab == 'Other university')/n(),
            `Did not attend`   = sum(ed_oxb_lab == 'Did not attend')/n(),
            `No information`   = sum(ed_oxb_lab == 'No information')/n())|> 
  pivot_longer(c(`Oxford/Cambridge`, `Other university`, `Did not attend`, `No information`), names_to = 'ed_oxb', values_to = 'pct')







## Create initial plots

sex_all_plot <- qt_0119_sex_all |> 
  ggplot(aes(x = year, y = pct, fill = sex)) + 
  geom_bar(stat = "identity", width = 1, colour = 'white',
    linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0,1, 0.25), 
                     labels = scales::percent, 
                     expand = c(0.01,0)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2015, 2019),
                    labels = c(2001, 2005, 2010, 2015, 2019),
                    expand = c(0,0)) +
  labs(x = '', y = '', title = 'Sex') +
  geom_hline(yintercept = seq(0, 1, .25), 
             linetype = 'solid', 
             colour = 'white',
             alpha = 0.25,
             linewidth = .2) +
custom_theme +
   scale_fill_manual(values = sex_pal) 


eth_all_grp_plot <- qt_0119_eth_grp_all |> 
  mutate(eth_fac = factor(eth_grp, levels = rev(c('Ethnic minority', 'White', 'No information')))) |> 
  ggplot(aes(x = year, y = pct, fill = eth_fac)) + 
  geom_bar(stat = "identity", width = 1, colour = 'white',
    linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0,1, 0.25), 
                     labels = scales::percent, 
                     expand = c(0.01,0)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2015, 2019),
                    labels = c(2001, 2005, 2010, 2015, 2019),
                    expand = c(0,0)) +
  labs(x = '', y = '', title = 'Ethnicity') +
  geom_hline(yintercept = seq(0, 1, .25), 
             linetype = 'solid', 
             colour = 'white',
             alpha = 0.25,
             linewidth = .2) +
custom_theme +
  guides(fill = guide_legend(nrow=2, byrow=TRUE, reverse = F)) +
     scale_fill_manual(values = eth_grp_pal) 



ed_priv_all_plot <- qt_0119_ed_priv_all |> 
  mutate(ed_priv_fac = factor(ed_priv, levels = rev(c('Privately educated', 'State educated', 'No information')))) |> 
  ggplot(aes(x = year, y = pct, fill = ed_priv_fac)) + 
  geom_bar(stat = "identity", width = 1, colour = 'white',
    linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0,1, 0.25), 
                     labels = scales::percent, 
                     expand = c(0.01,0)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2015, 2019),
                    labels = c(2001, 2005, 2010, 2015, 2019),
                    expand = c(0,0)) +
  labs(x = '', y = '', title = 'Education (School)') +
  geom_hline(yintercept = seq(0, 1, .25), 
             linetype = 'solid', 
             colour = 'white',
             alpha = 0.25,
             linewidth = .2) +
custom_theme +
  guides(fill = guide_legend(nrow=2, byrow=TRUE, reverse = F)) +
     scale_fill_manual(values = pri_pal) 




ed_oxb_all_plot <- qt_0119_ed_oxb_all |> 
  mutate(ed_oxb_fac = factor(ed_oxb, levels = rev(c('Oxford/Cambridge', 'Other university', 'Did not attend', 'No information')))) |> 
  ggplot(aes(x = year, y = pct, fill = ed_oxb_fac)) + 
  geom_bar(stat = "identity", width = 1, colour = 'white',
    linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0,1, 0.25), 
                     labels = scales::percent, 
                     expand = c(0.01,0)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2015, 2019),
                    labels = c(2001, 2005, 2010, 2015, 2019),
                    expand = c(0,0)) +
  labs(x = '', y = '', title = 'Education (University)') +
  geom_hline(yintercept = seq(0, 1, .25), 
             linetype = 'solid', 
             colour = 'white',
             alpha = 0.25,
             linewidth = .2) +
custom_theme +
  guides(fill = guide_legend(nrow=2, byrow=TRUE, reverse = F)) +
     scale_fill_manual(values = oxb_pal ) 







## Overlay population data over the plots

# Set aesthetics for population data

bl_linewidth <- .6
bl_col   <- 'white'
bl_line  <- 'solid'
bl_line_public <- 'dashed'
bl_alpha <- 1



# Now for each plot, we can overlay the population percentages for the UK public
# And for each parliament between 2001-2019
# values are manually inserted, full source information included in supplementary materials file


# Sex
sex_all_bl <- sex_all_plot +
  geom_hline(aes(yintercept = .51), 
                 linetype = bl_line_public,
                 colour = bl_col,
                 alpha = bl_alpha,
                 linewidth = bl_linewidth) +
  geom_segment(aes(y = .18, yend =.18,
                   x = 2001,  xend = 2010),   # 97-10
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth) +
  geom_segment(aes(y = .22, yend =.22,
                   x = 2010,  xend = 2015),   # 10-15
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)   +
  geom_segment(aes(y = .31, yend =.31,
                   x = 2015,  xend = 2019),   # 15-19
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)  


# Ethnicity

# Census data for plotting points
dat <- data.frame(x = c(2001, 2011, 2019), y = c(.08, .13, .18))


eth_all_bl <- eth_all_grp_plot +
  geom_point(data = dat, 
             (aes(x = x, y = y)), 
                  fill = 'white',
                  colour = 'white',
                  size = 1.5) +
  geom_segment(aes(y = .0232, yend =.0232,
                   x = 2001,  xend = 2010),  # 01-10
                   linetype = bl_line,
                   colour = 'white',
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)   +
  geom_segment(aes(y = .0415, yend =.0415,
                   x = 2010,    xend = 2017), # 10-17
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)   +
  geom_segment(aes(y = .08, yend =.08,
                   x = 2017,  xend = 2019),   # 17-19
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth) 


# School education
ed_priv_all_bl <- ed_priv_all_plot +
geom_hline(aes(yintercept = .07), 
               linetype = bl_line_public,
               colour = bl_col,
               alpha = bl_alpha,
               linewidth = bl_linewidth) +
  geom_segment(aes(y = .3, yend =.3,
                   x = 2001,    xend = 2005), # 97-05
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)   +
  geom_segment(aes(y = .34, yend =.34,
                   x = 2005,  xend = 2017),   # 05-17
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth) +
  geom_segment(aes(y = .286, yend =.286,
                   x = 2017,  xend = 2019),   # 17-19
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)   


#university Education

ed_oxb_all_bl <- ed_oxb_all_plot +
  geom_hline(aes(yintercept = .01), 
                 linetype = bl_line_public,
                 colour = bl_col,
                 alpha = bl_alpha,
                 linewidth = bl_linewidth) +
  geom_segment(aes(y = .25, yend =.25,
                   x = 2001,    xend = 2019), # 97-19
                   linetype = bl_line,
                   colour = bl_col,
                   alpha = bl_alpha,
                   linewidth = bl_linewidth)  








## Edit plots to export in high resolution 

# To export the plots in 300dpi resolution we need to alter some of the plot aesthetics

sex_all_bl  +
  theme(plot.title = element_text(margin = margin(b = -.05, unit = 'cm')), # distance between title and legend
        plot.margin = margin(t = .25, r = .1, unit = 'cm'),
        legend.key.spacing.y = unit(0, 'cm'), # distance between key items
        legend.box.margin = margin(b = -.1, unit = 'cm'), # distance between legend and plot
        text = element_text(size = text_size),
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size)) +
eth_all_bl +
  theme(plot.title = element_text(margin = margin(b = -.05, unit = 'cm')),
        plot.margin = margin(t = .25, l = .1, unit = 'cm'),
        legend.key.spacing.y = unit(0, 'cm'), # distance between key items
        legend.box.margin = margin(b = -.1, unit = 'cm'), # distance between legend and plot
        text = element_text(size = text_size),
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size)) +
ed_priv_all_bl +
  theme(plot.title = element_text(margin = margin(b = -.05, unit = 'cm')),
        plot.margin = margin(t = .25, r = .1, unit = 'cm'),
        legend.key.spacing.y = unit(0, 'cm'), # distance between key items
        legend.box.margin = margin(b = -.1, unit = 'cm'), # distance between legend and plot
        text = element_text(size = text_size),
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size)) +
ed_oxb_all_bl +
  theme(plot.title = element_text(margin = margin(b = -.05, unit = 'cm')),
        plot.margin = margin(t = .25, l = .1,unit = 'cm'),
        legend.key.spacing.y = unit(0, 'cm'), # distance between key items
        legend.box.margin = margin(b = -.1, unit = 'cm'), # distance between legend and plot
        text = element_text(size = text_size),
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size))



# Export figure 1 

ggsave(filename = here( 'output' , 'figures', 'fig1_demo_plot_all_bl.png'),  
       width = 15, height = 16, dpi = 300, units = "cm")



















# 02.Figure 2 - Iraq War all appearances ----------------------------------------------------------------


## Prepare data for plotting 

# Filter relevant appearances
qt_iraq <- qt_0119 |> 
  filter(iraq_stage == 1)

qt_iraq_pol <- qt_iraq |> 
  filter(uk_pol == 1)

qt_iraq_non_pol <- qt_iraq |> 
  filter(uk_pol == 0)



# Get aggregate breakdown of Iraq views for:

# All guests
iw_all <- qt_iraq |>
  count(iraq_inv_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
         guests = paste0('\n', '(N = ', sum(n), ')'),
         group_lab = 'All appearances') |> 
  select(group_lab, everything())



# UK politicians
iw_pol <- qt_iraq_pol |>
  count(iraq_inv_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
         guests = paste0('\n', '(N = ', sum(n), ')'),
         group_lab = 'UK politicians') |> 
  select(group_lab, everything())


# Non-politicians
iw_nonpol <- qt_iraq_non_pol |>
  count(iraq_inv_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
         guests = paste0('\n', '(N = ', sum(n), ')'),
         group_lab = 'Non-politicians') |> 
  select(group_lab, everything())



# Repeat for politician guests, split by party affilitation
# Split by party and apply function
iw_pol_party <- qt_iraq_pol |>
  count(party3, iraq_inv_lab) |> 
  group_split(party3) |> 
  map(group_pct) |> 
  reduce(rbind) |> 
  mutate(group_lab = party3) |> 
  select(-party3)




# Combine data for plotting together
iw_cmb <- rbind(iw_all, iw_pol, iw_nonpol) |> 
  mutate(group_lab = paste0(group_lab, guests)) 

# Set order of factor levels rather than alphabetical
iw_cmb$group_lab <- factor(iw_cmb$group_lab, levels = unique(iw_cmb$group_lab))




iw_all_plot <- iw_cmb |> 
  ggplot(aes(x = group_lab, y = pct, fill = iraq_inv_lab)) +
  geom_hline(yintercept = seq(0, 100, 20), 
             linetype = 'solid', 
             colour = 'grey40',
             alpha = 0.2,
             linewidth = .2) +
  geom_col(position = position_dodge(width = .9), width = .8) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = '', y = '% of Appearances') +
  theme_minimal() + 
  geom_text(aes(label = pct_lab, y = pct+4.5),
            size = 10, position = position_dodge(width = .85)) +
  theme(text = element_text(family = 'arimo', lineheight = 1.2, size = text_size),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = 'top',
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        legend.justification = 'right',
        legend.direction = "horizontal",
        legend.key.size = unit(0.8, "lines"),   
        legend.box.spacing = unit(0, "cm"),
        axis.text.x = element_text(lineheight=.3),
        legend.title = element_blank()) +
  scale_fill_manual(values = iraq_pal)




# Export figure 2

iw_all_plot 
ggsave(filename = here( 'output' , 'figures', 'fig2_plot_iraq_all.png'),  
       width = 14, height = 7, dpi = 300, units = "cm")








# 03.Figure 3 - Iraq War by Party ----------------------------------------------------------------





iw_pol_cmb <- iw_pol_party |> 
  mutate(party_lab = paste0(group_lab, guests))


iw_party_plot <- iw_pol_cmb |> 
  ggplot(aes(x = party_lab, y = pct, fill = iraq_inv_lab)) +
  geom_hline(yintercept = seq(0, 100, 20), 
             linetype = 'solid', 
             colour = 'grey40',
             alpha = 0.2,
             linewidth = .2) +
  geom_col(position = position_dodge(width = .9), width = .8) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = '', y = '% of Appearances') +
  theme_minimal() + 
  geom_text(aes(label = pct_lab, y = pct+4.5),
            size = 10, position = position_dodge(width = .85)) +
  theme(text = element_text(family = 'arimo', lineheight = 1.2, size = text_size),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = 'top',
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        legend.justification = 'right',
        legend.direction = "horizontal",
        legend.key.size = unit(0.8, "lines"),   
        axis.text.x = element_text(lineheight=.3),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank()) +
  scale_fill_manual(values = iraq_pal)


# Export figure 3

iw_party_plot 
ggsave(filename = here( 'output' , 'figures', 'fig3_plot_iraq_party.png'),  
       width = 13, height = 6, dpi = 300, units = "cm")











# 04.Figure 4 - Brexit - Leave\Remain ---------------------------------------------------


# calculate % of guest appearances for each group that were leave/remain
# all appearances first

# get % of guests for each brexit stance
# create label with % suffix, and leading whitespace if <10%

lr_all <- qt_0119 |> 
  filter(parl == '15-17') |> 
  count(brex_lr_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
         guests = paste0('All appearances', '\n', '(N = ', sum(n), ')')) 



# repeat for UK politician appearances

lr_pol <- qt_0119 |> 
  filter(parl == '15-17', uk_pol == 1) |> 
  count(brex_lr_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
         guests = paste0('UK politicians', '\n', '(N = ', sum(n), ')'))


# add in 0 values for empty politician categories so bars will be evenly spaced
lr_pol[dim(lr_pol)[1]+1, ] <- 
  c('Did not vote', '0', '0', '', lr_pol$guests[1]) 
lr_pol[dim(lr_pol)[1]+1, ] <- 
  c('No information', '0', '0', '', lr_pol$guests[1]) 


# repeat for non UK-politician appearances
lr_nonpol <- qt_0119 |> 
  filter(parl == '15-17', uk_pol == 0) |> 
  count(brex_lr_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = if_else(nchar(pct) > 1, paste0(pct, '%'), paste0(' ', pct, '%')),
         guests = paste0('Non-politicians', '\n', '(N = ', sum(n), ')')) 



# combine all three subsets into single dataframe
# convert variable to correct class, and add factor levels to brexit stance

lr_guests <- 
  rbind(lr_all, lr_pol, lr_nonpol) %>%
  mutate(guests = factor(guests, 
                         unique(guests)),
         brex_lr_lab = factor(brex_lr_lab,
                              levels = c('Remain', 'Leave', 
                                         'Did not vote',
                                         'No information'),
                              ordered = T),
         n = as.numeric(n),
         pct = as.numeric(pct))




## Create Plot 


lr_agg_plot <- lr_guests |> 
  ggplot(aes(x = guests, y = pct, fill = brex_lr_lab)) +
  geom_hline(yintercept = seq(0, 100, 20), 
             linetype = 'solid', 
             colour = 'grey40',
             alpha = 0.2,
             linewidth = .2) +
  geom_col(position = position_dodge(width = .9), width = .8) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = '', y = '% of Appearances') +
  theme_minimal() + 
  geom_text(aes(label = pct_lab, y = pct+4.5),
            size = 10, position = position_dodge(width = .85)) +
  theme(text = element_text(family = 'arimo', lineheight = 1.2, size = text_size),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = 'top',
        legend.justification = 'right',
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        legend.direction = "horizontal",
        legend.key.size = unit(0.8, "lines"),   
        axis.text.x = element_text(lineheight=.3),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank()) +
  scale_fill_manual(values = lr_pal)




# Export figure 4 

lr_agg_plot
ggsave(filename = here('output', 'figures', 'fig4_plot_brex_lr.png'),  
       width = 14, height = 7, dpi = 300, units = "cm")











# 05.Figure 5 - Brexit - Categorical Measure --------------------------------------------


# calculate % of guest appearances for each group across each categorical brexit stance
# all appearances first


# get % of guests for each brexit stance
# create label with % suffix, and leading whitespace if <10%
brexcat_all <- qt_0119 |> 
  filter(parl == '17-19') |> 
  count(brex_cat_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = paste0(pct, '%'),
         guests = paste0('All guests', '\n', '(N = ', sum(n), ')')) 

# repeat for UK politicians
brexcat_pol <- qt_0119 |> 
  filter(parl == '17-19', uk_pol == 1) |> 
  count(brex_cat_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = paste0(pct, '%'),
         guests = paste0('UK politicians', '\n', '(N = ', sum(n), ')'))

# repeat for non-UK politicians
brexcat_nonpol <- qt_0119 |> 
  filter(parl == '17-19', uk_pol == 0) |> 
  count(brex_cat_lab) |> 
  mutate(pct = round((n / sum(n) * 100), 0),
         pct_lab = paste0(pct, '%'),
         guests = paste0('Non-politicians', '\n', '(N = ', sum(n), ')')) 

# combine into single dataframe, convert variables to correct class
# and apply factor levels to brexit stance variable
brexcat_guests <- 
  rbind(brexcat_all, brexcat_pol, brexcat_nonpol) %>%
  mutate(guests = factor(guests, 
                         unique(guests)),
         brex_cat_lab = factor(brex_cat_lab,
                               levels = c('Revoke/2nd ref', 'Deal, open to 2nd ref   ', 
                                          'Deal', 'Deal, open to no deal   ', 'No deal/hard brexit',
                                          'No information'),
                               ordered = T),
         n = as.numeric(n),
         pct = as.numeric(pct))


## Create plot


brexcat_agg_plot <- 
  brexcat_guests %>%
  ggplot(aes(x = guests, y = pct, fill = brex_cat_lab)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  geom_hline(yintercept = seq(0, 50, 10), 
             linetype = 'solid', 
             colour = 'grey40',
             alpha = 0.2,
             linewidth = .2) +
  geom_col(position = position_dodge(width = .9), width = .8) +
  labs(x = '', y = '% of Appearances') +
  theme_minimal() + 
  geom_text(aes(label = pct_lab, y = pct+2.1),
            size = 9, position = position_dodge(width = .9)) +
  theme(text = element_text(family = 'arimo', lineheight = 1.2, size = 28),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top',
        legend.justification = 'right',
        legend.direction = "horizontal",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.key.size = unit(0.8, "lines"),    
        legend.box.spacing = unit(0, "cm"),
        legend.spacing.x = unit(0.1, 'cm'),
        axis.text = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        legend.box.margin = margin(0, 0, 0, 0, "cm"), 
        axis.text.x = element_text(lineheight=.3),
        legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_manual(values = cat_pal)



# Export figure 5 

brexcat_agg_plot
ggsave(filename = here('output' , 'figures', 'fig5_plot_brex_cat.png'),  
       width = 15, height = 7.5, dpi = 300, units = "cm")











# Table 1 - Demographics BBCQT Public Parliament --------------------------


# Rouund to nearest percentage
round_n <- 0

# Calculate group percentages for politician and non-politician guests
qt_0119_dem_mean <- rbind(
  qt_0119 |> 
    filter(uk_pol == 1) |> 
    summarise(oxb_pct = round((sum(ed_oxb_lab == 'Oxford/Cambridge') / n())*100, round_n),
      ethmin_pct      = round((sum(eth_grp == 'Ethnic minority') / n())*100, round_n),
      priv_pct        = round((sum(ed_priv_lab == 'Privately educated') / n())*100, round_n),
      fem_pct         = round((sum(sex == '1') / n())*100, round_n))  |> 
    mutate(label = 'Politicians'),
  
  qt_0119 |> 
    filter(uk_pol == 0) |> 
    summarise(oxb_pct = round((sum(ed_oxb_lab == 'Oxford/Cambridge') / n())*100, round_n),
      ethmin_pct      = round((sum(eth_grp == 'Ethnic minority') / n())*100, round_n),
      priv_pct        = round((sum(ed_priv_lab == 'Privately educated') / n())*100, round_n),
      fem_pct         = round((sum(sex == '1') / n())*100, round_n)) |> 
    mutate(label = 'Non-politicians')
)





### UK Population baseline figures

# Full information included in supplementary information file

## UK Public 2001-2019 average population figures
# % Female = 51%
# % Ethnic Minority = 14%
# % Privately educated = 7%
# % Attended Oxford/Cambridge = <1%


eth_year_uk <- data.frame(year = 2001:2021,
           census = c(7.9, rep(NA, 9), 12.8, rep(NA, 9), 17),
           imputed = c(seq(7.9, 12.8, length.out = 11)[1:10], seq(12.8, 17, length.out = 11)))


eth_year_uk |> 
  filter(year < 2019) |> 
  pull(imputed) |> 
  mean()




### MP Baseline figures

# Education figures needed to be manually calculated

# Commons report includes party percentages for education background
# Calculate total percentages for educational background across each parliament
mp_ed <- read.csv(here('data', 'uk_mps_demographic_education_data.csv'))


# Private education and Oxbridge Rates for each parliament 2001-2017
mp_ed <- mp_ed |> 
  mutate(mps_ed = mps_con + mps_lab + mps_lib + mps_snp,
         mps_priv = round(con_pct_priv/100 * mps_con, 0) + 
                    round(lab_pct_priv/100 * mps_lab, 0) + 
                    round(lib_pct_priv/100 * mps_lib, 0) + 
                    round(snp_pct_priv/100 * mps_snp, 0),
         mps_priv_pct = round(mps_priv / mps_ed * 100, 2),
         
         
         mps_oxb = round(con_pct_oxb/100 * mps_con, 0) + 
                    round(lab_pct_oxb/100 * mps_lab, 0) + 
                    round(lib_pct_oxb/100 * mps_lib, 0) + 
                    round(snp_pct_oxb/100 * mps_snp, 0),
         mps_oxb_pct = round(mps_oxb / mps_ed * 100, 2)) |> 
  select(el_year, mps_priv_pct, mps_oxb_pct) |> 
  filter(el_year > 1997 & el_year < 2019)

# This gives us the figures, used in the MP demographic data below





### Mean proportion of MPs in each demographic group

# Import parliament demographic data
mp_dem <- read.csv(here('data', 'uk_mps_demographic_data_01-19.csv'))

# Expand values across all years of each parliamentary term, then take the mean
dem_mean_df <- data.frame(el_year = 2001:2019)  |> 
  left_join(mp_dem, by = 'el_year') |> 
  fill(contains('pct'), .direction = 'down') |> 
  mutate_all(as.numeric)

# Group mean across the 2001-2019 period
round(mean(dem_mean_df$mp_fem_pct), 1)
round(mean(dem_mean_df$mp_em_pct), 1)
round(mean(dem_mean_df$mp_priv_pct), 1)
round(mean(dem_mean_df$mp_oxb_pct), 1)



# Add these figures to the BBCQT Appearance group averages
dem_mean <- 
rbind(qt_0119_dem_mean, 
      c(26, 4, 33, 23, 'UK MPs'),
      c(1, 12, 7, 51, 'UK public'))  







# Add in new rows for net difference (show appearances - population baseline)
dem_mean <- rbind(dem_mean,
c(as.numeric(dem_mean[dem_mean$label == 'Politicians', 1:4]) - as.numeric(dem_mean[dem_mean$label == 'UK MPs', 1:4]), 'Politicians - MPs'),
c(as.numeric(dem_mean[dem_mean$label == 'Non-politicians', 1:4]) - as.numeric(dem_mean[dem_mean$label == 'UK public', 1:4]), 'Non-politicians - UK public'))


# Paste in leading + or - for positive and negative net difference values
if(any(k <- dem_mean[5, 1:4] > 0)){
  dem_mean[5, which(k)] <- paste0('+', dem_mean[5, which(k)])
}

if(any(k <- dem_mean[6, 1:4] > 0)){
  dem_mean[6, which(k)] <- paste0('+', dem_mean[6, which(k)])
}




# Prepare data for table output
# Create new column tabels
dem_mean_df <- dem_mean |> 
  select(label,  
         `% Female` = fem_pct, 
         `% Ethnic Minority` = ethmin_pct, 
         `% Private School` = priv_pct, 
         `% Oxbridge` = oxb_pct)

# Transpose data
dem_table <- dem_mean_df |> 
  t() |> 
  as.data.frame()

# Use the rownames to relabel the columns
dem_table <- cbind(rownames(dem_table), dem_table)
colnames(dem_table) <- dem_table[1,]
# Can remove rownames once this is done
dem_table <- dem_table[-1, ]
rownames(dem_table) <- NULL


# Create Table
tab1 <- dem_table |>  
   gt() |> 
    cols_label(
    label = "",
      `Non-politicians` = 'Others',
      `Politicians - MPs` = 'MPs',
      `Non-politicians - UK public` = 'Public') |> 
  cols_align(
  align =  "center",
  columns = -1) |> 
  tab_spanner(label = "BBCQT Appearances", 
    columns = 2:3)|> 
  tab_spanner(label = "Baseline Group", 
    columns = 4:5) |> 
  tab_spanner(label = "Net Difference", 
    columns = 6:7)

# Export Table
tab1 |> 
   gtsave(here('output', 'tables', "tab_01_demographics.docx"))

 
 









# Table 2 - Parliament Iraq War Attidues ----------------------------------


mps_iraq <- read.csv(here('data', 'uk_mps_01-05_iraq.csv'))


mps_iraq_all_tab <- mps_iraq |> 
  count(iraq_inv) |> 
  mutate(party_pct = round(n / sum(n)*100, 2)) |> 
  mutate(party3 = 'All MPs') |> 
  select(party3, everything())


mps_iraq_party_tab <- mps_iraq |> 
  count(party3, iraq_inv) |> 
  group_by(party3) |> 
  mutate(party_pct = round(n / sum(n)*100, 2))


mps_iraq_tab <- rbind(mps_iraq_all_tab, mps_iraq_party_tab)



tab2 <- mps_iraq_tab  |> 
  gt() |> 
    cols_label(
      'party3' = 'Group',
      'iraq_inv' = 'Iraq Stance',
      'n' = 'Count',
      'party_pct' = 'Group Percent')


# Export Table
tab2 |> 
   gtsave(here('output', 'tables', "tab_02_iraq_mps.docx"))





# Table 3 - Parliament Brexit Preferences ---------------------------------



# The dataset lists all MPs in the 2017 UK Parliament
# For any MPs that already appear in the Question Time dataset, their Brexit attitudes
# can be copied over into the MP data.

# For the remaining MPs, Brexit positions were coded using the same process as 
# the BBCQT appearance dataset, as outlined in the supplementary materials.

# The follow parliamentary votes were also included to aid with coding:

## Theresa May Brexit Deal Voted Down
## Division Date: 16/01/2019
# Link: https://www.publicwhip.org.uk/division.php?date=2019-01-15&number=293&display=allpossible
# Vote was to approve the withdrawal deal negotiated by Theresa May
#    Aye == approve deal
#    No == reject deal

## Vote to Leave the EU without a deal
## Division Date: 27/3/2019
# EU Withdrawal and Future Relationship Votes — Motion (B) — Leave Without a Deal on 12 April 2019
# The majority of MPs voted against leaving the European Union on the 12th of April 2019 without a withdrawal agreement.
# Link: https://www.publicwhip.org.uk/division.php?date=2019-03-27&number=386&display=allpossible
# Aye == leave with no deal on 12th April
# No == do not leave with no deal on 12th april

## Delay Brexit deadline bill
## Division Date: 09/04/2019
# Link: https://www.publicwhip.org.uk/division.php?date=2019-04-09&number=413&display=allpossible
# Vote was to delay the United Kingdom leaving the European Union until 30 June 2019.
#    Aye == delay until June
#    No == don't delay brexit

## Benn Act - Prevent withdrawal unless deal in place
## Division Date: 4/09/2019
# Link: https://www.publicwhip.org.uk/division.php?date=2019-09-04&number=442&display=allpossible
# MPs voted to require the Prime Minister to seek a delay to the United Kingdom leaving
# the European Union until 31 January 2020 unless MPs have approved either terms of a 
# withdrawal agreement, or withdrawal without an agreement.
#    Aye == require extension unless a deal is agreed and parliament approves
#    No == no extension, leave with no deal if no arrangements have been agreed






# Import MP data with brexit coding variables
mps_brexit <- read.csv(here('data', 'uk_mps_17-19_brexit.csv'))


# Create table and add percentages
mps_brexit_tab <- mps_brexit |> 
  filter(exclude == 0) |> 
  count(brex_cat, brex_cat_lab) |> 
  mutate(pct = round(n / sum(n)*100, 2)) |> 
  select(-brex_cat)


# Format the table
tab3 <- mps_brexit_tab  |> 
  gt() |> 
    cols_label(
      'brex_cat_lab' = 'Brexit Preference',
      'n' = 'Count',
      'pct' = 'Percentage')


# Export Table
tab3 |> 
   gtsave(here('output', 'tables', "tab_03_brexit_preference_mps.docx"))





