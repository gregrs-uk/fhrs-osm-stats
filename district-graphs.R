require(tidyverse)
require(forcats)
require(scales)

fhrs <- read_csv('combined.csv')
# optional - test with only two districts
#fhrs <- fhrs %>%
#  filter(district_name %in% c('Warwick', 'Rugby'))

good_colour <- '#5DA5DA'
bad_colour <- '#F15854'
width = 8
height = 6


# number of FHRS establishments

fhrs_no_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  select(district_id, district_name, date, total_FHRS, PostcodeMatch=matched,
         PostcodeMismatch=matched_postcode_error) %>%
  gather(key=Type, value=Establishments,
         -c(district_id, district_name, date, total_FHRS)) %>%
  mutate(Type=fct_rev(Type)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_line(aes(y=total_FHRS, colour='Total')) +
                       geom_area(aes(y=Establishments, fill=Type)) +
                       scale_color_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour), name='') +
                       scale_y_continuous(breaks=pretty_breaks()) +
                       labs(title=paste('Number of FHRS establishments in', .x, 'over time'),
                            subtitle='Total number of establishments and number matched in OSM',
                            x='Date', y='Number of establishments') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(fhrs_no_plots$district_id, fhrs_no_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('fhrs-no-', .x, '.png')),
             plot=.y, width=width, height=height))


# % of FHRS establishments

fhrs_pc_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  mutate(PostcodeMatch = matched*100 / total_FHRS,
         PostcodeMismatch = matched_postcode_error*100 / total_FHRS) %>%
  select(district_id, district_name, date, PostcodeMatch, PostcodeMismatch) %>%
  gather(key=Type, value=Percentage,
         -c(district_id, district_name, date)) %>%
  mutate(Type=fct_rev(Type)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_area(aes(y=Percentage, fill=Type)) +
                       scale_color_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour), name='') +
                       scale_y_continuous(breaks=pretty_breaks()) +
                       labs(title=paste('Percentage of FHRS establishments in', .x, 'matched over time'),
                            x='Date', y='% of FHRS establishments') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(fhrs_pc_plots$district_id, fhrs_pc_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('fhrs-pc-', .x, '.png')),
             plot=.y, width=width, height=height))


# number of OSM entities

osm_no_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  mutate(NonMismatched = matched + OSM_with_postcode,
         PostcodeMismatch = matched_postcode_error) %>%
  select(district_id, district_name, date, NonMismatched, PostcodeMismatch, total_OSM) %>%
  gather(key=Type, value=Number,
         -c(district_id, district_name, date, total_OSM)) %>%
  mutate(Type=fct_rev(Type)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_line(aes(y=total_OSM, colour='Total')) +
                       geom_area(aes(y=Number, fill=Type)) +
                       scale_colour_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour), name='') +
                       labs(x='Date', y='Number of OSM nodes/ways',
                            title=paste('Number of relevant OSM nodes/ways in', .x, 'over time'),
                            subtitle='Total number of relevant OSM nodes/ways and number with a postcode set') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(osm_no_plots$district_id, osm_no_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('osm-no-', .x, '.png')),
             plot=.y, width=width, height=height))


# % of OSM entities

osm_pc_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  mutate(NonMismatched = (matched + OSM_with_postcode) * 100 / total_OSM,
         PostcodeMismatch = matched_postcode_error*100 / total_OSM) %>%
  select(district_id, district_name, date, NonMismatched, PostcodeMismatch) %>%
  gather(key=Type, value=Percentage,
         -c(district_id, district_name, date)) %>%
  mutate(Type=fct_rev(Type)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_area(aes(y=Percentage, fill=Type)) +
                       scale_color_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour), name='') +
                       scale_y_continuous(breaks=pretty_breaks()) +
                       labs(title=paste('Percentage of relevant OSM nodes/ways in', .x, 'with a postcode over time'),
                            x='Date', y='% of OSM nodes/ways') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(osm_pc_plots$district_id, osm_pc_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('osm-pc-', .x, '.png')),
             plot=.y, width=width, height=height))