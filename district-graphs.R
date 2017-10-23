require(tidyverse)
require(forcats)
require(scales)

fhrs <- read_csv('combined.csv')
# optional - test with only two districts
fhrs <- fhrs %>%
  filter(district_name %in% c('Warwick', 'Rugby'))

good_colour <- '#5DA5DA'
bad_colour <- '#F15854'
width = 8
height = 6


# number of FHRS establishments

fhrs_no_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  select(district_id, district_name, date, total_FHRS, Match=matched,
         Mismatch=matched_postcode_error) %>%
  gather(key=Postcode, value=Establishments,
         -c(district_id, district_name, date, total_FHRS)) %>%
  mutate(Postcode=fct_rev(Postcode)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_line(aes(y=total_FHRS, colour='Total')) +
                       geom_area(aes(y=Establishments, fill=Postcode)) +
                       scale_color_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour)) +
                       scale_y_continuous(breaks=pretty_breaks()) +
                       labs(title=paste('FHRS establishments in', .x),
                            subtitle='Total number of establishments with a geocode and number matched in OSM',
                            caption='Postcode match = FHRS postcode matches OSM addr:postcode or not:addr:postcode',
                            x='Date', y='Number of establishments') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(fhrs_no_plots$district_id, fhrs_no_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('fhrs-no-', .x, '.png')),
             plot=.y, width=width, height=height))


# % of FHRS establishments

fhrs_pc_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  mutate(Match = matched*100 / total_FHRS,
         Mismatch = matched_postcode_error*100 / total_FHRS) %>%
  select(district_id, district_name, date, Match, Mismatch) %>%
  gather(key=Postcode, value=Percentage,
         -c(district_id, district_name, date)) %>%
  mutate(Postcode=fct_rev(Postcode)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_area(aes(y=Percentage, fill=Postcode)) +
                       scale_color_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour)) +
                       scale_y_continuous(breaks=pretty_breaks()) +
                       labs(title=paste('Percentage of FHRS establishments matched in', .x),
                            caption='Postcode match = FHRS postcode matches OSM addr:postcode or not:addr:postcode',
                            x='Date', y='% of FHRS establishments') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(fhrs_pc_plots$district_id, fhrs_pc_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('fhrs-pc-', .x, '.png')),
             plot=.y, width=width, height=height))


# number of OSM entities

osm_no_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  mutate(NonMismatch = matched + OSM_with_postcode,
         Mismatch = matched_postcode_error) %>%
  select(district_id, district_name, date, NonMismatch, Mismatch, total_OSM) %>%
  gather(key=Postcode, value=Number,
         -c(district_id, district_name, date, total_OSM)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_line(aes(y=total_OSM, colour='Total')) +
                       geom_area(aes(y=Number, fill=Postcode)) +
                       scale_colour_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour)) +
                       labs(x='Date', y='Number of OSM nodes/ways',
                            title=paste('Relevant OSM nodes/ways in', .x),
                            subtitle='Total number of relevant OSM nodes/ways and number with a postcode set',
                            caption='Non-mismatch = OSM addr:postcode or not:addr:postcode matches FHRS postcode, or fhrs:id not set') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(osm_no_plots$district_id, osm_no_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('osm-no-', .x, '.png')),
             plot=.y, width=width, height=height))


# % of OSM entities

osm_pc_plots <- fhrs %>%
  group_by(district_id, district_name) %>%
  mutate(NonMismatch = (matched + OSM_with_postcode) * 100 / total_OSM,
         Mismatch = matched_postcode_error*100 / total_OSM) %>%
  select(district_id, district_name, date, NonMismatch, Mismatch) %>%
  gather(key=Postcode, value=Percentage,
         -c(district_id, district_name, date)) %>%
  nest() %>%
  mutate(plot = map2(district_name, data,
                     ~ggplot(aes(x=date), data=.y) +
                       geom_area(aes(y=Percentage, fill=Postcode)) +
                       scale_color_manual(values='black', name='') +
                       scale_fill_manual(values=c(bad_colour, good_colour)) +
                       scale_y_continuous(breaks=pretty_breaks()) +
                       labs(x='Date', y='% of OSM nodes/ways',
                            title=paste('Percentage of relevant OSM nodes/ways with a postcode in', .x),
                            caption='Non-mismatch = OSM addr:postcode or not:addr:postcode matches FHRS postcode, or fhrs:id not set') +
                       guides(colour=guide_legend(order=1), fill=guide_legend(order=2))
  ))

map2(osm_pc_plots$district_id, osm_pc_plots$plot,
     ~ggsave(file=file.path('district-graphs', paste0('osm-pc-', .x, '.png')),
             plot=.y, width=width, height=height))