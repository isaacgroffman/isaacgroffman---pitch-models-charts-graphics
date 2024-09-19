#find player ids
miller_id <- baseballr::playerid_lookup(last_name = "Miller", first_name = "Bryce") %>%
  dplyr::pull(mlbam_id)

#pull specific statctast data on the pitchers
miller_data <- baseballr::statcast_search_pitchers(start_date = "2024-03-31",
                                                   end_date = "2024-09-06",
                                                   pitcherid = 682243)
# Select the relevant columns
select_columns <- function(data) {
  data %>%
    select(pitch_type, 
           release_speed, 
           release_spin_rate, 
           pfx_x, 
           pfx_z, 
           plate_x, 
           plate_z, 
           description, 
           release_pos_z,   # Release point (vertical)
           release_extension)  # Release extension
}

# Apply to our data 
miller_filtered_data <- select_columns(miller_data)

# Calculate traditional IVB and HB
miller_filtered_data <- miller_filtered_data %>%
  mutate(ivb = pfx_z * 12,  
         hb = pfx_x * -12)

# Filter the pitches you want to make a heat map of
miller_filtered_data <- miller_filtered_data %>%
  filter(pitch_type == "FS", release_spin_rate <= 820)

ggplot(miller_filtered_data, aes(x = plate_x, y = plate_z)) +
  geom_density_2d_filled(aes(fill = stat(level)), contour_var = "ndensity") +
  scale_fill_manual(values = c("white", "transparent", "transparent","transparent","transparent","transparent","lightpink","lightcoral","red","darkred")) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(0, 5)) +
  labs(title = "Bryce Miller FS Locations, under 820 RPM's",
       subtitle = "By @isaacgroffman",
       x = "Horizontal Location",
       y = "Vertical Location",
       fill = "Pitch Density") +
  theme_void() +
  geom_segment(aes(x = -0.71, xend = 0.71, y = 1.5, yend = 1.5), color = 'black') +
  geom_segment(aes(x = -0.71, xend = 0.71, y = 3.6, yend = 3.6), color = 'black') +
  geom_segment(aes(x = -0.71, xend = -0.71, y = 1.5, yend = 3.6), color = 'black') +
  geom_segment(aes(x = 0.71, xend = 0.71, y = 1.5, yend = 3.6), color = 'black') +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        text = element_text(color = "#333333"))



