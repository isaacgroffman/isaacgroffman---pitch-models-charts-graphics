miller_cleaned_data <- miller_data %>%
dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
              game_type == "R") %>% 
  dplyr::mutate(pfx_x_in_pv = -12*pfx_x,
                pfx_z_in = 12*pfx_z)

# Make a named vector to scale pitch colors with
pitch_colors <- c("4-Seam Fastball" = "red",
                  "2-Seam Fastball" = "blue",
                  "Sinker" = "cyan",
                  "Cutter" = "violet",
                  "Fastball" = "black",
                  "Curveball" = "green",
                  "Knuckle Curve" = "pink",
                  "Slider" = "orange",
                  "Changeup" = "gray50",
                  "Split-Finger" = "beige",
                  "Knuckleball" = "gold")

# Find unique pitch types to not have unnecessary pitches in legend
miller_pitch_types <- unique(miller_cleaned_data$pitch_name)

miller_cleaned_data %>%
  ggplot2::ggplot(ggplot2::aes(x = pfx_x_in_pv, y = pfx_z_in, color = pitch_name)) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_hline(yintercept = 0) +
  # Make the points slightly transparent
  ggplot2::geom_point(size = 1.5, alpha = 0.7) +
  # Scale the pitch colors to match what we defined above
  # and limit it to only the pitches Burnes throws
  ggplot2::scale_color_manual(values = pitch_colors,
                              limits = miller_pitch_types) +
  # Scale axes and add " to end of labels to denote inches
  ggplot2::scale_x_continuous(limits = c(-22,22),
                              breaks = seq(-22,22, 5),
                              labels = scales::number_format(suffix = "\"")) +
  ggplot2::scale_y_continuous(limits = c(-22,22),
                              breaks = seq(-22,22, 5),
                              labels = scales::number_format(suffix = "\"")) +
  ggplot2::coord_equal() +
  ggplot2::labs(title = "Bryce Miller Pitch Movement",
                subtitle = "By @isaacgroffman",
                x = "Horizontal Break",
                y = "Induced Vertical Break",
                color = "Pitch Name")
