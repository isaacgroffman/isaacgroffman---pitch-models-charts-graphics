miller_filtered_table <- miller_data %>%
  select(pitch_type, release_speed, release_spin_rate,pfx_x,pfx_z, description)

miller_filtered_table <- miller_filtered_table %>%
  mutate(
    ivb = pfx_z * 12,  # Convert pfx_z (in feet) to inches
    hb = pfx_x * -12   # Convert pfx_x (in feet) to inches and reverse sign for HB
  )

total_pitches <- nrow(miller_filtered_table)

miller_filtered_table <- miller_filtered_table %>%
  group_by(pitch_type) %>%
  mutate(
    usage_pct = (n() / total_pitches) * 100  # Usage percentage for each pitch type
  )

miller_filtered_table <- miller_filtered_table %>%
  mutate(
    swinging_strike = ifelse(description == "swinging_strike", 1, 0)  # Binary flag for swinging strikes
  ) %>%
  group_by(pitch_type) %>%
  mutate(
    swinging_strike_pct = (sum(swinging_strike) / n()) * 100  # Calculate swinging-strike percentage
  )

miller_filtered_table <- miller_data %>%
  select(pitch_type, release_speed, release_spin_rate,pfx_x,pfx_z, description)

miller_filtered_table <- miller_filtered_table %>%
  mutate(
    ivb = pfx_z * 12,  # Convert pfx_z (in feet) to inches
    hb = pfx_x * -12   # Convert pfx_x (in feet) to inches and reverse sign for HB
  )

total_pitches <- nrow(miller_filtered_table)

miller_filtered_table <- miller_filtered_table %>%
  group_by(pitch_type) %>%
  mutate(
    usage_pct = (n() / total_pitches) * 100  # Usage percentage for each pitch type
  )

miller_filtered_table <- miller_filtered_table %>%
  mutate(
    swinging_strike = ifelse(description == "swinging_strike", 1, 0)  # Binary flag for swinging strikes
  ) %>%
  group_by(pitch_type) %>%
  mutate(
    swinging_strike_pct = (sum(swinging_strike) / n()) * 100  # Calculate swinging-strike percentage
  )

total_pitches <- nrow(miller_filtered_table)

miller_summary <- miller_filtered_table %>%
  group_by(pitch_type) %>%
  summarize(count = n(),
            avg_velo = round(mean(release_speed, na.rm = TRUE), 1),  # Round to 1 decimal
            avg_spin = round(mean(release_spin_rate, na.rm = TRUE), 1),
            avg_ivb = round(mean(ivb, na.rm = TRUE), 1),
            avg_hb = round(mean(hb, na.rm = TRUE), 1),
            usage_pct = round(100 * n() / total_pitches, 1),  # Calculate usage percentage
            swinging_strike_pct = round(100 * mean(swinging_strike, na.rm = TRUE), 1)) %>%
  arrange(desc(count)) # Sort by count in descending order

kable(miller_summary, 
      caption = "Bryce Miller Pitch Data for 2024 Season", 
      col.names = c("Pitch Type", "Count", "Avg Velo", "Avg Spin", "Avg IVB", "Avg HB", 
                    "Usage %", "Swinging Strike %")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE) %>%
  column_spec(1, bold = TRUE, color = "black", background = "#F7F7F7") %>%
  column_spec(2:8, color = "black") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#D35400") %>%
  add_header_above(c(" " = 8)) %>%
  footnote(general = "Chart by @isaacgroffman", 
           general_title = "", 
           footnote_as_chunk = TRUE, 
           escape = FALSE) %>%
  kable_classic(full_width = FALSE, html_font = "Arial")
