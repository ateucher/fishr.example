gear_types <- data.frame(
  gear_type = c(
    "nordic_gillnet",
    "sinking_longline",
    "fyke_net",
    "electrofishing"
  ),
  gear_factor = c(1.0, 0.72, 1.35, 0.45),
  description = c(
    "Nordic multi-mesh gillnet (standard reference gear)",
    "Sinking baited longline",
    "Passive fyke net trap",
    "Electrofishing unit"
  ),
  stringsAsFactors = FALSE
)

usethis::use_data(gear_types, internal = TRUE, overwrite = TRUE)
