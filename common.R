## Intereset level (il)
g_il <- c("Very interested"     = 3,
          "Somewhat interested" = 2,
          "Not interested"      = 1)

## Challenge tags (ct)
g_ct <- c("01-artifacts",
        "02-segmentation",
        "03-virtual-IF",
        "04-VAE",
        "05-cross-talk",
        "06-viz-comp",
        "07-TB-scale",
        "08-thumbnails",
        "09-galaxy",
        "10-neuroglancer",
        "endosomes",		## Endosomes challenge is cancelled
        "11-cosmetic")

## Shorthand notations (sn) for column names
g_sn <- set_names(10:21, ct) %>%
    c(Email       = 2,
      Institution = 4,
      Stage       = 5,
      Network     = 6,
      Experience  = 7,
      Languages   = 8,
      GPU         = 9)
