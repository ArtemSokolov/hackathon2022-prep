load_registration <- function(fn) {
    ## Intereset level (il)
    il <- c("Very interested"     = 3,
            "Somewhat interested" = 2,
            "Not interested"      = 1)

    ## Shorthand notations (sn) for column names
    sn <- c("01-artifacts",
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
            "11-cosmetic") %>%
        set_names(10:21, .) %>%
        c(Email       = 2,
          Institution = 4,
          Stage       = 5,
          Network     = 6,
          Experience  = 7,
          Languages   = 8,
          GPU         = 9)

    ## Read a list of registrants and clean it up
    read_csv(fn, col_types=cols()) %>%
        rename(!!!sn) %>% select(-endosomes) %>%
        mutate(across(`01-artifacts`:`11-cosmetic`, recode, !!!il))
}
