## ----echo = FALSE, message = FALSE--------------------------------------------
library(knitr)
library(kableExtra)
library(TestDesign)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(constraints_science_raw[1:5, ]) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em") %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_science_raw[1, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em", background = 'cyan') %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_science_raw[32, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em", background = 'cyan') %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_science_raw[33, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em", background = 'cyan') %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_science_raw[34, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em", background = 'cyan') %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_science_raw[35, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em", background = 'cyan') %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_science_raw[36, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em", background = 'cyan') %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_reading_raw[3, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em") %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em", background = 'cyan') %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_fatigue_raw[1, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em") %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em", background = 'cyan') %>%
  column_spec(6, "3em", background = 'cyan') %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_reading_raw[17, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em") %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em", background = 'cyan') %>%
  column_spec(6, "3em", background = 'cyan') %>%
  column_spec(7, "3em")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(constraints_reading_raw[18, ], row.names = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, "5em") %>%
  column_spec(2, "5em") %>%
  column_spec(3, "5em") %>%
  column_spec(4, "10em") %>%
  column_spec(5, "3em") %>%
  column_spec(6, "3em") %>%
  column_spec(7, "3em", background = 'cyan')

