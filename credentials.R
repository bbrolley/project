install.packages("usethis")
gitcreds::gitcreds_set()
# And enter the created GH-Token in the R console when prompted
usethis::use_git_config(user.name = "bbrolley",
                        user.email = "barry.brolley@businesspartner.roche.com")
