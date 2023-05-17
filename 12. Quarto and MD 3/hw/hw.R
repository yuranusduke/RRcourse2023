library(quarto)

seasons <- seq(1, 8)
setwd('/Users/kunhongyu/Desktop/DSBA/Year2/Semester4/RR/RRcourse2023/12. Quarto and MD 3/hw')
for (season in seasons){
  file_name <- paste0('season_', season)
  quarto_render("Assignment.qmd", execute_params = list(
    file_name = file_name,
    season = season
  ), output_file = paste0('season', season, ".pdf"))
}