# funciton writing files to dir

write_to_dir = function(l = NULL,
                        output_dir = NULL,
                        output_file = NULL,
                        file_format = c('csv', 'json'),
                        file_type = c('single', 'multiple')) {
  # TODO optimize memory handling. Maybe clean RAM cache before every file creation
  # checks
  if (is.null(l))
    stop('Provide a list to be put out.')
  if (is.null(output_dir))
    stop('Provide an ouput directory.')
  if (is.null(output_file))
    stop('Provide a file name for the output.')
  file_format = match.arg(file_format)
  file_type = match.arg(file_type)
  # variables
  dir = file.path(output_dir, output_file)
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  # writing
  if (file_type == 'multiple') {
    for (i in seq_along(l)) {
      dt = l[[i]]
      nam = names(l)[i]
      if (file_format == 'csv')
        fwrite(dt, file.path(dir, paste0(nam, '.csv')))
      if (file_format == 'json')
        jsonlite::write_json(dt, file.path(dir, paste0(nam, '.json')))
    }
  }
  if (file_type == 'single') {
    out = cbind_fill(dflist = l)
    if (file_format == 'csv')
      fwrite(out, file.path(dir, paste0(output_file, '.csv')))
    if (file_format == 'json')
      jsonlite::write_json(out, file.path(dir, paste0(output_file, '.json')))
  }
}
