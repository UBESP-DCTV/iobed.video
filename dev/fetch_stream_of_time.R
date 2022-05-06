

# Functions -------------------------------------------------------


fetch_stream_of_time <- function(sec, fps = 3) {

  op <- options(digits.secs = 6)
  withr::defer(options(op))

  my_stream <- Rvision::stream(index = 0)
  withr::defer(Rvision::release(my_stream))

  t <- fps * sec
  frames <- vector("list", t)
  names <- vector("character", t)

  for (i in seq_len(t)) {
    names[[i]] <- Sys.time() |>
      stringr::str_replace_all(c(
        `-|:` = "",
        ` `="t",
        `\\.`="m"
      ))

    frames[[i]] <- Rvision::readNext(my_stream)
  }

  invisible(purrr::set_names(frames, names))
}



write_stream_files <- function(stream, dir) {
  stopifnot(fs::is_dir(dir))

  output_files <- paste0(names(stream), ".png")
  out_paths <- fs::path_expand(file.path(dir, output_files))

  wi_safe <- purrr::safely(~{
    suppressMessages(Rvision::write.Image(.x, .y))
    TRUE
  }, otherwise = FALSE)
  res <- vector("list", length(stream)) |>
    purrr::set_names(output_files)

  for (frame in seq_along(stream)) {
    res[[frame]] <- wi_safe(stream[[frame]], out_paths[[frame]])
  }
  res <- purrr::transpose(res)
  are_succeded <- unlist(res[["result"]])
  errors <- res[["error"]][!are_succeded]

  if (!all(are_succeded)) {
    usethis::ui_warn("Error(s) occured during image saving")
    usethis::ui_info(
      "The attribute {usethis::ui_field('errors')} of the (invisible) output contains the error messages."
    )
    return(invisible(`attr<-`(FALSE, "errors", errors)))
  } else {
    usethis::ui_done(
      "All images are written into {usethis::ui_value(dir)}."
    )
    return(invisible(TRUE))
  }
}



# examples --------------------------------------------------------

stream <- fetch_stream_of_time(2)
a <- write_stream_files(stream, "test")
b <- write_stream_files(stream, "test")
a
b
