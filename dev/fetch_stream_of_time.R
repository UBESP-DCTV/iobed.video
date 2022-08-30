

# Functions -------------------------------------------------------




max_sec_estimated <- 60
fps <-  30
index <-  0

op <- options(digits.secs = 6)
withr::defer(options(op))

my_stream <- Rvision::stream(index = index)

my_queue <- my_stream |>
  Rvision::queue(
    size = max_sec_estimated * fps,
    overflow = "replace"
  )


frames <- vector("list", 3600 * fps)
names <- vector("character", 3600 * fps)
i <- 1L
while (TRUE) {
  frames[[i]] <- Rvision::readNext(my_queue)

  names[[i]] <- Sys.time() |>
    stringr::str_replace_all(c(
      `-|:` = "",
      ` `="t",
      `\\.`="m"
    ))
  i <- i + 1L
}

res <- purrr::set_names(frames, names)



fetch_stream_of_time <- function(
    max_sec_estimated = 3600,
    fps = 25,
    stop_loop = FALSE
) {

  op <- options(digits.secs = 6)
  withr::defer(options(op))

  my_stream <- Rvision::stream(index = 0)
  withr::defer(Rvision::release(my_stream))

  t <- fps * max_sec_estimated
  ms_step <- round(1 / fps, 3)
  frames <- vector("list", t)
  names <- vector("character", t)



  time_0 <- Sys.time()

  for (i in seq_len(t)) {
    if (stop_loop) break

    time_i <- Sys.time()
    while (time_i < time_0 + ms_step) {
      time_i <- Sys.time()
    }

    frames[[i]] <- Rvision::readNext(my_stream)

    names[[i]] <- time_i |>
      stringr::str_replace_all(c(
        `-|:` = "",
        ` `="t",
        `\\.`="m"
      ))

    time_0 <- time_i
  }

  invisible(purrr::set_names(frames, names))
}



write_queue_files <- function(queue, dir) {
  stopifnot(fs::is_dir(dir))

  output_files <- paste0(seq_len(length(queue)), ".png")
  out_paths <- fs::path_expand(file.path(dir, output_files))

  wi_safe <- purrr::safely(~{
    suppressMessages(Rvision::write.Image(.x, .y))
    TRUE
  }, otherwise = FALSE)
  res <- vector("list", length(queue)) |>
    purrr::set_names(output_files)

  for (frame in seq_along(queue)) {
    res[[frame]] <- wi_safe(queue$readNext(), out_paths[[frame]])
  }
  res <- purrr::transpose(res)
  are_succeded <- unlist(res[["result"]])
  errors <- res[["error"]][!are_succeded]

}


write_stream_files <- function(stream, dir) {
  stopifnot(fs::is_dir(dir))

  output_files <- paste0(seq_len(length(stream)), ".png")
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
