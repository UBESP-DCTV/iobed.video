# Install rtools
# https://cran.r-project.org/bin/windows/Rtools/history.html

# Install CMake at system level
# https://cmake.org/download/

# install.packages("ROpenCVLite")
# ROpenCVLite::installOpenCV() # if it works....
#
# install.packages("devtools")
# devtools::install_github("swarm-lab/Rvision")


record_from_camera <- function(
    file_path = here::here(lubridate::now() |>
      stringr::str_remove_all("[^\\d]") |>
      paste0("_video.mp4")),
    index = 0,
    ...,
    fps = 30,
    fourcc = "mpeg"
) {

  file_path <- fs::path_expand(file_path) |> normalizePath()

  if (fs::file_exists(file_path)) {
    usethis::ui_info(
      "For recording the camera stream to a file it must not exists."
    )
    usethis::ui_todo(
      "Please, delete or move it, or select a different new {usethis::ui_field('file_path')} and re-run {usethis::ui_code('record_from_camera')}."
    )
    usethis::ui_stop("The file {usethis::ui_value(file_path)} exists.")
  }

  my_stream <- Rvision::stream(index = index)
  withr::defer(Rvision::release(my_stream))

  # Create a queue of frames
  my_buffer <- Rvision::queue(
    my_stream,
    size = 10 * fps,
    # delay = round(1e6/fps),
    overflow = "grow"
  )
  withr::defer(Rvision::release(my_buffer))


  imm <- Rvision::readNext(my_buffer)
  img <- imm[nrow(imm):1, 1:ncol(imm), 3:1 , drop = FALSE]
  img_range <- range(img)
  if (diff(img_range) != 0) {
    img <- 255 * (
      (img - img_range[1]) / diff(img_range)
    )
  }


  plot(Rvision::readNext(my_stream))


  my_writer <- Rvision::videoWriter(
    outputFile = file_path,
    fourcc = fourcc,
    fps = fps,
    height = nrow(imm), width = ncol(imm)
  )
  withr::defer(Rvision::release(my_writer))

  for (i in seq_len(fps * 100)) {
    if (!Rvision::empty(my_buffer)) {
      Rvision::writeFrame(my_writer, Rvision::readNext(my_buffer))
    }
  }

}


record_from_camera()


