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
    file_path = lubridate::now() |>
      stringr::str_remove_all("[^\\d]") |>
      paste0("_video.mp4"),
    seconds = 5,
    ...,
    fps = 30,
    fourcc = "mpeg"
) {

  file_path <- fs::path_expand(file_path)

  if (fs::file_exists(file_path)) {
    usethis::ui_info(
      "For recording the camera stream to a file it must not exists."
    )
    usethis::ui_todo(
      "Please, delete or move it, or select a different new {usethis::ui_field('file_path')} and re-run {usethis::ui_code('record_from_camera')}."
    )
    usethis::ui_stop("The file {usethis::ui_value(file_path)} exists.")
  }

  my_stream <- Rvision::stream(index = 0)
  imm <- Rvision::readNext(my_stream)
  img <- imm[nrow(imm):1, 1:ncol(imm), 3:1 , drop = FALSE]
  img_range <- range(img)
  if (diff(img_range) != 0) {
    img <- 255 * (
      (img - img_range[1]) / diff(img_range)
    )
  }

  withr::defer(Rvision::release(my_stream))

  plot(Rvision::readNext(my_stream))

  my_writer <- Rvision::videoWriter(
    outputFile = file_path,
    fourcc = fourcc,
    fps = fps,
    height = 720, width = 1280
  )

  withr::defer(Rvision::release(my_writer))

  for (i in seq_len(fps * seconds)) {
    Rvision::writeFrame(my_writer, Rvision::readNext(my_stream))
  }

}


record_from_camera("cicciopasticcio.mpg")



