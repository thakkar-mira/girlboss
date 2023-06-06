#' complete list of palettes
#'
#' use \code{\link{girlboss_palette}} to construct palettes of desired length.
#'
#' @export
girlboss_palettes <- list(

  girlboss_in_q = c("#f208c5", "#f20850", "#f23508", "#f2aa08", "#c5f208", "#b9a59b","#17B5D5","#aa08f2"),

  fem_in_stem = c("#ffb8f2","#ff0095", "#ff4d00", "#ffcc00", "#8f0053", "#ab6ad9", "#7b08ff", "#3b0022"),

  girls_when = c("#EB0000", "#750000", "#0099DF", "#005780", "#989898", "#232A2D", "#7F4D2F", "#402718"),

  hot_when_u_cry = c("#bdbdbd", "#a5a5a5","#8e8e8e", "#767676", "#5f5f5f", "#474747", "#2f2f2f", "#181818"),

  space_force_cicero = c("#FFD100", "#31b600", "#169617", "#00b685", "#008cb6", "#0031b6","#7A22D6", "#40037F"),

  d1_medschool = c("#FFBE80","#FF7C00", "#80A5FF", "#004AFF", "#AEC7A8", "#266916", "#A9AFBB","#535E77"),

  elf_bar = c("#FF97E5", "#FF97B1", "#FFB197", "#FFE597", "#E5FF97", "#B1FF97", "#B9EFFF", "#DBB9FF")

)

#' a girlboss palette generator
#'
#' these are a handful of color palettes i made (#girl #boss #moment!!!!! #mfs #in #stem)!! can't explain the inspiration behind the names of the palettes
#' here but i bet you can prob guess!! #gatekeep #gaslight #girlboss!
#'
#' @param n number of colors desired. unfortunately most palettes only
#'   have 6 colors. hopefully i'll add more palettes soon (as i get more stories). all color
#'   schemes are derived from my #beautiful #sexy #mind.
#'   if omitted, uses all colors.
#'
#' @param name name of desired palette. choices are:
#'   \code{girlboss_in_q}, \code{fem_in_stem}, \code{girls_when}, \code{hot_when_u_cry},  \code{space_force_cicero},
#'   \code{d1_medschool}, \code{elf_bar}
#'
#' @param type either "continuous" or "discrete". use continuous if you want
#'   to automatically interpolate between colours.
#'    @importFrom graphics rgb rect par image text
#' @return a vector of colours.
#' @export
#' @keywords colors
#' @examples
#' girlboss_palette("girlboss_in_q")
#' girlboss_palette("d1_medschool")
#' girlboss_palette("fem_in_stem", 3)
#'
#' # if you need more colors than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colors
#' pal <- girlboss_palette(21, name = "fem_in_stem", type = "continuous")
#' image(volcano, col = pal)
girlboss_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- girlboss_palettes[[name]]
  if (is.null(pal))
    stop("ur delusional. or bad at spelling.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
