#' complete list of palettes
#'
#' use \code{\link{girlboss_palette}} to construct palettes of desired length.
#'
#' @export
girlboss_palettes <- list(
  girlboss_in_question = c("#aa08f2", "#f208c5", "#f20850", "#f23508", "#f2aa08", "#c5f208"),
  ur_hot_when_u_cry = c("#406089", "#444089", "#694089", "#894084", "#894060", "#894440"),
  cicero_space_force_mf = c("#31b600", "#00b62a", "#00b685", "#008cb6", "#0031b6","#2a00b6"),
  d1_medschool_mf = c("#f68200", "0074f6", "#00f607", "#f600ef" ,"#00f682", "#d16ce9"),
  lovers_to_friends = c("#00f6e9", "#0088f6", "#000df6", "#6e00f6", "#e900f6", "#f60088"),
  one_night_stand_tm = c("#d729ff", "#ff29bc", "#ff2951", "#ff6c29", "#ffd729","#bcff29"),
  thx_for_the_claws_ig = c("#002fd9", "#d9009c", "#d9aa00", "#00d93d", "#111216", "#b9bdc9"),
  bro_waited_4_yrs_lmao = c("#0e00d8", "#7a00d8", "#d800ca", "#d8005e", "#d80e00", "#d87a00")
)

#' a girlboss palette generator
#'
#' these are a handful of color palettes i made (#girl #boss #moment!!!!! #mfs #in #stem) because I can't use cute pretty colors in my thesis
#' unless they come from a palette. so, take that, higher education! you can't tell me what to do! even better, I named most of these
#' palettes after some mfs I met on tinder. i made my fun pretty palette for my thesis, but then thought that itd be even funnier to build palettes based off
#' of dudes i've seen this year (bc that's totally sane and professional!). i bet everyone reading through this is having so much fun seeing how
#' #sane i am! please admit me into your grad program! i'm so cool and sexy and smart! shoutout to Karthik (if I figure out how to link their github,
#' I will--i'm just really dumb ngl), whose 'wesanderson' color palette skeleton I ripped in order to build out my girboss package. xoxo thx, Karthik!
#'
#' @param n Number of colors desired. unfortunately most palettes now only
#'   have 6 colors. But hopefully i'll add more palettes soon (tinder moment). all color
#'   schemes are derived from my #beautiful #sexy #mind.
#'   if omitted, uses all colors.
#' @param name name of desired palette. choices are:
#'   \code{girlboss_in_question}, \code{ur_hot_when_u_cry},  \code{cicero_space_force_mf},
#'   \code{d1_medschool_mf}, \code{lovers_to_friends},  \code{one_night_stand_tm}, \code{thx_for_the_claws_ig},
#'   \code{bro_waited_4_yrs_lmao}
#' @param type either "continuous" or "discrete". use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return a vector of colours.
#' @export
#' @keywords colors
#' @examples
#' girlboss_palette("girlboss_in_question")
#' girlboss_palette("d1_medschool_mf")
#' girlboss_palette("ur_hot_when_u_cry", 3)
#'
#' # if you need more colors than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colors
#' pal <- girlboss_palette(21, name = "lovers_to_friends", type = "continuous")
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
