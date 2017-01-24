#' A stopwatch
#'
#' @param mins a number. can be minutes or seconds.
#' @param sound a number specifying the ring sound. See ?beepr::beep
#'
#' @return A ringtone will go off when the time is off.
#' @export
#'
#' @examples
#' timer(1, sound = 8) # 1 minute
#' timer(0.3) # 30 seconds
#' timer(0.05) # 5 seconds
timer <- function(mins, sound = 8) {

total_mins <- ifelse(abs(mins - round(mins)) < .Machine$double.eps^0.5, mins * 60, mins * 100)

timeisup <- Sys.time() + total_mins

while(Sys.time() < timeisup) {
# do nothing
}

beepr::beep(sound)
}
