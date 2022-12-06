runner <- function(expr) {
    tictoc::tic.clear()
    tictoc::tic()
    werethereanyerrors <- "There was no error"
    tryCatch(
        expr = {
            expr
        },
        error = function(err) {
            werethereanyerrors <<- "There was an error"
            print(err)
        },
        finally = {
            message(werethereanyerrors)
            alert <- tictoc::toc()
            tictoc::tic.clear()
            notification <- paste(alert$callback_msg, "and", werethereanyerrors)
            beepr::beep(2)
            # RPushbullet::pbPost("note", "Script alert", notification)
        }
    )
}
