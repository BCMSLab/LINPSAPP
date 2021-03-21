counter <- function(x, max, direction = 'next') {
    switch (direction,
        'next' = ifelse(x < max, x + 1, x),
        'back' = ifelse(x > 1, x - 1, x)
    )
}
