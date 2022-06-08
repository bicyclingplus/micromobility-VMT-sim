subsample_indx <- sample(unique(t3$sampno), 3)

subsample <- t3[ t3$sampno %in% subsample_indx, ]

subsample[[ 'study_week' ]] <- subsample[[ 'week' ]] + 52 * (subsample[[ 'year' ]] - 2004)

subsample$id = as.factor(subsample$sampno)

ggplot(subsample) + aes(x=study_week, y=totdist, group=sampno, color=id) + geom_line() + xlab("Study week") + ylab("Weekly distance driven")

ggplot(subsample) + aes(x=study_week, y=totdist) + geom_point()
