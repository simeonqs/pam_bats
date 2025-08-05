library(dplyr, stringr)

morten = read.csv('analysis/data/ENVarter_ME.csv')
signe = read.csv2('analysis/data/ENVarter_SB.csv')

colnames(signe) = c('file', 'v1', 'v2', 'art_signe', 'v3')
signe$file = signe$file |> str_remove_all('"') |> str_remove_all(',')

morten = morten[order(morten$file),]
signe = signe[order(signe$file),]

if(any(morten$file != signe$file)) stop('Mismatch!')

merged = morten[,c('file', 'art')]
colnames(merged) = c('file', 'art_morten')
merged = cbind(merged, signe[,c('art_signe')])

write.csv(merged, '~/Desktop/species_offshore_env.csv')