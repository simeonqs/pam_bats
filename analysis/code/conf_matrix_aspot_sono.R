# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: pam bats  
# Author: Simeon Q. Smeele
# Description: Confusion matrix between sonochiro and animal spot. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('stringr', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths 
path_sono = 'analysis/results/sonochiro/results.csv'
path_aspot = '~/Documents/results_aspot/defenitely_bats/fall'
path_ground_truth = 'aspot/test_data_sets/boeje_test_data/txt'
path_png = 'analysis/results/confusion_matrix_sonochiro_aspot.png'
path_functions = 'analysis/code/functions'

# Load functions
.functions = lapply(list.files(path_functions, full.names = TRUE), source)

# Load data
sono = read.csv(path_sono)
aspot = load.selection.tables(path_aspot, recursive = TRUE)
manual = load.selection.tables(path_ground_truth)

# Subset to include only folders that are shared
sono$folder = sono$file |> strsplit('_') |> sapply(`[`, 1)
aspot$folder = aspot$file |> strsplit('_') |> sapply(`[`, 1)
manual$folder = manual$file |> strsplit('_') |> sapply(`[`, 1)
intersect_folders = intersect(intersect(sono$folder, aspot$folder), 
                              manual$folder)
aspot = aspot[aspot$folder %in% intersect_folders,]
manual = manual[manual$folder %in% intersect_folders,]
sono = sono[sono$folder %in% intersect_folders,]

# Subset sono to only files with bat detections
sono = sono[!sono$Sound.type %in% 
              c('Regn', 'Skib', 'Skib og meget støj', 'Noise', 'noise',
                'parasi'),]
message('Including following types from Sonochiro:')
print(table(sono$Sound.type))
rownames(sono) = seq_len(nrow(sono))
sono$Sound.type = 'target'

# Run conf.mat
class_results_compare = conf.mat(aspot, sono)
class_results_aspot = conf.mat(aspot, manual)
class_results_sono = conf.mat(sono, manual)

# Plot confusion matrix
png(path_png, width = 8.5, height = 3, units = 'in', res = 1000)
par(mfrow = c(1, 3))
plot.conf.mat(levels = c('target', 'noise'), class_results_compare,
              path_pdf = NULL, labs = c('aspot', 'sono'), main = 'compared')
plot.conf.mat(levels = c('target', 'noise'), class_results_aspot,
              path_pdf = NULL, labs = c('aspot', 'manual'), main = 'aspot')
plot.conf.mat(levels = c('target', 'noise'), class_results_sono,
              path_pdf = NULL, labs = c('sono', 'manual'), main = 'sonochiro')
dev.off()

# Message
message('Stored confusion matrix.')
