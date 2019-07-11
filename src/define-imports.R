#############################################################################################
# Just loads the scripts necessary to process the dataset. This script is needed when
# using foreach (parallel processing), since each worker must load all these dependencies.
# 
# 04/2019 Nejat ARINIK
#############################################################################################

source("src/define-constants.R")
source("src/define-algos.R")
source("src/define-functions.R")
source("src/define-paths.R")


source("src/build-networks/extract-networks.R")
source("src/build-networks/process-agreement.R")
source("src/build-networks/extract-rollcall-networks.R")

source("src/partition-networks/detect-rollcall-clusters.R")

source("src/cluster-rollcalls/cluster-rollcalls.R")
source("src/cluster-rollcalls/compare-rollcall-clu-results-by-theme.R")
source("src/cluster-rollcalls/make-post-rollcall-clu-analysis-by-theme.R")

source("src/cluster-rollcalls/aggregate-rollcall-networks-by-cluster.R")
source("src/cluster-rollcalls/partition-aggregated-rollcall-networks.R")
source("src/cluster-rollcalls/compare-aggregated-rollcall-network-clusters.R")
source("src/cluster-rollcalls/define-purity.R")
source("src/cluster-rollcalls/clustering-common.R")
source("src/cluster-rollcalls/define-alluvial-diagram.R")

source("src/define-graphml-operations.R")

source("src/partition-networks/compare-clusters.R")
source("src/partition-networks/detect-clusters.R")
source("src/partition-networks/evaluate-clusters.R")

source("src/prepare-data/generate-data.R")
source("src/prepare-data/load-itsyourparliament.R")
source("src/prepare-data/load-parltrack.R")
source("src/prepare-data/load-votewatch.R")
source("src/prepare-data/process-stats.R")
