setOldClass("RODBC");
setClass(	"FLDataMiningAnalysis", 
			slots = list(	ODBCConnection       = "RODBC",
							AnalysisID           = "character",
							WidetoDeepAnalysisID = "character",
							DeepTableName        = "character"))

setClass(	"FLKMeans",
			representation(	centers = "data.frame",
							cluster = "data.frame"),
			contains = "FLDataMiningAnalysis")

setGeneric("fetch.results", function(object) {
  standardGeneric("fetch.results")
})

# fetch_results method for KMeans
setMethod("fetch.results",
          signature("FLKMeans"),
          function(object) {
      		DBConnection <- object@ODBCConnection;            
      		
      		#Fetch Centers Dendrogram
			SQLStr           <- paste("SELECT HypothesisID,Level,ClusterID,VarID,Centroid FROM fzzlKMeansDendrogram WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			KMeansDendrogram <- sqlQuery(DBConnection, SQLStr);
				
			#Fetch ClusterID Arrays
			SQLStr           <- paste("SELECT HypothesisID,ObsID,ClusterID FROM fzzlKMeansClusterID WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3",sep = "");
			KMeansClusterID  <- sqlQuery(DBConnection, SQLStr);

			object@centers = KMeansDendrogram;
			object@cluster = KMeansClusterID;
			object
          }
)
# define FLDecisionTree Class
setClass("FLDecisionTree", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						dt.node.info  = "data.frame",
						dt.obs.classification = "data.frame"))
						
# fetch_results method for FLDecisionTree
setMethod("fetch.results",
          signature("FLDecisionTree"),
          function(object) {
      DBConnection <- object@ODBCConnection;            
      #Fetch Decision Tree Analysis Result Table
			SQLStr <- paste("SELECT TreeLevel, NodeID, ParentNodeID, IsLeaf, SplitVarID, SplitVal, ChildNodeLeft, ChildNodeRight, NodeSize, PredictClass, PredictClassProb FROM fzzlDecisionTreeMN WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			NodeInfo <- sqlQuery(DBConnection, SQLStr);
			SQLStr <- paste("SELECT ObsID, ObservedClass, NodeID, PredictedClass, PredictClassProb FROM fzzlDecisionTreeMNPred WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			ObservationClassification <- sqlQuery(DBConnection, SQLStr);
			object@dt.node.info = NodeInfo;
			object@dt.obs.classification = ObservationClassification;
			
			#print(paste(object@AnalysisID));
			object
          }
)

# define FLLogRegr Class
setClass("FLLogRegr", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						logregr.coeffs  = "data.frame",
						logregr.stats = "data.frame"))
						
# fetch_results method for FLLogRegr
setMethod("fetch.results",
          signature("FLLogRegr"),
          function(object) {
      DBConnection <- object@ODBCConnection;            
      #Fetch Logistic Regression Analysis Result Table
			SQLStr <- paste("SELECT MODELID, COEFFID, COEFFVALUE, STDERR, CHISQ, PVALUE FROM fzzlLogRegrCoeffs WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			CoeffsValue <- sqlQuery(DBConnection, SQLStr);
			SQLStr <- paste("SELECT MODELID, NUMOFVARS, ITERATIONS, CONCORDANT, DISCORDANT, TIED, TOTALPAIRS, GINICOEFF, CSTATISTIC, GAMMA, HIGHESTPVALUE, EVENTS, NONEVENTS, NUMOFOBS, FALSEPOSITIVE, FALSENEGATIVE FROM fzzlLogRegrStats WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			LogRegrStats <- sqlQuery(DBConnection, SQLStr);
			object@logregr.coeffs = CoeffsValue;
			object@logregr.stats = LogRegrStats;
			
			#print(paste(object@AnalysisID));
			object
          }
)

# define FLLDA Class
setClass("FLLDA", 
		slots = list(	ODBCConnection = "RODBC",
						AnalysisID     = "character", 
						lda.canonical.coeffs  = "data.frame",
						lda.fisher.coeffs = "data.frame",
						lda.canonical.variates  = "data.frame",
						lda.predicted.vs.observed = "data.frame"))
# fetch_results method for FLLDA
setMethod("fetch.results",
          signature("FLLDA"),
          function(object) {
      DBConnection <- object@ODBCConnection;            
      #Fetch LDA Result Table
			SQLStr <- paste("SELECT CANTYPE, VARID, CANID, NUM_VAL FROM fzzlLDACanCoeff WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3,4",sep = "");
			CanonicalCoeffs <- sqlQuery(DBConnection, SQLStr);
			SQLStr <- paste("SELECT CATNUM_VAL, COEFFID, COEFFNUM_VAL FROM fzzlLDAFisherCoeffs WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3",sep = "");
			FisherCoeffs <- sqlQuery(DBConnection, SQLStr);
			SQLStr <- paste("SELECT OBSID, VARID, NUM_VAL FROM fzzlLDACanVariate WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3",sep = "");
			CanonicalVariates <- sqlQuery(DBConnection, SQLStr);
			SQLStr <- paste("SELECT Y, PREDICTEDY, OBS_COUNT FROM fzzlLDACrossTab WHERE AnalysisID = '", object@AnalysisID,"' ORDER BY 1,2,3",sep = "");
			PredVsObs <- sqlQuery(DBConnection, SQLStr);
			object@lda.canonical.coeffs = CanonicalCoeffs;
			object@lda.fisher.coeffs = FisherCoeffs;
			object@lda.canonical.variates = CanonicalVariates;
			object@lda.predicted.vs.observed = PredVsObs;
			
			#print(paste(object@AnalysisID));
			object
          }
)