     \name{collarCodes}
     \docType{data}
     \alias{collarCodes}
     \title{collarCodes}
     \description{Relation of Collar-Id and experimental treatment in the MANIP project.}
     \usage{collarCodes}
     \format{data.frame with columns \describe{
		\item{collar}{id of the measured respiration collar, usually an input to the data logger}
		\item{trNutrient}{Nutrient treamtent: N- nitorgen, P phosphorous, NP nitrogen+phosphorous, C: control (none)}
		\item{trBlock}{experimental block (1-4)}
		\item{repl}{replicate (1-2)}
		\item{replId}{combination of trNutrient_trBlock_repl}
		\item{blockId}{combination of trNutrient_trBlock}
		}
     }
     \keyword{datasets}
     \examples{
     	data(collarCodes)
     	head(collarCodes)
     }
      
