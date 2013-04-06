Object spims {
	def main (argv:Array[String]):Unit = {
		if(CommandLineParser.parse(argv)) {
		// try {
			for (patternFileName <- patternFileNames){
				if (debug){
					println ("Searching for: " patternFileName)
				}
				for (sourceFileName <- sourceFileNames){
					if (debug) {
						println("Looking in: " sourceFileName)
					}
					Output.printMatches(filePathPairToMatches(sourceFileName, patternFileName))
				}
			}
		// } catch {
		// 	if (debug) {
		// 		println(_)
		// 	}
		// }
	}
}

	def formatMatch(sourceFileName: String, patternFileName: String, prematch: Prematch,
		matchWidth: Int, matchHeight: Int): Match = {
		return new Match(sourceFileName, patternFileName, matchWidth, matchHeight, prematch.x, prematch.y)
	}

	def filePathPairToMatches(sourceFileName: String, patternFileName: String): List[Match] = {
		val sourceImage = Bitmap(load(sourceFileName))
		val patternImage = Bitmap(load(patternFileName))
		val results = List[Match]()
		for (prematch <- Algorithm.finePatternInSource(sourceImage, patternImage)) {
			results.::(formatMatch(sourceFileName, patternFileName, prematch, patternImage.width, patternImage.height))
		}
		return results
	}
}