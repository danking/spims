class Match(source: String, pattern: String, x: Int, y: Int, m1: Int, n1: Int) {

	private val LOWER_SIZE_TOLERANCE : Double = 1.2
	private val UPPER_SIZE_TOLERANCE : Double = .8
	private val LOCATION_INTOLERANCE : Double = 15
	private def pythagorean(x: Int, y: Int) : Double = {
		return Math.sqrt((x * x) + (y * y))
	}

	def duplicate(m1: Match) : Boolean = {
		return !(this.sameLoc(m1) && this.sameSize(m1))
	}

	private def sameLoc(m1: Match) : Boolean = {
		return (pythagorean((m1.x - this.x), (m1.y - this.y)) <
			(pythagorean(this.x, this.y) / LOCATION_INTOLERANCE))
	}

	private def sameSize(m1: Match) : Boolean = {
		val sizeRatio = Math.abs(pythagorean(this.x, this.y) / pythagorean(m1.x, m1.y))
		return sizeRatio < UPPER_SIZE_TOLERANCE && sizeRatio > LOWER_SIZE_TOLERANCE
	}
}

object Output {

	private def printMatch(result : Match) {
		println(result.pattern + " matches " + result.source +
			" at " +
			result.m1 + "x" + result.n1 +
			"+" +
			result.x + "+" + result.y)
	}

	def printMatches(results: List[Match]) = {
		for (x <- 0 until results.length - 1 ) {
			val m = results.apply(x)
			results.filterNot(m.duplicate)
			printMatch(m)
		}
	}
}