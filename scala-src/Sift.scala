object Sift {
  /** default number of sampled intervals per octave */
  val SiftIntvls = 3

  /** default sigma for initial gaussian smoothing */
  val SiftSigma = 1.6

  /** default threshold on keypoint contrast |D(x)| */
  val SiftContrThr = 0.04

  /** default threshold on keypoint ratio of principle curvatures */
  val SiftCurvThr = 10

  /** double image size before pyramid construction? */
  val SiftImgDbl = 1

  /** default width of descriptor histogram array */
  val SiftDescrWidth = 4

  /** default number of bins per histogram in descriptor array */
  val SiftDescrHistBins = 8

  /* assumed gaussian blur for input image */
  val SiftInitSigma = 0.5

  /* width of border in which to ignore keypoints */
  val SiftImgBorder = 5

  /* maximum steps of keypoint interpolation before failure */
  val SiftMaxInterpSteps = 5

  /* default number of bins in histogram for orientation assignment */
  val SiftOriHistBins = 36

  /* determines gaussian sigma for orientation assignment */
  val SiftOriSigFctr = 1.5

  /* determines the radius of the region used in orientation assignment */
  val SiftOriRadius = 3.0 * SiftOriSigFctr

  /* number of passes of orientation histogram smoothing */
  val SiftOriSmoothPasses = 2

  /* orientation magnitude relative to max that results in new feature */
  val SiftOriPeakRatio = 0.8

  /* determines the size of a single descriptor orientation histogram */
  val SiftDescrSclFctr = 3.0

  /* threshold on magnitude of elements of descriptor vector */
  val SiftDescrMagThr = 0.2

  /* factor used to convert floating-point descriptor to unsigned char */
  val SiftIntDescrFctr = 512.0

  def main(args: Array[String]) {

  }
}
