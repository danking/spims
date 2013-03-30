import java.awt.{Graphics2D, Image}
import java.awt.image.BufferedImage
import Bitmap._
import Sift._

object Pyramid {
  val Octaves = 5
  val Intervals = 4

  type Pyramid = Array[Array[Bitmap]]
  type Matrix = Array[Array[Double]]
  type Histogram = Array[Double]
  type SuperHistogram = Array[Array[Histogram]]

  case class DetectionData(x: Int, y: Int, octv: Int, intvl: Int, subIntvl: Int, sclOctv: Double)
  case class Feature(dd: DetectionData, x: Double, y: Double, scl: Double, ori: Double, descr: Array[Int])

  def buildGaussPyr(base: BufferedImage, octvs: Int, intvls: Int, sigma: Double) = {
    val k = Math.sqrt(2)
    (for {
      (o, s) <- (0 until octvs) map { x => (x, Math.pow(0.5, x)) }
    } yield {
      val scaledImg = {
        val copy = deepCopy(base)
        val grph = copy.createGraphics
        grph.scale(s, s)
        grph.dispose
        copy
      }
      (for {
        i <- 0 until intvls
      } yield Bitmap(gaussianBlur(scaledImg, (sigma * Math.pow(k, i)).toFloat))).toArray
    }).toArray
  }

  def buildDogPyr(gaussPyr: Pyramid) = for (o <- gaussPyr) yield (o zip o.slice(1, o.length)) map { case (i1, i2) => subtract(i2, i1) }

  def isExtremum(dogPyr: Pyramid, octv: Int, intvl: Int, x: Int, y: Int): Boolean = {
    val pix = dogPyr(octv)(intvl).getGrey(x, y)
    var (i, j, k) = (-1, -1, -1)

    if (pix > 0) {
      while (i <= 1)
        while (j <= 1)
          while (k <= 1) {
            if (pix <  dogPyr(octv)(intvl + i).getGrey(x + j, y + k))
              return false
            k = k + 1
            j = j + 1
            i = i + 1
          }
    } else {
      while (i <= 1)
        while (j <= 1)
          while (k <= 1) {
            if (pix >  dogPyr(octv)(intvl + i).getGrey(x + j, y + k))
              return false
            k = k + 1
            j = j + 1
            i = i + 1
          }
    }
    false
  }

  def deriv3D(dogPyr: Pyramid, octv: Int, intvl: Int, x: Int, y: Int) = {
    val dx = ( dogPyr(octv)(intvl).getGrey(x, y + 1) -
               dogPyr(octv)(intvl).getGrey(x, y - 1) ) / 2.0
    val dy = ( dogPyr(octv)(intvl).getGrey(x + 1, y) -
               dogPyr(octv)(intvl).getGrey(x - 1, y) ) / 2.0
    val ds = ( dogPyr(octv)(intvl + 1).getGrey(x, y) -
               dogPyr(octv)(intvl - 1).getGrey(x, y) ) / 2.0
    Array(Array(dx, dy, ds))
  }

  def hessian3D(dogPyr: Pyramid, octv: Int, intvl: Int, x: Int, y: Int) = {
    val v = dogPyr(octv)(intvl).getGrey(x, y)
    val dxx = ( dogPyr(octv)(intvl).getGrey(x, y + 1) +
                dogPyr(octv)(intvl).getGrey(x, y - 1) - 2.0 * v )
    val dyy = ( dogPyr(octv)(intvl).getGrey(x + 1, y) +
                dogPyr(octv)(intvl).getGrey(x - 1, y) - 2.0 * v )
    val dss = ( dogPyr(octv)(intvl + 1).getGrey(x, y) +
                dogPyr(octv)(intvl - 1).getGrey(x, y) - 2.0 * v )
    val dxy = ( dogPyr(octv)(intvl).getGrey(x + 1, y + 1) -
                dogPyr(octv)(intvl).getGrey(x + 1, y - 1) -
                dogPyr(octv)(intvl).getGrey(x - 1, y + 1) +
                dogPyr(octv)(intvl).getGrey(x - 1, y - 1) ) / 4.0
    val dxs = ( dogPyr(octv)(intvl + 1).getGrey(x, y + 1) -
                dogPyr(octv)(intvl + 1).getGrey(x, y - 1) -
                dogPyr(octv)(intvl - 1).getGrey(x, y + 1) +
                dogPyr(octv)(intvl - 1).getGrey(x, y - 1) ) / 4.0
    val dys = ( dogPyr(octv)(intvl + 1).getGrey(x + 1, y) -
                dogPyr(octv)(intvl + 1).getGrey(x - 1, y) -
                dogPyr(octv)(intvl - 1).getGrey(x + 1, y) +
                dogPyr(octv)(intvl - 1).getGrey(x - 1, y) ) / 4.0
    Array(Array(dxx, dxy, dxs),
          Array(dxy, dyy, dys),
          Array(dxs, dys, dss))
  }

  def transpose(m: Matrix) = {
    (for (j <- 0 until m(0).length) yield
      (for (i <- 0 until m.length) yield
        m(i)(j)).toArray).toArray
  }

  def gemm(a: Matrix, b: Matrix, scalar: Double): Matrix = {
    (for (i <- 0 until a.length) yield // Rows of a
      (for (j <- 0 until b(0).length) yield // Columns of b
        (for (k <- 0 until a(0).length) yield scalar * a(i)(k) * b(k)(j)).foldLeft(0.0)(_ + _)).toArray).toArray
  }

  def interpStep(dogPyr: Pyramid, octv: Int, intvl: Int, x: Int, y: Int) = {
    def inv3x3(matrix: Matrix)= {
      val (a, b, c) = (matrix(0)(0), matrix(0)(1), matrix(0)(2))
      val (d, e, f) = (matrix(1)(0), matrix(1)(1), matrix(1)(2))
      val (g, h, k) = (matrix(2)(0), matrix(2)(1), matrix(2)(2))

      val det = a * (e * k - f * h) - b * (k * d - f * g) + c * (d * h - e * g)

      if (det == 0) throw new Exception("Cannot have a determinant of 0. Uninvertable matrix.")

      val m = Array( Array( (e * k - f * h), (c * h - b * k), (b * f - c * e) ),
                     Array( (f * g - d * k), (a * k - c * g), (c * d - a * f) ),
                     Array( (d * h - e * g), (g * b - a * h), (a * e - b * d) ) )
      m map { arr => arr map { x => x / det } }
    }

    val dD = transpose(deriv3D(dogPyr, octv, intvl, x, y))
    val H = hessian3D(dogPyr, octv, intvl, x, y)
    val invH = inv3x3(H)

    transpose(gemm(invH, dD, -1.0))
  }

  def interpContr(dogPyr: Pyramid, octv: Int, intvl: Int, x: Int, y: Int, xm: Matrix) = {
    val txm = transpose(xm)
    val dD = deriv3D(dogPyr, octv, intvl, x, y)
    val t = gemm(dD, txm, 1.0)(0)(0) // 1x1 Matrix
    dogPyr(octv)(intvl).getGrey(x, y) * t * 0.5
  }

  def interpExtremum(dogPyr: Pyramid, octv: Int, intvl: Int, x: Int, y: Int, intvls: Int, contrThr: Double): Option[Feature] = {
    var i = 0
    var (r, c) = (x, y)
    var intvl2 = intvl

    var xm = interpStep(dogPyr, octv, intvl2, x, y)
    while ( (i < SiftMaxInterpSteps) &&
            (xm(0) forall { x => Math.abs(x) >= 0.5 }) ) {

      c = c + Math.round(xm(0)(0)).toInt
      r = r + Math.round(xm(0)(1)).toInt
      intvl2 = intvl2 + Math.round(xm(0)(2)).toInt

      if (intvl2 < 1  ||
          intvl2 > intvls  ||
          c < SiftImgBorder  ||
          r < SiftImgBorder  ||
          c >= dogPyr(octv)(0).width - SiftImgBorder  ||
          r >= dogPyr(octv)(0).height - SiftImgBorder)
      return None

      i = i + 1
      xm = interpStep(dogPyr, octv, intvl2, x, y)
    }

    /* ensure convergence of interpolation */
    if(i >= SiftMaxInterpSteps)
      return None

    val contr = interpContr(dogPyr, octv, intvl2, r, c, xm)
    if(Math.abs(contr) < contrThr / intvls)
      return None

    Some(Feature(DetectionData(r, c, octv, intvl2, xm(0)(2).toInt, 0),
                 ((c + xm(0)(0)) * Math.pow(2.0, octv)).toInt,
                 ((r + xm(0)(1)) * Math.pow(2.0, octv)).toInt,
                 0.0, 0.0, null))
  }

  def scaleSpaceExtrema(dogPyr: Pyramid, octvs: Int, intvls: Int, contrThr: Double, curvThr: Int) = {
    val prelimContrThr = 0.5 * contrThr / intvls
    (for {
      o <- 0 until octvs
      i <- 0 until intvls
      r <- SiftImgBorder until dogPyr(o)(0).height - SiftImgBorder
      c <- SiftImgBorder until dogPyr(o)(0).width - SiftImgBorder
      if ((Math.abs(dogPyr(o)(i).getGrey(r, c)) > prelimContrThr) &&
           isExtremum(dogPyr, o, i, r, c))
    } yield
      interpExtremum(dogPyr, o, i, r, c, intvls, contrThr) match {
        case None => None
        case Some(f) => {
          if (!isTooEdgeLike(dogPyr(f.dd.octv)(f.dd.intvl), f.dd.x, f.dd.y, curvThr))
            Some(f)
          else None
        }
      }) filter(_ != None)
  }

  def isTooEdgeLike(dogImg: Bitmap, x: Int, y: Int, curvThr: Int) = {
    val d   = dogImg.getGrey(x, y)
    val dxx = dogImg.getGrey(x, y + 1) + dogImg.getGrey(x, y - 1) - 2 * d
    val dyy = dogImg.getGrey(x + 1, y) + dogImg.getGrey(x - 1, y) - 2 * d
    val dxy = (dogImg.getGrey(x + 1, y + 1) - dogImg.getGrey(x + 1, y - 1) -
               dogImg.getGrey(x - 1, y + 1) + dogImg.getGrey(x - 1, y - 1)) / 4.0
    val tr = dxx + dyy
    val det = dxx * dyy - dxy * dxy

    /* negative determinant -> curvatures have different signs; reject feature */
    if (det <= 0)
      true
    else if (tr * tr / det < (curvThr + 1.0) * (curvThr + 1.0) / curvThr)
      false
    else true
  }

  def calcFeatureScales(features: Seq[Feature], sigma: Double, intvls: Int) = {
    for (f <- features) yield {
      val intvl = f.dd.intvl + f.dd.subIntvl
      val scl = sigma * Math.pow(2.0, f.dd.octv + intvl / intvls)
      val sclOctv = sigma * Math.pow(2.0, intvl / intvls)
      val dData = DetectionData(f.dd.x, f.dd.y, f.dd.octv, f.dd.intvl, f.dd.subIntvl, sclOctv)
      Feature(dData, f.x, f.y, scl, 0.0, null)
    }
  }

  def adjustForImgDbl(features: Seq[Feature]) = {
    for (f <- features) yield {
      Feature(f.dd, f.x / 2.0, f.y / 2.0, f.scl / 2.0, 0.0, null)
    }
  }

  def calcGradMagOri(img: Bitmap, x: Int, y: Int): Option[(Double, Double)] = {
    if (x > 0 && x < img.height - 1 && y > 0 && y < img.width - 1) {
      val dx = img.getGrey(x, y + 1) - img.getGrey(x, y - 1)
      val dy = img.getGrey(x - 1, y) - img.getGrey(x + 1, y)
      Some((Math.sqrt(dx * dx + dy * dy), Math.atan2(dy, dx)))
    }
    None
  }

  def oriHist(img: Bitmap, x: Int, y: Int, n: Int, rad: Int, sigma: Double) = {
    val hist = new Array[Double](n)
    for {
      i <- -rad until rad
      j <- -rad until rad
    } {
      calcGradMagOri(img, x + i, y + j) match {
        case None =>
        case Some((m, o)) => {
          val w  = Math.exp(-1 * (i * i + j * j) / (2 * sigma * sigma))
          val bin = Math.round(n * (o + Math.Pi / (2 * Math.Pi))).toInt
          val bin2 =  if (bin < n) bin else 0
          hist(bin2) = hist(bin2) + w * m
        }
      }
    }
    hist
  }

  def smoothOriHist(hist: Histogram, n: Int) {
    val h0 = hist(0)
    var prev = hist(n - 1)

    var i = 0
    while (i < n) {
      val tmp = hist(i)
      hist(i) = 0.25 * prev  + 0.5  * hist(i) +
                0.25 * (if (i + 1 == n) h0 else hist(i + 1))
      prev = tmp
      i = i + 1
    }
  }

  def dominantOri(hist: Histogram) = {
    hist.reduceLeft ( _ max _ )
  }

  def goodOriFeatures(hist: Histogram, magThr: Double, feat: Feature) = {
    val n = hist.length
    def isGoodP(i: Int) = {
      val l = if (i == 0) n - 1 else i - 1
      val r = (i + 1) % n

      hist(i) > hist(l) && hist(i) > hist(r) && hist(i) >= magThr
    }

    for(i <- 0 until n
        if isGoodP(i)) yield {
      val l = if (i == 0) n - 1 else i - 1
      val r = (i + 1) % n
      val bin = i + interpHistPeak( hist(l), hist(i), hist(r) )
      val bin2 = {
        if ( bin < 0 ) n + bin
        else if ( bin >= n ) bin - n
        else bin }
      val ori = ((Math.Pi * 2 * bin) / n) - Math.Pi
      Feature(feat.dd, feat.x, feat.y, feat.scl, ori, null)
    }
  }

  def calcFeatureOris(features: Seq[Feature], gaussPyr: Pyramid) = {
    features flatMap { f =>
      val dd = f.dd
      val hist = oriHist(gaussPyr(dd.octv)(dd.intvl), dd.x, dd.y, SiftOriHistBins, (Math.round(SiftOriRadius * dd.sclOctv)).toInt,
                         SiftOriSigFctr * dd.sclOctv)
      for (i <- 0 until SiftOriSmoothPasses)
        smoothOriHist(hist, SiftOriHistBins)
      val oMax = dominantOri(hist)
      goodOriFeatures(hist, oMax * SiftOriPeakRatio, f)
    }
  }

  def interpHistPeak(l: Double, c: Double, r:Double) = (0.5 * (l - r) / (l - 2.0 * c + r))

  def interpHistEntry(hist: SuperHistogram, rBin: Double, cBin: Double, oBin: Double,
                      mag: Double, d: Int, n: Int) = {
    val r0 = Math.floor(rBin).toInt
    val c0 = Math.floor(cBin).toInt
    val o0 = Math.floor(oBin).toInt

    val dR = rBin - r0
    val dC = cBin - c0
    val dO = oBin - o0

    for (r <- 0 to 1) {
      val rb = r0 + r
      if (rb >= 0 && rb < d) {
        val vR = mag * (if (r == 0) 1.0 - dR else dR)
        val row = hist(rb)
        for (c <- 0 to 1) {
          val cb = c0 + c
          if (cb >= 0 && cb < d) {
            val vC = vR * (if (c == 0) 1.0 - dC else dC)
            val h = row(cb)
            for (o <- 0 to 1) {
              val ob = (o0 + o) % n
              val vO = vC * (if(o == 0) 1.0 - dO else dO )
              h(ob) = h(ob) + vO
            }
          }
        }
      }
    }
  }

  def descrHist(img: Bitmap, x: Int, y: Int, ori: Double, scl: Double, d: Int, n: Int) = {
    val cosT = Math.cos(ori)
    val sinT = Math.sin(ori)
    val PI2 = 2.0 * Math.Pi
    val binsPerRad = n / PI2
    val expDenom = d * d * 0.5
    val histWidth = SiftDescrSclFctr * scl
    val hist = (for (i <- 0 until d) yield
                 (for (j <- 0 until d) yield new Array[Double](n)).toArray).toArray
    val radius = (histWidth * Math.sqrt(2) * (d + 1.0) * 0.5 + 0.5).toInt
    for {
      i <- -radius to radius
      j <- -radius to radius
    } {
      val cRot = (j * cosT - i * sinT) / histWidth
      val rRot = (j * sinT + i * cosT) / histWidth
      val rBin = rRot + d / 2 - 0.5
      val cBin = cRot + d / 2 - 0.5
      if (rBin > -1.0 && rBin < d && cBin > -1.0 && cBin < d)
        calcGradMagOri(img, x + i, y + j) match {
          case None =>
          case Some((m, o)) => {
            var gradOri = o - ori
            while (gradOri < 0.0)
              gradOri = gradOri + PI2
            while (gradOri >= PI2)
              gradOri = gradOri - PI2

            val oBin = gradOri * binsPerRad
            val w = Math.exp(-(cRot * cRot + rRot * rRot) / expDenom)
            interpHistEntry(hist, rBin, cBin, oBin, m * w, d, n)
          }
        }
    }
  hist
  }

  def normalizeDescr(descr: Histogram) = {
    val lenSq = (0.0 /: descr)((x, y) => x*x + y)
    val lenInv = 1.0 / Math.sqrt(lenSq)

    descr map (x => x * lenInv)
  }

  def featureCmp(f1: Feature, f2: Feature) = {
    if (f1.scl < f2.scl)
      1
    else if (f1.scl > f2.scl)
      -1
    else
      0
  }

  def histToDescr(hist: SuperHistogram, d: Int, n: Int, feat: Feature) = {
    val flat = hist.flatMap(x => x).flatMap(x => x)
    val normed1 = normalizeDescr(flat)
    val clipped = normed1.map(x => { if (x > SiftDescrMagThr) SiftDescrMagThr else x })
    val normed2 = normalizeDescr(clipped)
    val intHist = normed2.map(x => Math.min(255, (x * SiftIntDescrFctr).toInt))

    Feature(feat.dd, feat.x, feat.y, feat.scl, feat.ori, intHist)
  }

  def computeDescriptors(features: Seq[Feature], gaussPyr: Pyramid, d: Int, n: Int) = {
    for (f <- features) yield {
      val dd = f.dd
      val hist = descrHist(gaussPyr(dd.octv)(dd.intvl), dd.x, dd.y, f.ori, dd.sclOctv, d, n)
      histToDescr(hist, d, n, f)
    }
  }
}
