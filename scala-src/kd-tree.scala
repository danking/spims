object KdTreeWrapper {

  // def isLessThanOrEqualTo(n: Node) {
  //   (position zip n.position).forall(case (x, y) => x <= y)
  // }

  // def isGreaterThan(n: Node) {
  //   (position zip n.position).forall(case (x, y) => x > y)
  // }

  def distance (u: Seq[Float], v: Seq[Float]) = {
    val sqrs = (u.elements zip v.elements).map({case ((_, u), (_, v)) => (u - v) * (u - v)})
    val sum = sqrs.foldLeft(0.0.toFloat)((acc, x) => (acc + x))
    Math.sqrt(sum)
  }

  def createKdTree(vs: Seq[(Map[Int, Float], Map[Int, Float])]): Option[KdTree] = {
    createKdTree(vs, 0)
  }

  type MyTuple = (Map[Int, Float], Map[Int, Float])
  def createKdTree(vs: Seq[(Map[Int, Float], Map[Int, Float])], split: Int): Option[KdTree] = {
    createKdTreeHelp(scala.util.Sorting.stableSort(vs, { (a: MyTuple, b: MyTuple) =>
                                  val (x,_) = a
                                  val (y,_) = b
                                  x(split) < y(split) }),
                     split)
  }

  private def createKdTreeHelp(sortedVs: Seq[(Map[Int, Float], Map[Int, Float])], split: Int): Option[KdTree] = {
    if (sortedVs.isEmpty) {
      return None
    } else {
      val median = sortedVs.length/2
      val (position,value) = sortedVs(median)
      val dim = position.size
      val left = createKdTree(sortedVs.slice(0, median), (split + 1) % dim)
      val right = createKdTree(sortedVs.slice(median+1, sortedVs.length), (split + 1) % dim)

      Some(new KdTree(position, value, split, left, right))
    }
  }

  class KdTree(val position: Map[Int, Float], val value: Map[Int, Float], split: Int, left: Option[KdTree], right: Option[KdTree]) {
    val dim = position.size

    def isLessThanOrEqualTo(n: KdTree) {
      this.position(split) <= n.position(split)
    }

    def isGreaterThan(n: KdTree) {
      this.position(split) > n.position(split)
    }

    def nearestneighbor(target: Map[Int, Float], boundingBox: (Map[Int, Float], Map[Int, Float])): KdTree = {
      val (boxMin, boxMax) = boundingBox
      val lowerBox = (boxMin.update(split, position(split)),
                      boxMax)
      val upperBox = (boxMin,
                      boxMax.update(split, position(split)))


      val (subNearest, unsearchedKdTree, unsearchedBox) =
        { if (target(this.split) < this.position(this.split))
            this.left match {
              case None => (this, this.right, upperBox)
              case Some(left) =>
                (left.nearestneighbor(target, lowerBox), this.right, upperBox)
            }
          else
            this.right match {
              case None => (this, this.left, lowerBox)
              case Some(right) =>
                (right.nearestneighbor(target, upperBox), this.left, lowerBox)
            }
        }

      val nearest =
        if (distance(target, this.position) < distance(target, subNearest.position)) {
          this
        } else {
          subNearest
        }

      /* now we must check to be certain there are no hyperplanes within a
       hypersphere of radius dist */

      /* the maximum distance that any potentially closer matches could be within */
      val maxdist = distance(target, nearest.position)

      val pointClosestToSphereButInPlane =
        for ((((_, x), (_, min)), (_, max)) <- target.elements zip boxMin.elements zip boxMax.elements)
        yield {
          if (x < min) min
          else if (x > max) max
          else x }

          if (distance(target.elements map {case (_, x) => x}, pointClosestToSphereButInPlane) < maxdist) {
        /* then there might be a point in the other plane that is closer */
        val newNearest =
          unsearchedKdTree match {
            case None => nearest
            case Some(kdtree) =>
              kdtree.nearestneighbor(target, unsearchedBox)
          }

        if (distance(newNearest.position, target) > maxdist)
          nearest
        else
          newNearest
      } else {
        nearest
      }
    }
  }
}
