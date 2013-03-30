object KdTreeWrapper {

  // def isLessThanOrEqualTo(n: Node) {
  //   (position zip n.position).forall(case (x, y) => x <= y)
  // }

  // def isGreaterThan(n: Node) {
  //   (position zip n.position).forall(case (x, y) => x > y)
  // }

  def distance (u: Vector[Float], v: Vector[Float]) = {
    val sqrs = (u zip v).map({case (u, v) => (u - v) * (u - v)})
    val sum = sqrs.sum
    Math.sqrt(sum)
  }

  def createKdTree(vs: Seq[(Vector[Float], Vector[Float])]): Option[KdTree] = {
    createKdTree(vs, 0)
  }

  def createKdTree(vs: Seq[(Vector[Float], Vector[Float])], split: Int): Option[KdTree] = {
    createKdTreeHelp(vs.sortWith({(a, b) =>
                                  val (x,_) = a
                                  val (y,_) = b
                                  x(split) < y(split) }),
                     split)
  }

  private def createKdTreeHelp(sortedVs: Seq[(Vector[Float], Vector[Float])], split: Int): Option[KdTree] = {
    if (sortedVs.isEmpty) {
      return None
    } else {
      val median = sortedVs.length/2
      val (position,value) = sortedVs(median)
      val dim = position.length
      val left = createKdTree(sortedVs.slice(0, median), (split + 1) % dim)
      val right = createKdTree(sortedVs.slice(median+1, sortedVs.length), (split + 1) % dim)

      Some(new KdTree(position, value, split, left, right))
    }
  }

  class KdTree(val position: Vector[Float], val value: Vector[Float], split: Int, left: Option[KdTree], right: Option[KdTree]) {
    val dim = position.length

    def isLessThanOrEqualTo(n: KdTree) {
      this.position(split) <= n.position(split)
    }

    def isGreaterThan(n: KdTree) {
      this.position(split) > n.position(split)
    }

    def nearestneighbor(target: Vector[Float], boundingBox: (Vector[Float], Vector[Float])): KdTree = {
      val (boxMin, boxMax) = boundingBox
      val lowerBox = (boxMin updated (this.split, this.position(this.split)),
                      boxMax)
      val upperBox = (boxMin,
                      boxMax updated (this.split, this.position(this.split)))

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
        for (((x, min), max) <- target zip boxMin zip boxMax)
        yield {
          if (x < min) min
          else if (x > max) max
          else x }

      if (distance(target, pointClosestToSphereButInPlane) < maxdist) {
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
