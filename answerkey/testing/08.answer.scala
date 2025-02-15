def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A]
  val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
  State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)
