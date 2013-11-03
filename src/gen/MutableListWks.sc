import scala.collection.mutable

val ml = new mutable.MutableList[Int]
for (i <- 1 to 10) ml += i
ml.size
ml.toList