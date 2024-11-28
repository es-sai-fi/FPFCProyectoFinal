import Benchmark._
import Opinion._

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val sbms = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val polPar = rhoPar(1.2, 1.2)

val i132768 = i1(32768)
val i232768 = i2(32768)

val sbes = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolsPar = for {
  i <- 0 until sbms.length
} yield simEvolucion(Seq(sbms(i), sbes(i), sbts(i)),
  i232768, 10, polPar, confBiasUpdatePar, likert5,
  "Simulacion Paralela " ++ i.toString ++ "-" ++ sbms(i).length.toString
)