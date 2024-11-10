import Comete._
import common._
import scala.collection.parallel.CollectionConverters._
package object Opinion {
  // Si n es el numero de agentes, estos se identifican
  // con los enteros entre 0 y n 1
  // O sea el conjunto de Agentes A es
  // implicitamente el conjunto {0, 1, 2, ..., n-1}
  // Si b:BeliefConf, para cada i en Int, b[i] es un numero
  // entre 0 y 1 que indica cuanto cree el agente i en
  // la veracidad de la proposicion p
  // Si existe i: b(i)<0 o b(i)>1 b esta mal definida

  type SpecificBelief = Vector[Double]
  // Si b:SpecificBelief, para cada i en Int, b[i] es un
  // numero entre 0 y 1 que indica cuanto cree el
  // agente i en la veracidad de la proposicion p
  // El numero de agentes es b.length
  // Si existe i: b(i)<0 o b(i)>1 b esta mal definida.
  // Para i en Int\A, b(i) no tiene sentido

  type GenericBeliefConf = Int => SpecificBelief
  // si gb:GenericBelief, entonces gb(n) = b tal que
  // b:SpecificBelief

  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double
  // Si rho:AgentsPolMeasure y sb:SpecificBelief
  // y d:DistributionValues,
  // rho(sb,d) es la polarizacion de los agentes
  // de acuerdo a esa medida

  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    // en comete
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      val numAgents = specificBelief.length
      val k = distributionValues.length

      val firstInterval = (0.0, distributionValues(1)/2)
      val middleIntervals = (1 to k-2).map(i =>
        ((distributionValues(i)+distributionValues(i-1))/2, (distributionValues(i)+distributionValues(i+1))/2)).toVector
      val lastInterval = ((distributionValues(k-2)+1)/2, 1.0)

      val intervals = firstInterval +: middleIntervals :+ lastInterval

      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap
      val classification = specificBelief.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if(i == k-1) (start <= a && a <= end)
          else (start <= a && a < end)
      })
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      val frequency = finalClassification.map{ case (i, values) => (values.length.toDouble)/numAgents}.toVector

      val rhoAux = rhoCMT_Gen(alpha, beta)
      val normalizarAux = normalizar(rhoAux)
      normalizarAux((frequency, distributionValues))
    }
  }

  // Tipos para Modelar la evolucion de la opinion en una red
  type WeightedGraph = (Int, Int) => Double

  type SpecificWeightedGraph = (WeightedGraph, Int)

  type GenericWeightedGraph = Int => SpecificWeightedGraph

  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val (f, k) = swg
    for (i <- 0 until k) yield {
      for (j <- 0 until k) yield f(i, j)
    }
  }

  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val k = swg._2
    val I = swg._1

    def nbFunc(i: Int): Double = {
      def sum(i: Int): Double = {
        (0 until k).map(j => (1-math.abs(sb(j)-sb(i))) * I(j,i) * (sb(j)-sb(i))).sum
      }

      sb(i) + sum(i)/(i+1)
    }

    (0 until k).map(i => nbFunc(i)).toVector
  }

  /*def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {

  }*/

  // Versiones paralelas
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    // en comete
        (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      val numAgents = specificBelief.length
      val k = distributionValues.length

      val firstInterval = (0.0, distributionValues(1)/2)
      val middleIntervals = (1 to k-2).par.map(i =>
        ((distributionValues(i)+distributionValues(i-1))/2, (distributionValues(i)+distributionValues(i+1))/2)).toVector
      val lastInterval = ((distributionValues(k-2)+1)/2, 1.0)

      val intervals = firstInterval +: middleIntervals :+ lastInterval

      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap
      val classification = specificBelief.par.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if(i == k-1) (start <= a && a <= end)
          else (start <= a && a < end)
      })
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      val frequency = finalClassification.map{ case (i, values) => values.knownSize.toDouble/numAgents}.toVector

      val rhoAux = rhoCMT_Gen(alpha, beta)
      val normalizarAux = normalizar(rhoAux)
      normalizarAux((frequency, distributionValues))
    }
  }

  /*def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {

  }*/
}