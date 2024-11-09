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

  // Build uniform belief state .
  def uniformBelief(nags: Int): SpecificBelief = {
    Vector.tabulate(nags)((i: Int) =>
      (i + 1).toDouble / nags.toDouble)
  }

  // Builds mildly polarized belief state , in which
  // half of agents has belief decreasing from 0.25, and
  // half has belief increasing from 0.75, all by the given step.
  def midlyBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2
    Vector.tabulate(nags)((i: Int) =>
      if (i < middle) Math.max(0.25-0.01*(middle-i-1), 0)
      else Math.min(0.75-0.01*(middle-i), 1))
  }

  //Builds extreme polarized belief state , in which half
  // of the agents has belief 0, and half has belief 1.
  def allExtremeBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2
    Vector.tabulate(nags)((i: Int) =>
      if (i < middle) 0.0 else 1.0)
  }

  // Builds three pole belief state , in which each
  // one third of the agents has belief 0, one third has belief 0.5,
  // and one third has belief 1.
  def allTripleBelief(nags: Int): SpecificBelief = {
  val oneThird = nags / 3
  val twoThird = (nags / 3)*2
  Vector.tabulate(nags)((i: Int) =>
    if (i < oneThird) 0.0
    else if (i >= twoThird) 1.0
      else 0.5)
  }

  // Builds consensus belief state , in which each
  // All agents have same belief.
  def consensusBelief(b:Double)(nags: Int ):SpecificBelief = {
    Vector.tabulate(nags)((i: Int) => b)
  }

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

  /*def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {

  }

  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {

  }

  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {

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