package object Comete {
  type DistributionValues = Vector[Double]
  // Tipo para los valores, reales ordenados entre 0 y 1,
  // incluidos 0 y 1, de una distribucion

  type Frequency = Vector[Double]
  // Pi k es una frecuencia de longitud k
  // si Pi k.lenght=k, 0<=Pi k(i)<=1, 0<=i<=k-1
  // y Pi k.sum==1

  type Distribution = (Frequency, DistributionValues)
  // (Pi, dv) es una distribucion si pi es una Frecuencia
  // y dv son los valores de distribucion y pi y dv son
  // de la misma longitud

  type PolMeasure = Distribution => Double

  def min_p(f:Double=>Double, min:Double, max:Double, prec:Double):Double = {
    // Devuelve el punto p en [min,max] tal que f(p) es minimo
    // Suponiendo que f es convexa
    // si max-min<prec, devuelve el punto medio de [min, max]
    if ((max-min)<prec) (min+max)/2.0 else {
      val divideBy = 20
      val intervalLength = (max-min)/divideBy

      val points = (BigDecimal(min) to BigDecimal(max) by intervalLength).map(x => (x.doubleValue, f(x.doubleValue)))

      val (minP, fx) = points.minBy(_._2)

      if (fx == 0) minP else {
        val newMin = Math.max(min, minP - intervalLength / 2.0)
        val newMax = Math.min(max, minP + intervalLength / 2.0)
        min_p(f, newMin, newMax, prec)
      }
    }
  }

  def rhoCMT_Gen(alpha:Double, beta:Double):PolMeasure = {
    // Dados alpha y beta devuelve la funcion que calcula la medida
    // comete parametrizada en alpha y beta
    (distribution: Distribution) => {
      val frequency = distribution._1
      val distributionValues = distribution._2
      val tuples = frequency.zip(distributionValues)

      def sum(p: Double, tuples: Seq[(Double, Double)]): Double = {
        tuples.map{ case (pi,y) => Math.pow(pi, alpha) * Math.pow(Math.abs(y-p), beta)
        }.sum
      }

      val f = (p: Double) => sum(p, tuples)

      val min = min_p(f, 0.0, 1.0, 0.1)

      BigDecimal(f(min)).setScale(3, BigDecimal.RoundingMode.HALF_UP).doubleValue
    }
  }

  def normalizar(m:PolMeasure):PolMeasure = {
    // Recibe una medida de polarizacion, y devuelve la
    // correspondiente medida que la calcula normalizada
    (distribution: Distribution) => {
      val distributionValues = distribution._2
      val k = distributionValues.length

      val worstCaseFreq = Vector(0.5) ++ Vector.fill(k - 2)(0.0) ++ Vector(0.5)
      val worstCasePol = m((worstCaseFreq, distributionValues))

      val pol = m(distribution)
      val normalizedPol = pol/worstCasePol

      BigDecimal(normalizedPol).setScale(3, BigDecimal.RoundingMode.HALF_UP).doubleValue
    }
  }
}
