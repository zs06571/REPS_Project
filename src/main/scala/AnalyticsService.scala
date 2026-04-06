package com.reps

object AnalyticsService {

  def extractGeneration(records: List[EnergyRecord]): List[Double] = {
    records.map(_.generation)
  }

  def mean(values: List[Double]): Option[Double] = {
    if (values.isEmpty) None
    else Some(values.sum / values.length)
  }

  def median(values: List[Double]): Option[Double] = {
    if (values.isEmpty) {
      None
    } else {
      val sorted = values.sorted
      val mid = sorted.length / 2

      if (sorted.length % 2 == 0)
        Some((sorted(mid - 1) + sorted(mid)) / 2)
      else
        Some(sorted(mid))
    }
  }

  def mode(values: List[Double]): Option[Double] = {
    if (values.isEmpty) None
    else Some(values.groupBy(identity).maxBy(_._2.size)._1)
  }

  def range(values: List[Double]): Option[Double] = {
    if (values.isEmpty) None
    else Some(values.max - values.min)
  }

  def midrange(values: List[Double]): Option[Double] = {
    if (values.isEmpty) None
    else Some((values.max + values.min) / 2)
  }

  def analyzeGeneration(records: List[EnergyRecord]): Option[AnalysisResult] = {
    val values = extractGeneration(records)

    for {
      avg <- mean(values)
      med <- median(values)
      mod <- mode(values)
      ran <- range(values)
      mid <- midrange(values)
    } yield AnalysisResult(avg, med, mod, ran, mid, values.length)
  }
}