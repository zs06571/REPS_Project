package com.reps

object AnalyticsService {

  def analyzeGeneration(records: List[EnergyRecord]): Option[AnalysisResult] = {
    val validRecords = records.filter(_.isValidForAnalysis)
    val values = validRecords.map(_.actualGeneration)

    if (values.isEmpty) {
      None
    } else {
      val sorted = values.sorted
      val totalRecords = values.length
      val mean = values.sum / totalRecords

      val median =
        if (totalRecords % 2 == 1) {
          sorted(totalRecords / 2)
        } else {
          val mid1 = sorted(totalRecords / 2 - 1)
          val mid2 = sorted(totalRecords / 2)
          (mid1 + mid2) / 2.0
        }

      val frequencyMap = values.groupBy(identity).view.mapValues(_.size).toMap
      val mode = frequencyMap.maxBy(_._2)._1

      val minValue = values.min
      val maxValue = values.max
      val range = maxValue - minValue
      val midrange = (minValue + maxValue) / 2.0

      Some(
        AnalysisResult(
          mean = mean,
          median = median,
          mode = mode,
          range = range,
          midrange = midrange,
          totalRecords = totalRecords
        )
      )
    }
  }
}