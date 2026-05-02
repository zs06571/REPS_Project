// Shuo Zhao 002513249
// Hongyao Liu 002513919 
// Hongrui Zhang 002520436

package com.reps

object AnalyticsService {

  // Analyze valid generation records only
  def analyzeGeneration(records: List[EnergyRecord]): Option[AnalysisResult] = {
    val validRecords = records.filter(_.isValidForAnalysis)
    val values = validRecords.map(_.actualGeneration)

    // Return None if there is no valid data
    if (values.isEmpty) {
      None
    } else {
      val sorted = values.sorted
      val totalRecords = values.length
      val mean = values.sum / totalRecords

      // Find the middle value, or average the two middle values
      val median =
        if (totalRecords % 2 == 1) {
          sorted(totalRecords / 2)
        } else {
          val mid1 = sorted(totalRecords / 2 - 1)
          val mid2 = sorted(totalRecords / 2)
          (mid1 + mid2) / 2.0
        }

      // Count the frequency of each value and take the most frequent one
      val frequencyMap = values.groupBy(identity).view.mapValues(_.size).toMap
      val mode = frequencyMap.maxBy(_._2)._1

      // Range = max - min
      val minValue = values.min
      val maxValue = values.max
      val range = maxValue - minValue

      // Midrange = (min + max) / 2
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
