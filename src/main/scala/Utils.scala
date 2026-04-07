package com.reps

object Utils {

  def printRecords(records: List[EnergyRecord]): Unit = {
    if (records.isEmpty) {
      println("No records found.")
    } else {
      records.foreach { record =>
        val forecastText = record.forecastGeneration match {
          case Some(value) => value.toString
          case None        => "N/A"
        }

        val causeText = record.possibleCause.getOrElse("-")

        println(
          s"Date: ${record.date} | " +
            s"Time: ${record.time} | " +
            s"Type: ${record.energyType} | " +
            s"Actual: ${record.actualGeneration} | " +
            s"Forecast: $forecastText | " +
            s"Forecast enabled: ${record.forecastAvailable} | " +
            s"Status: ${record.status} | " +
            s"Cause: $causeText | " +
            s"Valid for analysis: ${record.isValidForAnalysis}"
        )
      }
    }
  }

  def printDeviceStatusRecords(records: List[DeviceStatusRecord]): Unit = {
    if (records.isEmpty) {
      println("No device status records found.")
    } else {
      records.foreach { record =>
        val noteText = record.note.getOrElse("-")
        println(
          s"Type: ${record.energyType} | " +
            s"Device status: ${record.deviceStatus} | " +
            s"Detected date: ${record.detectedDate} | " +
            s"Detected time: ${record.detectedTime} | " +
            s"Note: $noteText"
        )
      }
    }
  }

  def printAnalysis(result: Option[AnalysisResult]): Unit = {
    result match {
      case Some(analysis) =>
        println(s"Mean: ${analysis.mean}")
        println(s"Median: ${analysis.median}")
        println(s"Mode: ${analysis.mode}")
        println(s"Range: ${analysis.range}")
        println(s"Midrange: ${analysis.midrange}")
        println(s"Total valid records: ${analysis.totalRecords}")
      case None =>
        println("No valid data available for analysis.")
    }
  }

  def printAlerts(alerts: List[Alert]): Unit = {
    if (alerts.isEmpty) {
      println("No alerts generated.")
    } else {
      alerts.foreach { alert =>
        val causeText = alert.possibleCause.getOrElse("-")
        println(
          s"[${alert.severity}] " +
            s"Date: ${alert.date} | " +
            s"Time: ${alert.time} | " +
            s"Target: ${alert.target} | " +
            s"Message: ${alert.message} | " +
            s"Cause: $causeText"
        )
      }
    }
  }

  def printRecommendations(recommendations: List[Recommendation]): Unit = {
    if (recommendations.isEmpty) {
      println("No recommendations generated.")
    } else {
      recommendations.foreach { recommendation =>
        println(
          s"Date: ${recommendation.date} | " +
            s"Time: ${recommendation.time} | " +
            s"Target: ${recommendation.target} | " +
            s"Action: ${recommendation.action} | " +
            s"Reason: ${recommendation.reason}"
        )
      }
    }
  }

  def printErrors(errors: List[String]): Unit = {
    if (errors.isEmpty) {
      println("No parsing errors found.")
    } else {
      errors.foreach(println)
    }
  }
}