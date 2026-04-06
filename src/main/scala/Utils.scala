package com.reps

object Utils {

  def printRecords(records: List[EnergyRecord]): Unit = {
    if (records.isEmpty) {
      println("No records found.")
    } else {
      records.foreach(println)
    }
  }

  def printAlerts(alerts: List[Alert]): Unit = {
    if (alerts.isEmpty) {
      println("No alerts generated.")
    } else {
      alerts.foreach(println)
    }
  }

  def printAnalysis(result: Option[AnalysisResult]): Unit = result match {
    case Some(r) =>
      println(s"Mean: ${r.mean}")
      println(s"Median: ${r.median}")
      println(s"Mode: ${r.mode}")
      println(s"Range: ${r.range}")
      println(s"Midrange: ${r.midrange}")
      println(s"Total records: ${r.totalRecords}")
    case None =>
      println("No data available for analysis.")
  }

  def printRecommendations(recommendations: List[(Alert, ControlAction)]): Unit = {
    if (recommendations.isEmpty) {
      println("No recommendations available.")
    } else {
      recommendations.foreach {
        case (alert, action) =>
          println(s"Alert: ${alert.message} | Energy Type: ${alert.record.energyType} | Recommended Action: $action")
      }
    }
  }
}