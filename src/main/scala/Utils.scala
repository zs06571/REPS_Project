package com.reps

object Utils {

  def printRecords(records: List[EnergyRecord]): Unit = {
    if (records.isEmpty) {
      println("No records found.")
    } else {
      records.foreach { record =>
        println(
          s"Date: ${record.date} | Time: ${record.time} | Type: ${record.energyType} | " +
            s"Generation: ${record.generation} | Storage: ${record.storage} | " +
            s"Health: ${record.equipmentHealth} | Status: ${record.status}"
        )
      }
    }
  }

  def printAlerts(alerts: List[Alert]): Unit = {
    if (alerts.isEmpty) {
      println("No alerts generated.")
    } else {
      alerts.foreach { alert =>
        println(
          s"[${alert.severity}] ${alert.message} | Date: ${alert.record.date} | " +
            s"Time: ${alert.record.time} | Type: ${alert.record.energyType} | " +
            s"Generation: ${alert.record.generation} | Status: ${alert.record.status}"
        )
      }
    }
  }

  def printAnalysis(result: Option[AnalysisResult]): Unit = result match {
    case Some(r) =>
      println("----- Analysis Summary -----")
      println(s"Total Records : ${r.totalRecords}")
      println(s"Mean          : ${r.mean}")
      println(s"Median        : ${r.median}")
      println(s"Mode          : ${r.mode}")
      println(s"Range         : ${r.range}")
      println(s"Midrange      : ${r.midrange}")
      println("----------------------------")
    case None =>
      println("No data available for analysis.")
  }

  def printRecommendations(recommendations: List[(Alert, ControlAction)]): Unit = {
    if (recommendations.isEmpty) {
      println("No recommendations available.")
    } else {
      recommendations.foreach {
        case (alert, action) =>
          println(
            s"Recommendation: $action | Based on: ${alert.message} | " +
              s"Date: ${alert.record.date} | Time: ${alert.record.time} | " +
              s"Type: ${alert.record.energyType} | Status: ${alert.record.status}"
          )
      }
    }
  }

  def printErrors(errors: List[String]): Unit = {
    if (errors.isEmpty) {
      println("No file parsing errors found.")
    } else {
      println("File parsing errors:")
      errors.foreach(println)
    }
  }
}