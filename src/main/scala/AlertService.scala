package com.reps

object AlertService {

  def detectLowOutput(records: List[EnergyRecord]): List[Alert] = {
    records
      .filter(_.isValidForAnalysis)
      .filter(_.status == LowOutput)
      .map { record =>
        val basis = record.energyType match {
          case Solar =>
            val hour = record.time.split(":")(0).toInt
            if ((hour >= 6 && hour <= 8) || (hour >= 17 && hour <= 20)) {
              s"Solar generation is below 20 during daylight edge hours (${record.time})."
            } else {
              s"Solar generation is below 40 during core daylight hours (${record.time})."
            }

          case Wind =>
            s"Wind generation is below 300 at ${record.time}."

          case Hydro =>
            s"Hydro generation is below 700 at ${record.time}."
        }

        val cause = record.energyType match {
          case Solar => Some("Low sunlight or cloud cover")
          case Wind  => Some("Low wind speed")
          case Hydro => Some("Reduced water flow")
        }

        Alert(
          date = record.date,
          time = record.time,
          target = record.energyType.toString,
          severity = Warning,
          message = s"Low output detected. Basis: $basis",
          possibleCause = cause
        )
      }
  }

  def detectMaintenanceNeeded(records: List[EnergyRecord]): List[Alert] = {
    records
      .filter(_.isValidForAnalysis)
      .filter(_.status == MaintenanceNeeded)
      .map { record =>
        Alert(
          date = record.date,
          time = record.time,
          target = record.energyType.toString,
          severity = Warning,
          message = "Maintenance may be needed. Basis: the record status is explicitly marked as MaintenanceNeeded.",
          possibleCause = Some("Performance degradation")
        )
      }
  }

  def detectMalfunction(records: List[EnergyRecord]): List[Alert] = {
    records
      .filter(_.isValidForAnalysis)
      .filter(_.status == Malfunction)
      .map { record =>
        Alert(
          date = record.date,
          time = record.time,
          target = record.energyType.toString,
          severity = Critical,
          message = "Possible malfunction detected. Basis: the record status is explicitly marked as Malfunction.",
          possibleCause = Some("Equipment fault suspected")
        )
      }
  }

  def generateAllAlerts(records: List[EnergyRecord]): List[Alert] = {
    val alerts =
      detectMalfunction(records) :::
        detectLowOutput(records) :::
        detectMaintenanceNeeded(records)

    alerts.sortBy(alert => severityRank(alert.severity))
  }

  def countBySeverity(alerts: List[Alert]): Map[SeverityLevel, Int] = {
    alerts.groupBy(_.severity).view.mapValues(_.size).toMap
  }

  private def severityRank(severity: SeverityLevel): Int = severity match {
    case Critical => 1
    case Warning  => 2
    case Info     => 3
  }
}