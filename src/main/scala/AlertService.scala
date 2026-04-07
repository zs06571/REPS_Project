package com.reps

object AlertService {

  def detectLowOutput(records: List[EnergyRecord]): List[Alert] = {
    records
      .filter(_.isValidForAnalysis)
      .filter(_.status == LowOutput)
      .map { record =>
        Alert(
          date = record.date,
          time = record.time,
          target = record.energyType.toString,
          severity = Warning,
          message = "Energy output is low.",
          possibleCause = record.possibleCause
        )
      }
  }

  def detectForecastUnavailable(records: List[EnergyRecord]): List[Alert] = {
    records
      .filter(_.isValidForAnalysis)
      .filter(_.status == ForecastUnavailable)
      .map { record =>
        Alert(
          date = record.date,
          time = record.time,
          target = record.energyType.toString,
          severity = Info,
          message = "Forecast functionality is unavailable for this energy source.",
          possibleCause = record.possibleCause
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
          message = "Maintenance may be needed.",
          possibleCause = record.possibleCause
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
          message = "Possible malfunction detected.",
          possibleCause = record.possibleCause
        )
      }
  }

  def generateAllAlerts(records: List[EnergyRecord]): List[Alert] = {
    detectLowOutput(records) :::
      detectForecastUnavailable(records) :::
      detectMaintenanceNeeded(records) :::
      detectMalfunction(records)
  }
}