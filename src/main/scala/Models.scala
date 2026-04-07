package com.reps

sealed trait EnergyType
case object Solar extends EnergyType
case object Wind extends EnergyType
case object Hydro extends EnergyType

sealed trait DeviceStatus
case object Operational extends DeviceStatus
case object UnderMaintenance extends DeviceStatus
case object Damaged extends DeviceStatus

sealed trait PlantStatus
case object Normal extends PlantStatus
case object LowOutput extends PlantStatus
case object MaintenanceNeeded extends PlantStatus
case object Malfunction extends PlantStatus
case object ForecastUnavailable extends PlantStatus

sealed trait SeverityLevel
case object Info extends SeverityLevel
case object Warning extends SeverityLevel
case object Critical extends SeverityLevel

sealed trait ControlAction
case object AdjustSolarPanel extends ControlAction
case object AdjustWindTurbine extends ControlAction
case object InspectHydroFlow extends ControlAction
case object InspectStorageSystem extends ControlAction
case object ScheduleMaintenance extends ControlAction
case object NoActionNeeded extends ControlAction

case class EnergyRecord(
                         date: String,
                         time: String,
                         energyType: EnergyType,
                         actualGeneration: Double,
                         forecastGeneration: Option[Double],
                         forecastAvailable: Boolean,
                         status: PlantStatus,
                         possibleCause: Option[String],
                         isValidForAnalysis: Boolean
                       )

case class DeviceStatusRecord(
                               energyType: EnergyType,
                               deviceStatus: DeviceStatus,
                               detectedDate: String,
                               detectedTime: String,
                               note: Option[String]
                             )

case class StorageRecord(
                          date: String,
                          time: String,
                          chargingPower: Double,
                          dischargingPower: Double,
                          installedCapacity: Double
                        )

case class PlantSnapshot(
                          date: String,
                          time: String,
                          solarGeneration: Double,
                          windGeneration: Double,
                          hydroGeneration: Double,
                          totalGeneration: Double,
                          chargingPower: Double,
                          dischargingPower: Double,
                          installedStorageCapacity: Double
                        )

case class Alert(
                  date: String,
                  time: String,
                  target: String,
                  severity: SeverityLevel,
                  message: String,
                  possibleCause: Option[String]
                )

case class Recommendation(
                           date: String,
                           time: String,
                           target: String,
                           action: ControlAction,
                           reason: String
                         )

case class AnalysisResult(
                           mean: Double,
                           median: Double,
                           mode: Double,
                           range: Double,
                           midrange: Double,
                           totalRecords: Int
                         )