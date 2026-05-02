// Shuo Zhao 002513249
// Hongyao Liu 002513919
// Hongrui Zhang 002520436

package com.reps

// External references used in this file:
// Scala Documentation. (n.d.). Case classes | Tour of Scala. https://docs.scala-lang.org/tour/case-classes.html
// Case classes are used to model the main immutable data structures in this system.

// Energy source types used in the system
sealed trait EnergyType
case object Solar extends EnergyType
case object Wind extends EnergyType
case object Hydro extends EnergyType

// Device operating states
sealed trait DeviceStatus
case object Operational extends DeviceStatus
case object UnderMaintenance extends DeviceStatus
case object Damaged extends DeviceStatus

// Plant output or condition states
sealed trait PlantStatus
case object Normal extends PlantStatus
case object LowOutput extends PlantStatus
case object MaintenanceNeeded extends PlantStatus
case object Malfunction extends PlantStatus

// Alert severity levels
sealed trait SeverityLevel
case object Info extends SeverityLevel
case object Warning extends SeverityLevel
case object Critical extends SeverityLevel

// Actions suggested by the control logic
sealed trait ControlAction
case object AdjustSolarPanel extends ControlAction
case object AdjustWindTurbine extends ControlAction
case object InspectHydroFlow extends ControlAction
case object InspectStorageSystem extends ControlAction
case object ScheduleMaintenance extends ControlAction
case object NoActionNeeded extends ControlAction

// One generation record for solar, wind, or hydro
case class EnergyRecord(
                         date: String,
                         time: String,
                         energyType: EnergyType,
                         actualGeneration: Double,
                         status: PlantStatus,
                         possibleCause: Option[String],
                         isValidForAnalysis: Boolean
                       )

// One device status record for a specific energy type
case class DeviceStatusRecord(
                               energyType: EnergyType,
                               deviceStatus: DeviceStatus,
                               detectedDate: String,
                               detectedTime: String,
                               note: Option[String]
                             )

// One storage data record
case class StorageRecord(
                          date: String,
                          time: String,
                          chargingPower: Double,
                          dischargingPower: Double,
                          installedCapacity: Double
                        )

// Alert generated from abnormal conditions
case class Alert(
                  date: String,
                  time: String,
                  target: String,
                  severity: SeverityLevel,
                  message: String,
                  possibleCause: Option[String]
                )

// Recommendation generated from an alert
case class Recommendation(
                           date: String,
                           time: String,
                           target: String,
                           action: ControlAction,
                           reason: String
                         )

// Result of generation data analysis
case class AnalysisResult(
                           mean: Double,
                           median: Double,
                           mode: Double,
                           range: Double,
                           midrange: Double,
                           totalRecords: Int
                         )