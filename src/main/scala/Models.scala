package com.reps

sealed trait EnergyType
case object Solar extends EnergyType
case object Wind extends EnergyType
case object Hydro extends EnergyType

sealed trait PlantStatus
case object Normal extends PlantStatus
case object LowOutput extends PlantStatus
case object Malfunction extends PlantStatus
case object MaintenanceNeeded extends PlantStatus

sealed trait SeverityLevel
case object Info extends SeverityLevel
case object Warning extends SeverityLevel
case object Critical extends SeverityLevel

sealed trait ControlAction
case object AdjustSolarPanel extends ControlAction
case object AdjustWindTurbine extends ControlAction
case object InspectHydroFlow extends ControlAction
case object ScheduleMaintenance extends ControlAction
case object NoActionNeeded extends ControlAction

case class EnergyRecord(
                         date: String,
                         time: String,
                         energyType: EnergyType,
                         generation: Double,
                         storage: Double,
                         equipmentHealth: Double,
                         status: PlantStatus
                       )

case class Alert(
                  record: EnergyRecord,
                  severity: SeverityLevel,
                  message: String,
                  recommendedAction: ControlAction
                )

case class AnalysisResult(
                           mean: Double,
                           median: Double,
                           mode: Double,
                           range: Double,
                           midrange: Double,
                           totalRecords: Int
                         )