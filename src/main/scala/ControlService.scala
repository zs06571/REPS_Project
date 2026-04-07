package com.reps

object ControlService {

  def generateRecommendations(alerts: List[Alert]): List[Recommendation] = {
    alerts.map { alert =>
      val action = chooseAction(alert)
      val reason = buildReason(alert, action)

      Recommendation(
        date = alert.date,
        time = alert.time,
        target = alert.target,
        action = action,
        reason = reason
      )
    }
  }

  private def chooseAction(alert: Alert): ControlAction = {
    val target = alert.target.toLowerCase
    val message = alert.message.toLowerCase

    if (message.contains("malfunction")) {
      ScheduleMaintenance
    } else if (message.contains("maintenance")) {
      ScheduleMaintenance
    } else if (message.contains("low")) {
      target match {
        case "solar" => AdjustSolarPanel
        case "wind"  => AdjustWindTurbine
        case "hydro" => InspectHydroFlow
        case _       => NoActionNeeded
      }
    } else if (message.contains("forecast functionality is unavailable")) {
      NoActionNeeded
    } else {
      NoActionNeeded
    }
  }

  private def buildReason(alert: Alert, action: ControlAction): String = {
    val base = s"Based on alert: ${alert.message}"
    val cause = alert.possibleCause match {
      case Some(text) => s" Possible cause: $text"
      case None       => ""
    }

    val actionText = action match {
      case AdjustSolarPanel     => " Recommended to inspect solar operating conditions."
      case AdjustWindTurbine    => " Recommended to inspect wind operating conditions."
      case InspectHydroFlow     => " Recommended to inspect hydro flow conditions."
      case InspectStorageSystem => " Recommended to inspect storage subsystem."
      case ScheduleMaintenance  => " Recommended to schedule maintenance."
      case NoActionNeeded       => " No immediate action is needed."
    }

    base + cause + actionText
  }
}