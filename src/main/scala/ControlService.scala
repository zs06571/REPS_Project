//Shuo Zhao 002513249
//Hongyao Liu 002513919 
//Hongrui Zhang 002520436

package com.reps

object ControlService {

  // Generate one recommendation for each alert
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

  // Choose the control action based on alert severity, target, and message
  private def chooseAction(alert: Alert): ControlAction = {
    val target = alert.target.toLowerCase
    val message = alert.message.toLowerCase

    (alert.severity, target, message) match {
      case (Critical, _, m) if m.contains("malfunction") =>
        ScheduleMaintenance

      case (_, _, m) if m.contains("maintenance") =>
        ScheduleMaintenance

      case (_, "solar", m) if m.contains("low output") =>
        AdjustSolarPanel

      case (_, "wind", m) if m.contains("low output") =>
        AdjustWindTurbine

      case (_, "hydro", m) if m.contains("low output") =>
        InspectHydroFlow

      case _ =>
        NoActionNeeded
    }
  }

  // Build a readable explanation for the recommendation
  private def buildReason(alert: Alert, action: ControlAction): String = {
    val causeText = alert.possibleCause match {
      case Some(text) => s" Possible cause: $text."
      case None       => ""
    }

    val actionText = action match {
      case AdjustSolarPanel =>
        " Suggested action: inspect panel orientation, sunlight exposure, and solar operating conditions."
      case AdjustWindTurbine =>
        " Suggested action: inspect turbine operating conditions and wind-related performance."
      case InspectHydroFlow =>
        " Suggested action: inspect hydro flow conditions and turbine performance."
      case ScheduleMaintenance =>
        " Suggested action: schedule maintenance and technical inspection."
      case InspectStorageSystem =>
        " Suggested action: inspect the storage subsystem."
      case NoActionNeeded =>
        " No immediate action is required."
    }

    s"${alert.message}$causeText$actionText"
  }
}
