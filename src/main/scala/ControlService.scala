package com.reps

object ControlService {

  def recommendAction(alert: Alert): ControlAction = {
    alert.record.energyType match {
      case Solar =>
        if (alert.record.status == LowOutput) AdjustSolarPanel
        else alert.recommendedAction

      case Wind =>
        if (alert.record.status == LowOutput) AdjustWindTurbine
        else alert.recommendedAction

      case Hydro =>
        if (alert.record.status == LowOutput) InspectHydroFlow
        else alert.recommendedAction
    }
  }

  def generateRecommendations(alerts: List[Alert]): List[(Alert, ControlAction)] = {
    alerts.map(alert => (alert, recommendAction(alert)))
  }
}