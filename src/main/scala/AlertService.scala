package com.reps

object AlertService {

  def lowOutputRule(threshold: Double)(record: EnergyRecord): Boolean = {
    record.generation < threshold
  }

  def detectLowOutput(records: List[EnergyRecord], threshold: Double): List[Alert] = {
    records
      .filter(lowOutputRule(threshold))
      .map(record => Alert(record, Warning, "Low energy output detected.", NoActionNeeded))
  }

  def detectMalfunction(records: List[EnergyRecord]): List[Alert] = {
    records
      .filter(_.status == Malfunction)
      .map(record => Alert(record, Critical, "Equipment malfunction detected.", ScheduleMaintenance))
  }

  def detectLowHealth(records: List[EnergyRecord], threshold: Double): List[Alert] = {
    records
      .filter(_.equipmentHealth < threshold)
      .map(record => Alert(record, Warning, "Equipment health is low.", ScheduleMaintenance))
  }

  def generateAllAlerts(records: List[EnergyRecord]): List[Alert] = {
    detectLowOutput(records, 50.0) ::: detectMalfunction(records) ::: detectLowHealth(records, 40.0)
  }
}