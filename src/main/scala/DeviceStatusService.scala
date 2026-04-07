package com.reps

object DeviceStatusService {

  def latestStatusForEnergyType(
                                 records: List[DeviceStatusRecord],
                                 energyType: EnergyType
                               ): Option[DeviceStatusRecord] = {
    val filtered = records.filter(_.energyType == energyType)

    if (filtered.isEmpty) None
    else {
      Some(
        filtered.maxBy(r => (toSortableDate(r.detectedDate), r.detectedTime))
      )
    }
  }

  def markImportedRecordsAsInvalidIfNeeded(
                                            imported: List[EnergyRecord],
                                            statusHistory: List[DeviceStatusRecord]
                                          ): List[EnergyRecord] = {
    imported.map { record =>
      latestStatusForEnergyType(statusHistory, record.energyType) match {
        case Some(deviceRecord) if deviceRecord.deviceStatus != Operational =>
          record.copy(isValidForAnalysis = false)

        case _ =>
          record
      }
    }
  }

  private def toSortableDate(date: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)}"
  }
}