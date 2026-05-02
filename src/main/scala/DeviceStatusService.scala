// Shuo Zhao 002513249
// Hongyao Liu 002513919
// Hongrui Zhang 002520436

package com.reps

object DeviceStatusService {

  // Find the latest device status record for one energy type
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

  // If the latest device status is not operational, mark imported records as invalid
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

  // Convert dd/MM/yyyy into yyyy-MM-dd for sorting
  private def toSortableDate(date: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)}"
  }
}