package com.reps

object DeviceStatusService {

  // Find the latest device status record for one energy type
  // that happened at or before the given record time
  def latestStatusBeforeOrAt(
                              records: List[DeviceStatusRecord],
                              energyType: EnergyType,
                              date: String,
                              time: String
                            ): Option[DeviceStatusRecord] = {
    val targetDateTime = toSortableDateTime(date, time)

    val filtered = records.filter { r =>
      r.energyType == energyType &&
        toSortableDateTime(r.detectedDate, r.detectedTime) <= targetDateTime
    }

    if (filtered.isEmpty) None
    else {
      Some(
        filtered.maxBy(r => toSortableDateTime(r.detectedDate, r.detectedTime))
      )
    }
  }

  // Apply device status to imported records
  // Device status can override plant status
  def applyDeviceStatusToImportedRecords(
                                          imported: List[EnergyRecord],
                                          statusHistory: List[DeviceStatusRecord]
                                        ): List[EnergyRecord] = {
    imported.map { record =>
      latestStatusBeforeOrAt(
        statusHistory,
        record.energyType,
        record.date,
        record.time
      ) match {
        case Some(deviceRecord) =>
          deviceRecord.deviceStatus match {
            case Damaged =>
              record.copy(
                status = Malfunction,
                isValidForAnalysis = false,
                possibleCause = Some("Device status indicates damage")
              )

            case UnderMaintenance =>
              record.copy(
                status = MaintenanceNeeded,
                isValidForAnalysis = false,
                possibleCause = Some("Device is under maintenance")
              )

            case Operational =>
              record
          }

        case None =>
          record
      }
    }
  }

  // Convert dd/MM/yyyy and HH:MM into yyyy-MM-dd HH:MM for comparison
  private def toSortableDateTime(date: String, time: String): String = {
    val parts = date.split("/")
    s"${parts(2)}-${parts(1)}-${parts(0)} $time"
  }
}