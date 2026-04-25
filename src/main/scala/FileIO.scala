package com.reps

import scala.io.Source
import java.io.File
import java.io.FileWriter

object FileIO {

  private val energyHeader =
    "date,time,energyType,actualGeneration,status,possibleCause,isValidForAnalysis"

  private val deviceHeader =
    "energyType,deviceStatus,detectedDate,detectedTime,note"

  def loadEnergyRecords(filePath: String): List[Either[String, EnergyRecord]] = {
    val file = new File(filePath)
    if (!file.exists()) return Nil

    val source = Source.fromFile(filePath)
    try {
      source.getLines().drop(1).toList.map(Parser.parseEnergyLine)
    } finally {
      source.close()
    }
  }

  def loadValidRecords(filePath: String): List[EnergyRecord] = {
    loadEnergyRecords(filePath).collect { case Right(record) => record }
  }

  def loadErrors(filePath: String): List[String] = {
    val file = new File(filePath)
    if (!file.exists()) return Nil

    val source = Source.fromFile(filePath)
    try {
      source.getLines().drop(1).toList.zipWithIndex.flatMap {
        case (line, index) =>
          Parser.parseEnergyLine(line) match {
            case Left(error) => Some(s"Line ${index + 2}: $error | Raw data: $line")
            case Right(_)    => None
          }
      }
    } finally {
      source.close()
    }
  }

  def energyRecordToCsv(record: EnergyRecord): String = {
    val energyType = record.energyType match {
      case Solar => "Solar"
      case Wind  => "Wind"
      case Hydro => "Hydro"
    }

    val plantStatus = record.status match {
      case Normal              => "Normal"
      case LowOutput           => "LowOutput"
      case MaintenanceNeeded   => "MaintenanceNeeded"
      case Malfunction         => "Malfunction"
      case ForecastUnavailable => "ForecastUnavailable"
    }

    val cause = record.possibleCause.getOrElse("")

    s"${record.date},${record.time},$energyType,${record.actualGeneration},$plantStatus,$cause,${record.isValidForAnalysis}"
  }

  def appendRecord(filePath: String, record: EnergyRecord): Unit = {
    val file = new File(filePath)
    val fileExistsAndHasContent = file.exists() && file.length() > 0

    val writer = new FileWriter(filePath, true)
    try {
      if (!fileExistsAndHasContent) {
        writer.write(energyHeader + "\n")
      } else {
        writer.write("\n")
      }
      writer.write(energyRecordToCsv(record))
    } finally {
      writer.close()
    }
  }

  def appendRecords(filePath: String, records: List[EnergyRecord]): Unit = {
    records.foreach(record => appendRecord(filePath, record))
  }

  def overwriteEnergyRecords(filePath: String, records: List[EnergyRecord]): Unit = {
    val writer = new FileWriter(filePath, false)
    try {
      writer.write(energyHeader + "\n")
      writer.write(records.map(energyRecordToCsv).mkString("\n"))
    } finally {
      writer.close()
    }
  }

  def loadDeviceStatusRecords(filePath: String): List[Either[String, DeviceStatusRecord]] = {
    val file = new File(filePath)
    if (!file.exists()) return Nil

    val source = Source.fromFile(filePath)
    try {
      source.getLines().drop(1).toList.map(Parser.parseDeviceStatusLine)
    } finally {
      source.close()
    }
  }

  def loadValidDeviceStatusRecords(filePath: String): List[DeviceStatusRecord] = {
    loadDeviceStatusRecords(filePath).collect { case Right(record) => record }
  }

  def deviceStatusRecordToCsv(record: DeviceStatusRecord): String = {
    val energyType = record.energyType match {
      case Solar => "Solar"
      case Wind  => "Wind"
      case Hydro => "Hydro"
    }

    val deviceStatus = record.deviceStatus match {
      case Operational      => "Operational"
      case UnderMaintenance => "UnderMaintenance"
      case Damaged          => "Damaged"
    }

    val note = record.note.getOrElse("")

    s"$energyType,$deviceStatus,${record.detectedDate},${record.detectedTime},$note"
  }

  def appendDeviceStatusRecord(filePath: String, record: DeviceStatusRecord): Unit = {
    val file = new File(filePath)
    val fileExistsAndHasContent = file.exists() && file.length() > 0

    val writer = new FileWriter(filePath, true)
    try {
      if (!fileExistsAndHasContent) {
        writer.write(deviceHeader + "\n")
      } else {
        writer.write("\n")
      }
      writer.write(deviceStatusRecordToCsv(record))
    } finally {
      writer.close()
    }
  }

  def storageRecordToCsv(record: StorageRecord): String = {
    s"${record.date},${record.time},${record.chargingPower},${record.dischargingPower},${record.installedCapacity}"
  }

  def appendStorageRecord(filePath: String, record: StorageRecord): Unit = {
    val header = "date,time,chargingPower,dischargingPower,installedCapacity"
    val file = new java.io.File(filePath)
    val fileExistsAndHasContent = file.exists() && file.length() > 0

    val writer = new java.io.FileWriter(filePath, true)
    try {
      if (!fileExistsAndHasContent) {
        writer.write(header + "\n")
      } else {
        writer.write("\n")
      }
      writer.write(storageRecordToCsv(record))
    } finally {
      writer.close()
    }
  }

  def loadStorageRecords(filePath: String): List[StorageRecord] = {
    val file = new java.io.File(filePath)
    if (!file.exists()) return Nil

    val source = scala.io.Source.fromFile(filePath)
    try {
      source.getLines().drop(1).toList.flatMap { line =>
        val parts = line.split(",", -1).map(_.trim).toList
        parts match {
          case date :: time :: charging :: discharging :: capacity :: Nil =>
            try {
              Some(
                StorageRecord(
                  date = date,
                  time = time,
                  chargingPower = charging.toDouble,
                  dischargingPower = discharging.toDouble,
                  installedCapacity = capacity.toDouble
                )
              )
            } catch {
              case _: NumberFormatException => None
            }
          case _ => None
        }
      }
    } finally {
      source.close()
    }
  }

  def loadValidStorageRecords(filePath: String): List[StorageRecord] = {
    loadStorageRecords(filePath)
  }
}