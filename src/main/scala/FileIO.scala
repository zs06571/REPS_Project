// Shuo Zhao 002513249
// Hongyao Liu 002513919
// Hongrui Zhang 002520436

package com.reps

import scala.io.Source
import java.io.File
import java.io.FileWriter

object FileIO {

  // Header for energy data CSV files
  private val energyHeader =
    "date,time,energyType,actualGeneration,status,possibleCause,isValidForAnalysis"

  // Header for device status CSV files
  private val deviceHeader =
    "energyType,deviceStatus,detectedDate,detectedTime,note"

  // Load all energy records from
  // Each line is parsed into Either[String, EnergyRecord]
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

  // Keep only successfully parsed energy records
  def loadValidRecords(filePath: String): List[EnergyRecord] = {
    loadEnergyRecords(filePath).collect { case Right(record) => record }
  }

  // Return parsing errors with line numbers
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

  // Convert one EnergyRecord into CSV format
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
    }

    val cause = record.possibleCause.getOrElse("")

    s"${record.date},${record.time},$energyType,${record.actualGeneration},$plantStatus,$cause,${record.isValidForAnalysis}"
  }

  // Append one energy record to the CSV file
  // Write the header first if the file is empty
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

  // Append many energy records
  def appendRecords(filePath: String, records: List[EnergyRecord]): Unit = {
    records.foreach(record => appendRecord(filePath, record))
  }

  // Load all device status records from file
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

  // Keep only successfully parsed device status records
  def loadValidDeviceStatusRecords(filePath: String): List[DeviceStatusRecord] = {
    loadDeviceStatusRecords(filePath).collect { case Right(record) => record }
  }

  // Convert one DeviceStatusRecord into CSV format
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

  // Append one device status record to the CSV file
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

  // Convert one StorageRecord into CSV format
  def storageRecordToCsv(record: StorageRecord): String = {
    s"${record.date},${record.time},${record.chargingPower},${record.dischargingPower},${record.installedCapacity}"
  }

  // Append one storage record to the CSV file
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

  // Load all storage records from file
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

  // Return all parsed storage records
  def loadValidStorageRecords(filePath: String): List[StorageRecord] = {
    loadStorageRecords(filePath)
  }
}