package com.reps

import scala.io.Source
import java.io.File
import java.io.FileWriter

object FileIO {

  def loadRecords(filePath: String): List[Either[String, EnergyRecord]] = {
    val source = Source.fromFile(filePath)
    try {
      source.getLines().drop(1).toList.map(Parser.parseLine)
    } finally {
      source.close()
    }
  }

  def loadValidRecords(filePath: String): List[EnergyRecord] = {
    loadRecords(filePath).collect { case Right(record) => record }
  }

  def loadErrors(filePath: String): List[String] = {
    loadRecords(filePath).collect { case Left(error) => error }
  }

  def recordToCsv(record: EnergyRecord): String = {
    val energyType = record.energyType match {
      case Solar => "Solar"
      case Wind  => "Wind"
      case Hydro => "Hydro"
    }

    val status = record.status match {
      case Normal            => "Normal"
      case LowOutput         => "LowOutput"
      case Malfunction       => "Malfunction"
      case MaintenanceNeeded => "MaintenanceNeeded"
    }

    s"${record.date},${record.time},$energyType,${record.generation},${record.storage},${record.equipmentHealth},$status"
  }

  def appendRecord(filePath: String, record: EnergyRecord): Unit = {
    val file = new File(filePath)
    val needsLeadingNewline = file.exists() && file.length() > 0

    val writer = new FileWriter(filePath, true)
    try {
      if (needsLeadingNewline) {
        writer.write("\n")
      }
      writer.write(recordToCsv(record))
    } finally {
      writer.close()
    }
  }
}