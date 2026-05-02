// Shuo Zhao 002513249
// Hongyao Liu 002513919 
// Hongrui Zhang 002520436

package com.reps

object Parser {

  // Convert a string into an EnergyType
  def parseEnergyType(value: String): Either[String, EnergyType] = value.trim match {
    case "Solar" => Right(Solar)
    case "Wind" => Right(Wind)
    case "Hydro" => Right(Hydro)
    case _ => Left("Invalid energy type.")
  }

  // Convert a string into a DeviceStatus
  def parseDeviceStatus(value: String): Either[String, DeviceStatus] = value.trim match {
    case "Operational" => Right(Operational)
    case "UnderMaintenance" => Right(UnderMaintenance)
    case "Damaged" => Right(Damaged)
    case _ => Left("Invalid device status.")
  }

  // Convert a string into a PlantStatus
  def parsePlantStatus(value: String): Either[String, PlantStatus] = value.trim match {
    case "Normal" => Right(Normal)
    case "LowOutput" => Right(LowOutput)
    case "MaintenanceNeeded" => Right(MaintenanceNeeded)
    case "Malfunction" => Right(Malfunction)
    case _ => Left("Invalid plant status.")
  }

  // Convert a string into a Boolean flag
  def parseIsValidForAnalysis(value: String): Either[String, Boolean] = value.trim match {
    case "true" => Right(true)
    case "false" => Right(false)
    case _ => Left("Invalid isValidForAnalysis value. Use true or false.")
  }

  // Convert an optional text field
  def parseOptionalString(value: String): Option[String] = {
    val trimmed = value.trim
    if (trimmed.isEmpty) None else Some(trimmed)
  }

  // Parse one CSV line into an EnergyRecord
  def parseEnergyLine(line: String): Either[String, EnergyRecord] = {
    val parts = line.split(",", -1).map(_.trim).toList

    parts match {
      case date :: time :: energy :: actual :: status :: possibleCause :: isValidForAnalysis :: Nil =>
        if (!Validation.isValidDate(date)) {
          Left(s"Invalid date format: $date")
        } else if (!Validation.isValidTime(time)) {
          Left(s"Invalid time format: $time")
        } else {
          for {
            energyType <- parseEnergyType(energy)
            actualGeneration <- Validation.validateGeneration(actual)
            plantStatus <- parsePlantStatus(status)
            validFlag <- parseIsValidForAnalysis(isValidForAnalysis)
          } yield EnergyRecord(
            date = date,
            time = time,
            energyType = energyType,
            actualGeneration = actualGeneration,
            status = plantStatus,
            possibleCause = parseOptionalString(possibleCause),
            isValidForAnalysis = validFlag
          )
        }

      case _ =>
        Left("Invalid energy CSV line format.")
    }
  }

  // Parse one CSV line into a DeviceStatusRecord
  def parseDeviceStatusLine(line: String): Either[String, DeviceStatusRecord] = {
    val parts = line.split(",", -1).map(_.trim).toList

    parts match {
      case energy :: deviceStatus :: detectedDate :: detectedTime :: note :: Nil =>
        if (!Validation.isValidDate(detectedDate)) {
          Left(s"Invalid detected date format: $detectedDate")
        } else if (!Validation.isValidTime(detectedTime)) {
          Left(s"Invalid detected time format: $detectedTime")
        } else {
          for {
            energyType <- parseEnergyType(energy)
            status <- parseDeviceStatus(deviceStatus)
          } yield DeviceStatusRecord(
            energyType = energyType,
            deviceStatus = status,
            detectedDate = detectedDate,
            detectedTime = detectedTime,
            note = parseOptionalString(note)
          )
        }

      case _ =>
        Left("Invalid device status CSV line format.")
    }
  }
}
