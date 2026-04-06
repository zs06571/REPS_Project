package com.reps

object Parser {

  def parseEnergyType(value: String): Either[String, EnergyType] = value.trim match {
    case "Solar" => Right(Solar)
    case "Wind"  => Right(Wind)
    case "Hydro" => Right(Hydro)
    case _       => Left("Invalid energy type.")
  }

  def parsePlantStatus(value: String): Either[String, PlantStatus] = value.trim match {
    case "Normal"            => Right(Normal)
    case "LowOutput"         => Right(LowOutput)
    case "Malfunction"       => Right(Malfunction)
    case "MaintenanceNeeded" => Right(MaintenanceNeeded)
    case _                   => Left("Invalid plant status.")
  }

  def parseLine(line: String): Either[String, EnergyRecord] = {
    val parts = line.split(",").map(_.trim).toList

    parts match {
      case date :: time :: energy :: generation :: storage :: health :: status :: Nil =>
        if (!Validation.isValidDate(date)) {
          Left(s"Invalid date format: $date")
        } else if (!Validation.isValidTime(time)) {
          Left(s"Invalid time format: $time")
        } else {
          parseEnergyType(energy) match {
            case Left(err) => Left(err)
            case Right(energyType) =>
              Validation.validateGeneration(generation) match {
                case Left(err) => Left(err)
                case Right(gen) =>
                  Validation.validateStorage(storage) match {
                    case Left(err) => Left(err)
                    case Right(sto) =>
                      Validation.validateHealth(health) match {
                        case Left(err) => Left(err)
                        case Right(eqHealth) =>
                          parsePlantStatus(status) match {
                            case Left(err) => Left(err)
                            case Right(plantStat) =>
                              Right(EnergyRecord(date, time, energyType, gen, sto, eqHealth, plantStat))
                          }
                      }
                  }
              }
          }
        }

      case _ =>
        Left("Invalid CSV line format.")
    }
  }
}