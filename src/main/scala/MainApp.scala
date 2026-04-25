package com.reps

import scala.io.StdIn.readLine
import java.time.LocalDate
import scala.annotation.tailrec

object MainApp {

  val filePath = "data/energy_data.csv"
  val storageFilePath = "data/storage_data.csv"
  val deviceStatusFilePath = "data/device_status.csv"

  def showMenu(): Unit = {
    println("\n--- REPS Menu ---")
    println("1. View all generation records")
    println("2. Filter generation records")
    println("3. Sort generation records")
    println("4. Search generation records")
    println("5. Analyze generation data")
    println("6. Generate alerts")
    println("7. View control recommendations")
    println("8. Add new generation record")
    println("9. Import energy data")
    println("10. Device management")
    println("11. Storage management")
    println("12. Check generation file errors")
    println("13. Exit")
  }

  def filterMenu(records: List[EnergyRecord]): Unit = {
    println("\n--- Filter Menu ---")
    println("1. Filter by date")
    println("2. Filter by energy type")
    println("3. Filter by hour")
    println("4. Filter by month")
    println("5. Filter by date range")
    println("6. Filter by plant status")
    println("7. Back to main menu")

    val choice = readLine("Choose a filter option: ")

    choice match {
      case "1" =>
        val date = readValidDate()
        println(s"\nRecords for $date:")
        Utils.printRecords(QueryService.filterByDate(records, date))
        filterMenu(records)

      case "2" =>
        val energyType = readValidEnergyType()
        println(s"\nFiltered records for $energyType:")
        Utils.printRecords(QueryService.filterByEnergyType(records, energyType))
        filterMenu(records)

      case "3" =>
        val hour = readValidHour()
        println(s"\nRecords for hour $hour:")
        Utils.printRecords(QueryService.filterByHour(records, hour))
        filterMenu(records)

      case "4" =>
        val (month, year) = readValidMonthAndYear()
        println(s"\nRecords for month $month/$year:")
        Utils.printRecords(QueryService.filterByMonth(records, month, year))
        filterMenu(records)

      case "5" =>
        println("Enter start date:")
        val startDate = readValidDate()
        println("Enter end date:")
        val endDate = readValidDate()
        println(s"\nRecords from $startDate to $endDate:")
        Utils.printRecords(QueryService.filterByDateRange(records, startDate, endDate))
        filterMenu(records)

      case "6" =>
        val status = readValidPlantStatus()
        println(s"\nFiltered records for plant status $status:")
        Utils.printRecords(QueryService.filterByStatus(records, status))
        filterMenu(records)

      case "7" =>
        menuLoop()

      case _ =>
        println("Invalid filter option.")
        filterMenu(records)
    }
  }

  def sortMenu(records: List[EnergyRecord]): Unit = {
    println("\n--- Sort Menu ---")
    println("1. Sort by generation ascending")
    println("2. Sort by generation descending")
    println("3. Back to main menu")

    val choice = readLine("Choose a sort option: ")

    choice match {
      case "1" =>
        Utils.printRecords(QueryService.sortByActualGenerationAsc(records))
        sortMenu(records)

      case "2" =>
        Utils.printRecords(QueryService.sortByActualGenerationDesc(records))
        sortMenu(records)

      case "3" =>
        menuLoop()

      case _ =>
        println("Invalid sort option.")
        sortMenu(records)
    }
  }

  def importMenu(): Unit = {
    println("\n--- Import Energy Data ---")
    println("Choose energy sources:")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    println("4. Solar + Wind")
    println("5. Solar + Hydro")
    println("6. Wind + Hydro")
    println("7. Solar + Wind + Hydro")
    println("8. Back")

    val sourceChoice = readLine("Choose source option: ")

    sourceChoice match {
      case "8" => menuLoop()
      case _ =>
        val selectedSources = sourceChoice match {
          case "1" => List(Solar)
          case "2" => List(Wind)
          case "3" => List(Hydro)
          case "4" => List(Solar, Wind)
          case "5" => List(Solar, Hydro)
          case "6" => List(Wind, Hydro)
          case "7" => List(Solar, Wind, Hydro)
          case _   => Nil
        }

        if (selectedSources.isEmpty) {
          println("Invalid source selection.")
          importMenu()
        } else {
          importRangeMenu(selectedSources)
        }
    }
  }

  def importRangeMenu(selectedSources: List[EnergyType]): Unit = {
    println("\n--- Select Import Range ---")
    println("1. Latest record")
    println("2. Last 24 hours")
    println("3. Last 7 days")
    println("4. Last 30 days")
    println("5. Custom date range")
    println("6. Back")

    val rangeChoice = readLine("Choose range option: ")

    rangeChoice match {
      case "1" =>
        importLatestForSources(selectedSources)
        menuLoop()

      case "2" =>
        val end = LocalDate.now()
        val start = end.minusDays(1)
        importRangeForSources(selectedSources, formatDate(start), formatDate(end))
        menuLoop()

      case "3" =>
        val end = LocalDate.now()
        val start = end.minusDays(7)
        importRangeForSources(selectedSources, formatDate(start), formatDate(end))
        menuLoop()

      case "4" =>
        val end = LocalDate.now()
        val start = end.minusDays(30)
        importRangeForSources(selectedSources, formatDate(start), formatDate(end))
        menuLoop()

      case "5" =>
        println("Enter start date:")
        val startDate = readValidDate()
        println("Enter end date:")
        val endDate = readValidDate()
        importRangeForSources(selectedSources, startDate, endDate)
        menuLoop()

      case "6" =>
        importMenu()

      case _ =>
        println("Invalid range selection.")
        importRangeMenu(selectedSources)
    }
  }
  @tailrec
  def deviceMenu(): Unit = {
    println("\n--- Device Management ---")
    println("1. Update device status")
    println("2. View device status records")
    println("3. Back to main menu")

    val choice = readLine("Choose an option: ")

    choice match {
      case "1" =>
        updateDeviceStatusMenu()
        deviceMenu()

      case "2" =>
        println("\nDevice status records:")
        val deviceStatuses = FileIO.loadValidDeviceStatusRecords(deviceStatusFilePath)
        Utils.printDeviceStatusRecords(deviceStatuses)
        deviceMenu()

      case "3" =>
        menuLoop()

      case _ =>
        println("Invalid device menu option.")
        deviceMenu()
    }
  }
  @tailrec
  def storageMenu(): Unit = {
    println("\n--- Storage Management ---")
    println("1. Import latest storage data")
    println("2. View storage records")
    println("3. Back to main menu")

    val choice = readLine("Choose an option: ")

    choice match {
      case "1" =>
        ApiService.importLatestStorageData() match {
          case Right(record) =>
            FileIO.appendStorageRecord(storageFilePath, record)
            println("Latest storage record imported successfully.")
          case Left(error) =>
            println(s"Storage import failed: $error")
        }
        storageMenu()

      case "2" =>
        println("\nStorage records:")
        val storageRecords = FileIO.loadValidStorageRecords(storageFilePath)
        Utils.printStorageRecords(storageRecords)
        storageMenu()

      case "3" =>
        menuLoop()

      case _ =>
        println("Invalid storage menu option.")
        storageMenu()
    }
  }

  def importLatestForSources(selectedSources: List[EnergyType]): Unit = {
    println("\nImporting latest data...")

    val deviceStatuses = FileIO.loadValidDeviceStatusRecords(deviceStatusFilePath)

    selectedSources.foreach {
      case Solar =>
        ApiService.importLatestSolarData() match {
          case Right(record) =>
            val finalRecord = DeviceStatusService.markImportedRecordsAsInvalidIfNeeded(List(record), deviceStatuses).head
            FileIO.appendRecord(filePath, finalRecord)
            println("Solar latest record imported successfully.")
          case Left(error) =>
            println(s"Solar import failed: $error")
        }

      case Wind =>
        ApiService.importLatestWindData() match {
          case Right(record) =>
            val finalRecord = DeviceStatusService.markImportedRecordsAsInvalidIfNeeded(List(record), deviceStatuses).head
            FileIO.appendRecord(filePath, finalRecord)
            println("Wind latest record imported successfully.")
          case Left(error) =>
            println(s"Wind import failed: $error")
        }

      case Hydro =>
        ApiService.importLatestHydroData() match {
          case Right(record) =>
            val finalRecord = DeviceStatusService.markImportedRecordsAsInvalidIfNeeded(List(record), deviceStatuses).head
            FileIO.appendRecord(filePath, finalRecord)
            println("Hydro latest record imported successfully.")
          case Left(error) =>
            println(s"Hydro import failed: $error")
        }
    }
  }

  def importRangeForSources(selectedSources: List[EnergyType], startDate: String, endDate: String): Unit = {
    println(s"\nImporting records from $startDate to $endDate ...")

    val deviceStatuses = FileIO.loadValidDeviceStatusRecords(deviceStatusFilePath)

    selectedSources.foreach {
      case Solar =>
        ApiService.importSolarByRange(startDate, endDate) match {
          case Right(records) =>
            val finalRecords = DeviceStatusService.markImportedRecordsAsInvalidIfNeeded(records, deviceStatuses)
            FileIO.appendRecords(filePath, finalRecords)
            println(s"Solar range import successful. Imported ${finalRecords.length} record(s).")
          case Left(error) =>
            println(s"Solar range import failed: $error")
        }
        Thread.sleep(3000)

      case Wind =>
        ApiService.importWindByRange(startDate, endDate) match {
          case Right(records) =>
            val finalRecords = DeviceStatusService.markImportedRecordsAsInvalidIfNeeded(records, deviceStatuses)
            FileIO.appendRecords(filePath, finalRecords)
            println(s"Wind range import successful. Imported ${finalRecords.length} record(s).")
          case Left(error) =>
            println(s"Wind range import failed: $error")
        }
        Thread.sleep(3000)

      case Hydro =>
        ApiService.importHydroByRange(startDate, endDate) match {
          case Right(records) =>
            val finalRecords = DeviceStatusService.markImportedRecordsAsInvalidIfNeeded(records, deviceStatuses)
            FileIO.appendRecords(filePath, finalRecords)
            println(s"Hydro range import successful. Imported ${finalRecords.length} record(s).")
          case Left(error) =>
            println(s"Hydro range import failed: $error")
        }
        Thread.sleep(3000)
    }
  }

  def updateDeviceStatusMenu(): Unit = {
    println("\n--- Update Device Status ---")
    val energyType = readValidEnergyType()
    val deviceStatus = readValidDeviceStatus()
    println("Enter detected date:")
    val detectedDate = readValidDate()
    println("Enter detected time:")
    val detectedTime = readValidTime()
    val note = readOptionalNote()

    val statusRecord = DeviceStatusRecord(
      energyType = energyType,
      deviceStatus = deviceStatus,
      detectedDate = detectedDate,
      detectedTime = detectedTime,
      note = note
    )

    FileIO.appendDeviceStatusRecord(deviceStatusFilePath, statusRecord)
    println("Device status updated successfully.")
  }

  def formatDate(date: LocalDate): String = {
    val day = f"${date.getDayOfMonth}%02d"
    val month = f"${date.getMonthValue}%02d"
    val year = date.getYear.toString
    s"$day/$month/$year"
  }
  @tailrec
  def readValidDate(): String = {
    val input = readLine("Enter date (DD/MM/YYYY): ")
    if (Validation.isValidDate(input)) input
    else {
      println("Invalid date. Please use DD/MM/YYYY.")
      readValidDate()
    }
  }
  @tailrec
  def readValidTime(): String = {
    val input = readLine("Enter time (HH:MM): ")
    if (Validation.isValidTime(input)) input
    else {
      println("Invalid time. Please use HH:MM.")
      readValidTime()
    }
  }
  @tailrec
  def readValidHour(): String = {
    val input = readLine("Enter hour (HH): ")
    if (Validation.isValidHour(input)) input
    else {
      println("Invalid hour.")
      readValidHour()
    }
  }
  @tailrec
  def readValidMonthAndYear(): (String, String) = {
    val month = readLine("Enter month (MM): ")
    val year = readLine("Enter year (YYYY): ")

    if (!Validation.isValidMonth(month)) {
      println("Invalid month.")
      readValidMonthAndYear()
    } else if (!Validation.isValidYear(year)) {
      println("Invalid year.")
      readValidMonthAndYear()
    } else {
      (month, year)
    }
  }
  @tailrec
  def readValidEnergyType(): EnergyType = {
    val input = readLine("Enter energy type (Solar/Wind/Hydro): ")
    Parser.parseEnergyType(input) match {
      case Right(value) => value
      case Left(_) =>
        println("Invalid energy type.")
        readValidEnergyType()
    }
  }
  @tailrec
  def readValidActualGeneration(): Double = {
    val input = readLine("Enter generation value: ")
    Validation.validateGeneration(input) match {
      case Right(value) => value
      case Left(error) =>
        println(error)
        readValidActualGeneration()
    }
  }

  @tailrec
  def readValidPlantStatus(): PlantStatus = {
    val input = readLine("Enter plant status (Normal/LowOutput/MaintenanceNeeded/Malfunction/ForecastUnavailable): ")
    Parser.parsePlantStatus(input) match {
      case Right(value) => value
      case Left(_) =>
        println("Invalid plant status.")
        readValidPlantStatus()
    }
  }
  @tailrec
  def readValidDeviceStatus(): DeviceStatus = {
    val input = readLine("Enter device status (Operational/UnderMaintenance/Damaged): ")
    Parser.parseDeviceStatus(input) match {
      case Right(value) => value
      case Left(_) =>
        println("Invalid device status.")
        readValidDeviceStatus()
    }
  }

  def readOptionalCause(): Option[String] = {
    val input = readLine("Enter possible cause (leave empty if none): ").trim
    if (input.isEmpty) None else Some(input)
  }

  def readOptionalNote(): Option[String] = {
    val input = readLine("Enter note (leave empty if none): ").trim
    if (input.isEmpty) None else Some(input)
  }

  def createRecordFromInput(): EnergyRecord = {
    val date = readValidDate()
    val time = readValidTime()
    val energyType = readValidEnergyType()
    val actualGeneration = readValidActualGeneration()
    val status = readValidPlantStatus()
    val possibleCause = readOptionalCause()

    EnergyRecord(
      date = date,
      time = time,
      energyType = energyType,
      actualGeneration = actualGeneration,
      status = status,
      possibleCause = possibleCause,
      isValidForAnalysis = true
    )
  }
  @tailrec
  def menuLoop(): Unit = {
    val records = FileIO.loadValidRecords(filePath)
    val validRecords = QueryService.validForAnalysis(records)

    showMenu()
    val choice = readLine("Choose an option: ")

    choice match {
      case "1" =>
        println("\nAll records:")
        Utils.printRecords(records)
        menuLoop()

      case "2" =>
        filterMenu(records)

      case "3" =>
        sortMenu(records)

      case "4" =>
        println("\n--- Search Generation Records ---")
        println("Search supports date, time, energy type, plant status, and possible cause.")
        val showHelp = readLine("View detailed search help? (y/n): ").trim.toLowerCase

        if (showHelp == "y") {
          println("You can search by the following fields:")
          println("- Date: e.g. 07/04/2026")
          println("- Time: e.g. 12:30")
          println("- Energy type: Solar, Wind, Hydro")
          println("- Plant status: Normal, LowOutput, MaintenanceNeeded, Malfunction, ForecastUnavailable")
          println("- Possible cause text (if available)")
          println("The search is case-insensitive and matches partial text.")
        }

        val keyword = readLine("Enter keyword: ")
        println(s"\nSearch results for '$keyword':")
        Utils.printRecords(QueryService.searchByKeyword(records, keyword))
        menuLoop()

      case "5" =>
        println("\nGeneration analysis:")
        Utils.printAnalysis(AnalyticsService.analyzeGeneration(validRecords))
        menuLoop()

      case "6" =>
        println("\nGenerated alerts:")
        val alerts = AlertService.generateAllAlerts(validRecords)
        Utils.printAlerts(alerts)
        menuLoop()

      case "7" =>
        println("\nControl recommendations:")
        val alerts = AlertService.generateAllAlerts(validRecords)
        val recommendations = ControlService.generateRecommendations(alerts)
          .filter(_.action != NoActionNeeded)
        Utils.printRecommendations(recommendations)
        menuLoop()

      case "8" =>
        val record = createRecordFromInput()
        FileIO.appendRecord(filePath, record)
        println("New generation record added successfully.")
        menuLoop()

      case "9" =>
        importMenu()

      case "10" =>
        deviceMenu()

      case "11" =>
        storageMenu()

      case "12" =>
        println("\nChecking generation file parsing errors:")
        val errors = FileIO.loadErrors(filePath)
        Utils.printErrors(errors)
        menuLoop()

      case "13" =>
        println("Exiting REPS...")
    }
  }

  def main(args: Array[String]): Unit = {
    menuLoop()
  }
}